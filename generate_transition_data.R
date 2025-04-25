#' Generate transition data for multi-state modeling
#'
#' This function prepares patient-level transition data for multi-state survival models,
#' computing observed and possible transitions, and aggregating time spent in each state.
#' It also returns the empirical frequency of observed transitions.
#'
#' @param state_durations A data frame with one row per patient per state, including:
#'   - `patientid`: unique patient ID
#'   - `state`: character name of the state
#'   - `start_time`: time of entry into the state (numeric)
#'   - `end_time`: time of exit from the state (numeric)
#'   - `event`: 1 if transition was observed (not censored), 0 otherwise
#'
#' @return A list containing:
#'   - `transitions_combined`: aggregated durations per patient per state
#'   - `edf`: empirical transition frequencies (from → to)
#'
#' @examples
#' transitions <- generate_transition_data(state_durations)
#'
generate_transition_data <- function(state_durations) {
 
  # Map states to numbers
  state_levels <- unique(state_durations$state)
  state_map <- tibble::tibble(state = state_levels, state_id = seq_along(state_levels))
  
  allowed_transitions <- tibble::tribble(
       ~from, ~to,
       1,     2,
       1,     4,
       2,     3,
       2,     4,
      3,     2,
       3,     4)
  
  # Observed transitions
  obs_transitions <- state_durations %>%
    dplyr::left_join(state_map, by = "state") %>%
    dplyr::group_by(patientid) %>%
    dplyr::arrange(start_time, .by_group = TRUE) %>%
    dplyr::mutate(
      from = lag(state_id),
      to = state_id,
      Tstart = lag(start_time),
      Tstop = start_time
    ) %>%
    dplyr::filter(!is.na(from)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      status = event
    )
  
  # Add single censored transitions to observed transitions
  single_transitions <- state_durations %>%
    dplyr::left_join(state_map, by = "state") %>%
    dplyr::group_by(patientid) %>%
    dplyr::filter(n() == 1) %>%
    dplyr::mutate(
      from = state_id,
      to = state_id,
      Tstart = start_time,
      Tstop = end_time
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      status = 0,
      event = 0
    )
  
  # Bind to observed transitions
  obs_transitions <- obs_transitions %>%
    dplyr::bind_rows(single_transitions)
  
  # All possible transitions for observed intervals
  expand_possible <- obs_transitions %>%
    dplyr::select(patientid, from, Tstart, Tstop) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(allowed_transitions, by = "from", relationship = "many-to-many")
  
  # Merge observed transitions with all possible ones
  transitions_all <- expand_possible %>%
    dplyr::left_join(obs_transitions %>% 
                dplyr::select(patientid, from, to, Tstart, Tstop, status_obs = status),
              by = c("patientid", "from", "to", "Tstart", "Tstop")
    ) %>%
    dplyr::mutate(
      status = ifelse(is.na(status_obs), 0, status_obs)
    ) %>%
    dplyr::select(patientid, from, to, Tstart, Tstop, status) %>%
    dplyr::filter(Tstop > Tstart) %>%  # remove negative durations
    mutate(
      trans = match(paste(from, to), paste(allowed_transitions$from, allowed_transitions$to))
    )
  
  # Combine on and off-treatment transitions 
  transitions_combined <- transitions_all %>%
    dplyr::left_join(state_map, by = c("from" = "state_id")) %>%
    dplyr::rename(from_state = state) %>%
    dplyr::left_join(state_map, by = c("to" = "state_id")) %>%
    dplyr::rename(to_state = state) %>%
    dplyr::select(-from, -to, -to_state) %>% 
    dplyr::group_by(patientid, from_state) %>%
    dplyr::summarise(
      Tstart = min(Tstart),
      Tstop = max(Tstop),
      status = as.integer(any(status == 1)),
      .groups = "drop"
    )
    
  
  # Create empirical distribution (frequency)
  transitions_frequency <- transitions_all %>%
    dplyr::filter(status == 1) %>%
    dplyr::count(from, to) %>%
    dplyr::left_join(state_map, by = c("from" = "state_id")) %>%
    dplyr::rename(from_state = state) %>%
    dplyr::left_join(state_map, by = c("to" = "state_id")) %>%
    dplyr::rename(to_state = state) %>%
    dplyr::select(from_state, to_state, n) %>%
    dplyr::group_by(from_state) %>%
    dplyr::mutate(prob = n / sum(n)) %>%
    dplyr::ungroup()
    

  
  return(list(transitions_combined = transitions_combined, edf = transitions_frequency))
}



#' Fit Weibull Survival Models for Each State for sojourn survival time 
#'
#' This function fits separate Weibull models for each unique state in a state duration dataset.
#' It estimates the shape and scale parameters along with their 95% confidence intervals
#' using the `flexsurvreg` function from the `flexsurv` package.
#'
#' @param transitions_combined A data frame with columns: `from_state`, `Tstart`, `Tstop`, `status`.
#'        Each row represents time spent in a state for a given patient, where:
#'        - `Tstart` is the entry time into the state (usually 0)
#'        - `Tstop` is the exit time
#'        - `status` is 1 if the patient exited the state for any event, 0 if censored (no exit from the state)
#'
#' @return A tibble with columns:
#'   - `state`: state name
#'   - `shape`, `scale`: Weibull parameter estimates
#'   - `shape_lci`, `shape_uci`: 95% CI for shape
#'   - `scale_lci`, `scale_uci`: 95% CI for scale
#'
#' @examples
#' fit_weibull_by_state(transitions_all)
#'
#' @importFrom dplyr filter bind_rows
#' @importFrom flexsurv flexsurvreg
#' @importFrom tibble tibble
#' @export
fit_weibull_by_state <- function(transitions_combined) {
  unique_transitions <- unique(transitions_combined$from_state)
  
  param_results <- lapply(unique_transitions, function(t) {
    df_state <- transitions_combined %>% dplyr::filter(from_state == t)
    n_events <- sum(df_state$status == 1, na.rm = TRUE)
    
    # Handle no events
    if (n_events == 0) {
      message(glue::glue("No observed events for transition {t} Returning NA"))
      return(tibble::tibble(
        state_transition = t,
        shape = NA_real_, shape_lci = NA_real_, shape_uci = NA_real_,
        scale = NA_real_, scale_lci = NA_real_, scale_uci = NA_real_
      ))
    }
    # Fit Weibull model
    fit <- tryCatch({
      flexsurv::flexsurvreg(
        Surv(Tstart, Tstop, status) ~ 1,
        data = df_state,
        dist = "weibull")
    }, 
    error = function(e) NULL)
    
    # If failed, try fallback inits
    if (is.null(fit)) {
      median_surv <- median(df_state$Tstop - df_state$Tstart, na.rm = TRUE)
      message(glue::glue("Default fit optimizer failed for {t} Retrying with inits: shape = 1, scale = {median_surv}"))
      
      fit <- tryCatch({
        flexsurv::flexsurvreg(
          Surv(Tstart, Tstop, status) ~ 1,
          data = df_state,
          dist = "weibull",
          inits = c(shape = 1, scale = median_surv)
        )
      }, error = function(e) NULL)
    }
    
    if (!is.null(fit)) {
      tibble::tibble(
        state_transition = t,
        shape = fit$res["shape", "est"],
        shape_lci = fit$res["shape", "L95%"],
        shape_uci = fit$res["shape", "U95%"],
        scale = fit$res["scale", "est"],
        scale_lci = fit$res["scale", "L95%"],
        scale_uci = fit$res["scale", "U95%"]
      )
    } else {
      message(glue::glue("Weibull model failed for {t} Returning NA."))
      tibble::tibble(
        state_transition = t,
        shape = NA_real_, shape_lci = NA_real_, shape_uci = NA_real_,
        scale = NA_real_, scale_lci = NA_real_, scale_uci = NA_real_
      )
    }
  })
  
  params_results <- dplyr::bind_rows(param_results)
 params_results <- params_results %>%
    mutate(on_treatment_flag = str_detect(state_transition, "^On_Treatment")) %>%
    arrange(desc(on_treatment_flag), state_transition) %>%
    select(-on_treatment_flag)
  return(params_results)
}

