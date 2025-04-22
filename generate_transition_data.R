#' Generate transition dataset for multi-state modeling
#'
#' This function prepares a dataset of observed and possible transitions
#' between states for use in flexible multi-state survival models like
#' those fit with `flexsurvreg`. It computes transition intervals
#' (Tstart, Tstop), assigns transition IDs, and fills in censored transitions.
#'
#' @param state_durations A data frame with one row per patient per state, including:
#'   - `patientid`: patient identifier
#'   - `state`: state label
#'   - `start_time`: numeric time the state begins
#'   - `end_time`: numeric time the state ends
#'   - `event`: 1 if transition was observed, 0 if censored
#'
#' @param allowed_transitions A data frame (or tibble) with columns:
#'   - `from`: numeric state ID
#'   - `to`: numeric state ID
#' which details all possible transitions 
#' 
#' @return A tibble with one row per possible transition interval per patient:
#'   - `patientid`, `from`, `to`, `Tstart`, `Tstop`, `status`, `trans`
#'
#' @examples
#' 
#' transitions_all <- generate_transition_data(state_durations, allowed_transitions)
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
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(from)) %>%
    dplyr::mutate(status = event)
  
  # All possible transitions for observed intervals
  expand_possible <- obs_transitions %>%
    dplyr::select(patientid, from, Tstart, Tstop) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(allowed_transitions, by = "from")
  
  # Step 4: Merge observed transitions with all possible ones
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
  
  return(transitions_all)
}



#' Fit Weibull Survival Models for Each State for sojourn survival time 
#'
#' This function fits separate Weibull models for each unique state in a state duration dataset.
#' It estimates the shape and scale parameters along with their 95% confidence intervals
#' using the `flexsurvreg` function from the `flexsurv` package.
#'
#' @param transitions_all A data frame with columns: `state`, `T_start`, `T_stop`, `status`.
#'        Each row represents time spent in a state for a given patient, where:
#'        - `T_start` is the entry time into the state (usually 0)
#'        - `T_stop` is the exit time
#'        - `status` is 1 if the patient exited (event), 0 if censored
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
fit_weibull_by_state <- function(transitions_all) {
  unique_transitions <- unique(transitions_all$trans)
  
  param_results <- lapply(unique_transitions, function(t) {
    df_state <- transitions_all %>% dplyr::filter(trans == t)
    
    # Fit Weibull model
    fit <- tryCatch({
      flexsurv::flexsurvreg(
        Surv(Tstart, Tstop, status) ~ 1,
        data = df_state,
        dist = "weibull"
      )
    }, error = function(e) NULL)
    
    if (!is.null(fit)) {
      tibble::tibble(
        state = t,
        shape = fit$res["shape", "est"],
        shape_lci = fit$res["shape", "L95%"],
        shape_uci = fit$res["shape", "U95%"],
        scale = fit$res["scale", "est"],
        scale_lci = fit$res["scale", "L95%"],
        scale_uci = fit$res["scale", "U95%"]
      )
    } else {
      tibble::tibble(
        state_transition = t,
        shape = NA_real_,
        shape_lci = NA_real_,
        shape_uci = NA_real_,
        scale = NA_real_,
        scale_lci = NA_real_,
        scale_uci = NA_real_
      )
    }
  })
  
  dplyr::bind_rows(param_results)
}
