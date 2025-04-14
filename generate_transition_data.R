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
#' allowed_transitions <- tibble::tribble(
#'   ~from, ~to,
#'   1,     2,
#'   1,     4,
#'   2,     3,
#'   2,     4,
#'   3,     2,
#'   3,     4
#' )
#' transitions_all <- generate_transition_data(state_durations, allowed_transitions)
generate_transition_data <- function(state_durations, allowed_transitions) {
 
  # Map states to numbers
  state_levels <- unique(state_durations$state)
  state_map <- tibble::tibble(state = state_levels, state_id = seq_along(state_levels))
  
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
