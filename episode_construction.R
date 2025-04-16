#' Derive last follow-up and proxy death dates for patients
#'
#' This function merges last contact and mortality data, and computes:
#' - A `Date_LastFollowUp` as the max of last contact, last note, and treatment end
#' - A `death_date` using:
#'     - If the proxy date (max of contact/note/treatment) falls in the same month as reported month-of-death, use that
#'     - Otherwise, use `parsemonthofdeath` directly
#'
#' @param last_contact_data A data frame with patientid, Date_LastContact, Date_LastClinicalNote, Date_TxEnd_PostReport
#' @param mort_dat A data frame with patientid and parsemonthofdeath (as character or date)
#'
#' @return A data frame with patientid, Date_LastFollowUp, and derived death_date
#'
#' @examples
#' derive_death_date(last_contact_data, mort_dat)
#'
derive_death_date <- function(last_contact_data, mort_dat) {
 
  
  last_contact_data <- last_contact_data %>%
    dplyr::left_join(mort_dat, by = "patientid") %>%
    dplyr::select(patientid, Date_LastContact, Date_LastClinicalNote, Date_TxEnd_PostReport, parsemonthofdeath) %>%
    dplyr::mutate(parsemonthofdeath = lubridate::ymd(parsemonthofdeath)) %>%
    dplyr::mutate(Date_LastFollowUp = pmax(Date_LastContact, Date_LastClinicalNote, Date_TxEnd_PostReport, na.rm = TRUE)) %>%
    dplyr::mutate(
      coalesced_death_proxy = pmax(
        Date_LastContact,
        Date_LastClinicalNote,
        Date_TxEnd_PostReport,
        na.rm = TRUE
      )) %>% 
    dplyr::mutate(
      death_date = case_when(
        !is.na(coalesced_death_proxy) & !is.na(parsemonthofdeath) &
          format(coalesced_death_proxy, "%Y-%m") == format(parsemonthofdeath, "%Y-%m") ~ coalesced_death_proxy,
        !is.na(parsemonthofdeath) ~ parsemonthofdeath,
        TRUE ~ as.Date(NA)
      )
    ) %>%
    dplyr::select(patientid, Date_LastFollowUp, death_date)
    return(last_contact_data)
}


#' Collapse overlapping treatment lines for each patient
#'
#' Identifies and merges treatment lines that overlap in time for the same patient.
#' Overlapping lines are grouped and merged into one with:
#' - Earliest `linestartdate`
#' - Latest `lineenddate`
#' - Combined `linename` (de-duplicated and cleaned)
#'
#' @param lung_ep A data frame with columns `patientid`, `linestartdate`, `lineenddate`, and `linename`
#'
#' @return A cleaned data frame with one row per merged line and columns:
#'   `patientid`, `linestartdate`, `lineenddate`, `linename`, `linenumber`
#'
#' @importFrom dplyr arrange group_by mutate ungroup summarise row_number
#' @importFrom stringr str_replace_all str_squish
#'
collapse_overlapping_lines <- function(lung_ep) {
  lung_ep_overlap_flagged <- lung_ep %>%
    dplyr::arrange(patientid, linestartdate, linenumber) %>%
    dplyr::group_by(patientid) %>%
    dplyr::mutate(
      prev_end = dplyr::lag(lineenddate),
      overlaps_previous = dplyr::if_else(!is.na(prev_end) & linestartdate < prev_end, TRUE, FALSE)
    ) %>%
    dplyr::ungroup()
  
  lung_ep_grouped <- lung_ep_overlap_flagged %>%
    dplyr::arrange(patientid, linestartdate) %>%
    dplyr::group_by(patientid) %>%
    dplyr::mutate(
      group_id = cumsum(!overlaps_previous | is.na(overlaps_previous))
    ) %>%
    dplyr::ungroup()
  
  lung_ep_cleaned <- lung_ep_grouped %>%
    dplyr::group_by(patientid, group_id) %>%
    dplyr::summarise(
      linestartdate = min(linestartdate),
      lineenddate   = max(lineenddate),
      linename      = paste(sort(unique(linename)), collapse = " + "),
      .groups = "drop"
    ) %>%
    dplyr::arrange(patientid, linestartdate) %>%
    dplyr::group_by(patientid) %>%
    dplyr::mutate(linenumber = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  # Clean up repeated drug names and standardize formatting
  lung_ep_cleaned <- lung_ep_cleaned %>%
    dplyr::mutate(
      linename = stringr::str_replace_all(linename, "\\b(\\w+)( \\+ \\1)+\\b", "\\1"),
      linename = stringr::str_squish(gsub("\\s*\\+\\s*", " + ", linename))
    )
  
  return(lung_ep_cleaned)
}



#' Clean and collapse single-dose lung cancer treatment lines
#'
#' This function identifies and cleans single-day treatment lines in a longitudinal
#' drug exposure dataset. It attempts to merge single-day lines with adjacent lines
#' if they share overlapping drugs, and drops lines that are isolated and non-mergeable.
#'
#' It supports threshold-based partial overlap merging and reconstructs treatment
#' episodes by recomputing line numbers after merging or dropping.
#'
#' @param lung_ep A data frame containing line-level treatment data. Must include
#'   columns: `patientid`, `linestartdate`, `lineenddate`, `linenumber`, `linename`.
#' @param drug_separator A character string used to split multiple drugs in `linename`.
#'   Default is ",".
#' @param overlap_threshold An integer indicating the minimum number of overlapping
#'   drugs between line names required to consider lines similar enough to merge.
#'   Default is 1.
#'
#' @return A cleaned `data.frame` with the same columns (`patientid`, `linestartdate`,
#'   `lineenddate`, `linename`, `linenumber`), but with collapsed or dropped single-day lines.
#'
#' @examples
#' cleaned_lines <- single_dose_clean(lung_ep)
#'
#' @import dplyr
#' @importFrom stringr str_squish
#' @export
single_dose_clean <- function(lung_ep, drug_separator = ",", overlap_threshold = 1) {
  
  # Helper: split drugs into a sorted set
  split_drugs <- function(name) sort(trimws(unlist(strsplit(name, drug_separator))))
  
  # Helper: check if two line names have overlapping drugs (at least `threshold`)
  has_overlap <- function(name1, name2, threshold = overlap_threshold) {
    if (is.na(name1) | is.na(name2)) return(FALSE)
    length(intersect(split_drugs(name1), split_drugs(name2))) >= threshold
  }
  
  # Flag single-day lines and gather adjacent line info
  lung_ep_flagged <- lung_ep %>%
    dplyr::arrange(patientid, linestartdate, linenumber) %>%
    dplyr::group_by(patientid) %>%
    dplyr::mutate(
      is_single_day = as.numeric(lineenddate - linestartdate) == 0,
      prev_end      = dplyr::lag(lineenddate),
      prev_name     = dplyr::lag(linename),
      prev_gap      = as.numeric(linestartdate - prev_end),
      next_start    = dplyr::lead(linestartdate),
      next_name     = dplyr::lead(linename),
      next_gap      = as.numeric(next_start - lineenddate)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      merge_with_prev = mapply(function(x, y, gap, single) {
        single & !is.na(gap) & has_overlap(x, y)
      }, linename, prev_name, prev_gap, is_single_day),
      
      merge_with_next = mapply(function(x, y, gap, single) {
        single & !is.na(gap) & has_overlap(x, y)
      }, linename, next_name, next_gap, is_single_day),
      
      drop_line = is_single_day & !merge_with_prev & !merge_with_next
    )
  
  # Identify which lines will be merged and to whom
  to_merge <- dplyr::bind_rows(
    lung_ep_flagged %>% dplyr::filter(merge_with_prev) %>% dplyr::mutate(target_line = linenumber - 1),
    lung_ep_flagged %>% dplyr::filter(merge_with_next) %>% dplyr::mutate(target_line = linenumber + 1)
  )
  
  # Prepare target lines to merge with
  target_lines <- lung_ep_flagged %>%
    dplyr::select(patientid, linenumber, linestartdate, lineenddate, linename) %>%
    dplyr::rename(
      target_line   = linenumber,
      target_start  = linestartdate,
      target_end    = lineenddate,
      target_name   = linename
    )
  
  to_merge_full <- dplyr::left_join(to_merge, target_lines, by = c("patientid", "target_line"))
  
  # Collapse line info for merged lines
  collapsed_lines <- to_merge_full %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      linestartdate = min(linestartdate, target_start, na.rm = TRUE),
      lineenddate   = max(lineenddate, target_end, na.rm = TRUE),
      linename = paste(sort(unique(c(
        split_drugs(linename),
        split_drugs(target_name)
      ))), collapse = " + ")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(patientid, linestartdate, lineenddate, linename) %>%
    dplyr::distinct()
  
  # Remove merged or dropped lines from original
  lines_to_remove <- dplyr::bind_rows(
    to_merge %>% dplyr::select(patientid, linenumber),
    to_merge %>% dplyr::select(patientid, linenumber = target_line),
    lung_ep_flagged %>% dplyr::filter(drop_line) %>% dplyr::select(patientid, linenumber)
  )
  
  lung_ep_retained <- lung_ep_flagged %>%
    dplyr::anti_join(lines_to_remove, by = c("patientid", "linenumber")) %>%
    dplyr::select(patientid, linestartdate, lineenddate, linename)
  
  # Combine retained + collapsed lines and renumber
  lung_ep_final <- dplyr::bind_rows(lung_ep_retained, collapsed_lines) %>%
    dplyr::arrange(patientid, linestartdate) %>%
    dplyr::group_by(patientid) %>%
    dplyr::mutate(linenumber = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  return(lung_ep_final)
}




#' Construct drug episode state transitions for microsimulation
#'
#' This function cleans, merges, and filters drug line data for patients receiving systemic therapy.
#' It identifies valid treatment lines based on drug content, collapses overlapping lines and
#' single-dose episodes, and constructs a time-to-event dataset with treatment states, off-treatment
#' periods, and death. The result is formatted for input to TreeAge or microsimulation models.
#'
#' @param drug_episode_data A data frame with episode-level drug info, including:
#'   `patientid`, `linename`, `linestartdate`, `lineenddate`, `episodedate`, `linenumber`.
#' @param last_contact_data A data frame with `patientid`, `Date_LastFollowUp`, and `death_date`.
#' @param drug_separator A character string separating multiple drugs in `linename`. Default is ",".
#' @param overlap_threshold Integer: number of shared drugs required to collapse lines. Default = 1.
#' @param target_line A lowercase drug name to filter for (e.g., "cisplatin"). Default is "cisplatin".
#' @param line_placement Which line number to target for filtering (e.g., 2 = second-line). Default = 2.
#' @param off_treatment_threshold Days between lines to define off-treatment periods. Default = 7.
#' @param ... Additional arguments (unused, reserved for future use).
#'
#' @return A data frame with columns:
#'   `patientid`, `state`, `start_time`, `end_time`, `duration`, `event`
#'
#' @importFrom dplyr arrange group_by mutate ungroup filter select summarise transmute bind_rows row_number distinct left_join anti_join
#' @importFrom stringr str_detect
#' @importFrom lubridate ymd
#'
#' @export
construct_drug_episodes <- function(drug_episode_data, last_contact_data, 
                                    drug_separator = ",", 
                                    overlap_threshold = 1, 
                                    target_line = "cisplatin", 
                                    line_placement = 2, 
                                    off_treatment_threshold = 7, ...) {
  
  drug_episodes <- drug_episode_data %>%
    dplyr::select(patientid, linenumber, linename, linesetting, 
                  linestartdate, lineenddate, episodedate, detaileddrugcategory) %>%
    dplyr::filter(linesetting == "ADVANCED") %>%
    tidyr::drop_na(linenumber) %>%
    dplyr::group_by(patientid, linenumber) %>%
    dplyr::summarise(
      linename = dplyr::first(linename),
      linestartdate = min(linestartdate),
      lineenddate = max(episodedate),
      .groups = 'drop'
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(patientid, linenumber) %>%
    dplyr::mutate(
      linestartdate = lubridate::ymd(linestartdate),
      lineenddate   = lubridate::ymd(lineenddate)
    )
  
  # Apply overlapping and single-dose cleaning functions
  drug_episodes <- collapse_overlapping_lines(drug_episodes)
  drug_episodes <- single_dose_clean(drug_episodes, drug_separator = drug_separator, overlap_threshold = overlap_threshold)
  
  # Filter to patients with target drug on the target line
  ep_patients <- drug_episodes %>%
    dplyr::filter(stringr::str_detect(tolower(linename), target_line), linenumber == line_placement) %>%
    dplyr::pull(patientid) %>%
    unique()
  
  # We are only interested in patients with target line number and the line after (all possible transitions from target line)
  drug_episodes <- drug_episodes %>%
    dplyr::filter(patientid %in% ep_patients) %>%
    dplyr::filter(linenumber == line_placement | linenumber == line_placement + 1)
  
  # Join last contact data
  drug_episodes <- drug_episodes %>%
    dplyr::left_join(last_contact_data, by = "patientid")
 
  #  Prepare for state transitions
  lines <- drug_episodes %>%
    dplyr::group_by(patientid) %>%
    dplyr::mutate(
      next_start = dplyr::lead(linestartdate),
      gap_days = as.numeric(next_start - lineenddate),
      has_gap = gap_days >= off_treatment_threshold,
      adjust_line_start = gap_days < off_treatment_threshold & !is.na(gap_days),
      linestartdate = dplyr::if_else(
        dplyr::lag(adjust_line_start, default = FALSE),
        dplyr::lag(lineenddate) + 1,
        linestartdate
      ),
      base_time = dplyr::first(linestartdate)
    ) %>%
    dplyr::ungroup()
  
  # On-treatment states
  on_treatment <- lines %>%
    dplyr::mutate(
      state = paste0("On_Treatment_Line", linenumber)
    ) %>%
    dplyr::filter(!is.na(state)) %>%
    dplyr::transmute(
      patientid,
      state,
      start_date = linestartdate,
      end_date = lineenddate,
      linenumber, 
      base_time, 
      death_date, 
      event = 1
    )
  
  # Off-treatment periods between lines only for the target line placement of interest (for patients who have a line next)
  off_treatment <- lines %>%
    tidyr::drop_na(has_gap) %>%
    dplyr::filter(has_gap, linenumber == line_placement) %>%
    dplyr::transmute(
      patientid,
      state = paste0("Off_Treatment", line_placement),
      start_date = lineenddate + 1,
      end_date = next_start-1,
      base_time, 
      linenumber, 
      death_date,
      event = 1
    )
  
  # Off-treatment (for patients who transition to death from target line)
  final <- lines %>%
    dplyr::group_by(patientid) %>%
    dplyr::filter(max(linenumber) == line_placement) %>%
    dplyr::slice_max(linestartdate, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      final_end = dplyr::coalesce(death_date, Date_LastFollowUp),
      died = !is.na(death_date),
      state = dplyr::case_when(
        died ~ paste0("Off_Treatment", line_placement),
        lineenddate < final_end ~ paste0("Off_Treatment", line_placement),
        TRUE ~ paste0("On_Treatment_Line", linenumber)
      ),
      event = as.integer(died)
    ) %>%
    dplyr::transmute(
      patientid,
      state,
      start_date = lineenddate,
      end_date = final_end,
      base_time,
      event,
      death_date
    ) %>%
    dplyr::filter(!is.na(end_date) & end_date > start_date)
  
  # Add absorbing state for death
  death_state <- final %>%
    dplyr::filter(state == paste0("Off_Treatment", line_placement), !is.na(end_date), end_date == death_date) %>%
    dplyr::transmute(
      patientid,
      state = "Death",
      start_date = end_date,
      end_date = end_date,
      base_time,
      event = 1
    )
  
  
  # Combine states and compute time variables
  state_durations <- dplyr::bind_rows(
    on_treatment, 
    off_treatment,
    final,
    death_state
  ) %>%
    dplyr::mutate(
      start_time = as.numeric(start_date - base_time),
      end_time = as.numeric(end_date - base_time),
      duration = end_time - start_time
    ) %>%
    dplyr::select(patientid, state, start_time, end_time, duration, event) %>%
    dplyr::arrange(patientid, start_time)
  
  
  
  return(state_durations)
}

