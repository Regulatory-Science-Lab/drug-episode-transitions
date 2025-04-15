#Libraries
library('RODBC')
library(dplyr)
library(tidyverse)
library(lubridate)

#Connect to SQL
conn <- odbcConnect("predict")
sqlTables(conn)

bcc_dx <- sqlQuery(conn, "SELECT *
                           FROM analysis.bcc_dx;")
bcc_pharmacy <- sqlQuery(conn, "SELECT *
                           FROM analysis.bcc_pharmacy;")

#Build episode

build_episodes <- function(tumor_indication, target_drug, target_line, combination,
                            dx_data = bcc_dx, pharmacy_data = bcc_pharmacy) {
  
  stages = c('3', '3A', '3B', '3C', '4', '4A', '4B')
  j = 30
  k = 150
  high_line_limit <- 7
  
  #Find patients
  dx <- dx_data %>%
    filter(incid_simple_grp == tumor_indication) %>%
    filter(final_stage_grouping_code %in% stages)
  
  if (tumor_indication == "Head and Neck") {
    dx <- dx %>%
      filter(incid_minor_regrpt_grp == "Salivary Gland")
  }
  
  #Merge
  dx_id <- dx[, c('predict_id', 'incid_simple_grp')]
  dx_pharmacy <- merge(dx_id, pharmacy_data, by = "predict_id")
  
  #Clean
  dx_pharmacy <- dx_pharmacy %>%
    select(predict_id, incid_simple_grp, prescription_date, protocol_code, drug_name) %>%
    mutate(drug_name = str_trim(drug_name)) %>%
    mutate(drug_name = gsub("\\s*\\(.*?\\)\\s*", "", drug_name)) %>%
    mutate(protocol_code = str_remove(protocol_code, "^\\?")) %>%
    mutate(prescription_date = as.Date(prescription_date)) %>%
    filter(!grepl("VOID", drug_name, ignore.case = TRUE)) %>%
    filter(!grepl("^_", protocol_code)) %>%
    group_by(drug_name) %>% filter(n() >= 10 | drug_name %in% target_drug) %>% ungroup() %>%
    mutate(protocol_code = gsub("^U[-]?", "", toupper(protocol_code))) %>%
    filter(!is.na(protocol_code)) %>%
    distinct(predict_id, incid_simple_grp, prescription_date, protocol_code, drug_name, .keep_all = TRUE) %>%
    arrange(predict_id, prescription_date, protocol_code, drug_name) %>%

  # Cyclic algorithm
    
    group_by(predict_id) %>%
    
    # Naive approach
    # Calculate number of protocols & max_protocol for each predict_id
    
    mutate(protocol_n = cumsum(!is.na(protocol_code) & (protocol_code != lag(protocol_code, default = first(protocol_code)))) + 1) %>%
    mutate(max_protocol = if (all(is.na(protocol_n))) NA_integer_ else max(protocol_n, na.rm = TRUE)) %>%
    
    # Validated approach
    # Create days_between variable
    
    mutate(days_between = prescription_date - lag(prescription_date)) %>%
    
    # best algorithm: Past i=2 drugs, j=30 days_between
    
    mutate(flag_30 = 1) %>%
    mutate(flag_30 = ifelse(drug_name == lag(drug_name, 1) | drug_name == lag(drug_name, 2), 0, flag_30)) %>%
    mutate(flag_30 = ifelse(flag_30 == 1 & (protocol_code == lag(protocol_code, 1) | 
                                              protocol_code == lag(protocol_code, 2)), 0, flag_30)) %>%
    mutate(flag_30 = case_when(
      is.na(days_between) ~ 1,
      lag(is.na(days_between)) ~ 0,
      flag_30 == 0 & days_between <= j ~ 0,
      flag_30 == 0 & days_between > j ~ 1,
      flag_30 == 1 & days_between <= j ~ 1,
      flag_30 == 1 & days_between > j ~ 1,
      TRUE ~ 1)) %>%
    
    # Total number of lines & fill in line number
    
    mutate(total_nlines = sum(flag_30, na.rm = TRUE)) %>%
    mutate(line_number = cumsum(!is.na(flag_30) & flag_30 == 1)) %>%
    
    # Fix patients with high lines ###WIP###
    
    mutate(
      flag_30 = if_else(
        total_nlines > high_line_limit,
        case_when(
          lag(is.na(days_between)) ~ 0,
          is.na(days_between) ~ 1,
          protocol_code == lag(protocol_code, 1) |
            protocol_code == lag(protocol_code, 2) |
            protocol_code == lag(protocol_code, 3) |
            protocol_code == lag(protocol_code, 4) |
            protocol_code == lag(protocol_code, 5) ~ 0,
          flag_30 == 0 & days_between <= k ~ 0,
          flag_30 == 0 & days_between > k ~ 1,
          flag_30 == 1 & days_between <= k ~ 1,
          flag_30 == 1 & days_between > k ~ 1,
          TRUE ~ 1), 
        flag_30)) %>%
    
    # Update
    
    mutate(total_nlines = sum(flag_30, na.rm = TRUE)) %>%
    mutate(line_number = cumsum(!is.na(flag_30) & flag_30 == 1)) %>%
    ungroup() %>%
    select(-c(protocol_n,max_protocol,days_between, flag_30, total_nlines))
  
  # Generate episodes
  
  # Address if target_drug is a combination therapy

  if (combination == TRUE) {
    target_pop_id <- dx_pharmacy %>%
      filter(line_number == target_lsine) %>%
      group_by(predict_id) %>%
      summarise(drugs_received = list(unique(str_trim(drug_name)))) %>%
      filter(sapply(drugs_received, function(drugs) all(target_drug %in% drugs))) %>%
      pull(predict_id)
  } else {
    target_pop_id <- dx_pharmacy %>%
      filter(str_trim(drug_name) %in% target_drug) %>%
      filter(line_number == target_line) %>%
      pull(predict_id)
  }
  
  target_pop <- dx_pharmacy %>%
    filter(predict_id %in% target_pop_id)
  
  episode_df <- data.frame()
  episode_df <- cbind(unique(target_pop$predict_id))
  
  # Add on treatment periods
  
  episode_df <- target_pop %>%
    group_by(predict_id, line_number) %>%
    reframe(
      drug_name = paste(unique(drug_name), collapse=" + "),
      protocol = paste(unique(protocol_code), collapse= " + "),
      start = as.Date(min(prescription_date)),
      end = as.Date(max(prescription_date))) %>%
    ungroup() %>%
    group_by(predict_id) %>%
    mutate(duration = end-start) %>%
    mutate(duration = ifelse(duration == 0, 1, duration)) %>%
    mutate(state = paste0("On Treatment", sep = "_", row_number())) %>%
    ungroup()
  
  episode_df <- episode_df[, c(1,3,4,2,5,6,7,8)]
  
  # Add off treatment periods
  
  episode_df <- episode_df %>%
    group_by(predict_id) %>%
    arrange(start) %>%
    mutate(row_number = row_number(),
           max_row = max(row_number),
           next_start = lead(start)) %>%
    ungroup()
  
  episode_df <- episode_df %>%
    bind_rows(
      episode_df %>%
        group_by(predict_id) %>%
        filter(row_number < max_row) %>%
        mutate(
          drug_name = NA_character_,
          protocol = NA_character_,
          line_number = NA,
          state = "Off Treatment",
          duration = NA,
          start = end + 1,
          end = next_start - 1) %>%
        ungroup()) %>%
    arrange(predict_id, start) %>%
    select(-c(row_number, max_row, next_start)) %>%
    mutate(duration = ifelse(is.na(duration), end-start, duration))
  
  # Add death events
  
  episode_df_id <- episode_df %>%
    select(predict_id) %>%
    distinct()
  
  episode_df_death <- dx_data %>%
    semi_join(episode_df_id, by = "predict_id") %>%
    select(predict_id, death_date) %>%
    group_by(predict_id) %>%
    slice(1) %>%
    ungroup()
  
  death_events <- episode_df_death %>%
    filter(!is.na(death_date)) %>%
    select(predict_id, death_date)
  
  episode_df <- bind_rows(episode_df, death_events)
  
  episode_df <- episode_df %>%
    arrange(predict_id) %>%
    mutate(state = ifelse(is.na(state), "Death", state),
           start = if_else(is.na(start), death_date, start),
           end = if_else(is.na(end), death_date, end)) %>%
    select(-c(death_date))
  
  # Add off treatment between last on treatment and death
  
  episode_df <- episode_df %>%
    arrange(predict_id, start) %>%
    group_by(predict_id) %>%
    mutate(row_num = row_number()) %>%
    ungroup()
  
  death_rows <- episode_df %>%
    filter(state == "Death")
  
  episode_df <- episode_df %>%
    arrange(predict_id, start) %>%
    group_by(predict_id) %>%
    mutate(
      next_state = lead(state),
      next_start = lead(start),
      next_row_is_death = lead(state) == "Death") %>%
    ungroup()
  
  off_before_death <- episode_df %>%
    filter(next_row_is_death) %>%
    mutate(
      state = "Off Treatment",
      start = end + 1,
      end = next_start - 1,
      duration = as.numeric(end - start),
      drug_name = NA,
      protocol = NA,
      line_number = NA)
  
  episode_df <- episode_df %>%
    bind_rows(off_before_death) %>%
    arrange(predict_id, start) %>%
    select(-row_num, -next_state, -next_start, -next_row_is_death)
  
  # Remove irrelevant off treatment transitions
  
  gap_threshold <- 2
  
  episode_df <- episode_df %>%
    filter(!(state == "Off Treatment" & duration < gap_threshold))
  
  # Formatting
  
  episode_df$start <- as.numeric(episode_df$start)
  episode_df$end <- as.numeric(episode_df$end)
  
  episode_df <- episode_df %>%
    group_by(predict_id) %>%
    mutate(
      end = (end - min(start, na.rm = TRUE)),
      start = (start - min(start, na.rm = TRUE)),
      event = 1) %>%
    ungroup() %>%
    rename(patientid = predict_id,
           start_time = start,
           end_time = end) %>%
    select(-drug_name, -protocol, -line_number, -duration)
  
  episode_df_final <- episode_df[, c(1,4,2,3,5)]
  
  return(episode_df_final)
}

folfiri <- c("LEUCOVORIN","IRINOTECAN", "FLUOROURACIL")
gemdoce <- c("GEMCITABINE","DOCETAXEL")
platinum <-  c("CISPLATIN","CARBOPLATIN","OXALIPLATIN")

cisplatin_ep <- build_episodes("Lung", "CISPLATIN", 2, FALSE)
folfiri_ep <- build_episodes("Colorectal", folfiri, 2, TRUE)
capecitabine_ep <- build_episodes("Breast", "CAPECITABINE", 2, FALSE)
gemcitabine_ep <- build_episodes("Pancreas", "GEMCITABINE", 2, FALSE)
cabozantinib_ep <- build_episodes("Thyroid", "CABOZANTINIB", 2, FALSE)
gemdoce_ep <- build_episodes("Soft Tissue (including Heart)", gemdoce, 2, TRUE)
platinum_ep <- build_episodes("Head and Neck", platinum, 1, FALSE)
