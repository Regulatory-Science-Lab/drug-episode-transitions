# Libraries \
library(haven)
library(tidyverse)
library(stringr)
library(glue)
library(flexsurv)
# Reading in drug episodes data 
drug_episodes_flatiron <- read_dta("H:\\PREDiCText\\ek\\Flatiron_data_prep\\0_DrugEpisode_allTumourSpecific_round1and4_simple_byline_progression.dta")  

# Explore progression data
##################### Explore treatment data #######################
treatment_data <- read_dta("H:\\PREDiCText\\ek\\Flatiron_data_prep\\1_Dates_C_TxPostReport.dta")

# Keep only relevant columns 
# Filter out patients without a report date 
treatment_data <- treatment_data %>%
  filter(!is.na(Date_Report))
# Keep only patients that got treated after the report 
treatment_data <- treatment_data %>% 
  filter(flag_TxPostReport == 1| flag_TxOverlapReport == 1)
##################### Explore mortality data #########################
## Read in dates of last contact for patients 
last_contact_dates <- read_dta("H:\\PREDiCText\\ek\\Flatiron_data_prep\\1_Dates_E_LastContact.dta")

# Filter to selected columns 
last_contact_dates <- last_contact_dates %>%
  select(patientid, Date_LastContact, Date_LastClinicalNote)

# Merge mortality dataset 
mort_dat <- read_dta("H:\\PREDiCText\\lingyi\\Flatiron_exploratory_round1and4\\DeathDates\\DeathDates_AllDataSets_round1and4.dta")

# Let's look at patients with a missing progression date 
missing_prog_date <- prog_data %>%
  filter(is.na(progressiondate))

# 50% have mortality 
mort_dat <- mort_dat %>%
  inner_join(missing_prog_date) %>%
  left_join(treatment_data)

mort_dat <- mort_dat %>%
  distinct()

# Filter out only columns we need
mort_dat <- mort_dat %>% select(patientid, Date_TxStart_PostReport, Date_TxEnd_PostReport, dateofdeath)

# join lot progression 
mort_dat <- mort_dat %>%
  left_join(drug_episodes)

mort_dat <- mort_dat %>%
  select(patientid, Date_TxStart_PostReport, Date_TxEnd_PostReport, dateofdeath, Date_LoT_Progression, linenumber)
#########################################################################

# Clean version of treatment data 
treatment_data <- treatment_data %>%
  filter(patientid, Date_Report, entrectinib_) 

# Reading in progression data
prog_data <- read_dta("H:\\PREDiCText\\ek\\Flatiron_exploratory\\_#_Round4\\Progression\\Progression_allDataSets_round1and4.dta")

prog_data$progressiondate <- as.Date(prog_data$progressiondate)

# Reading in date data 
d_data <- read_dta("H:\\PREDiCText\\_#_ForAnalysis\\temp_Updated_Flatiron_2024Q1cut_id_reportdate_txdate_ntrkpos.dta")


###################### Progression from Post-Report Treatment Line ##################

# Filter to non-missing post-report line number
postreport_valid <- treatment_data %>%
  filter(!is.na(linenumber_TxPostReport)) %>%
  mutate(linenumber = linenumber_TxPostReport)

# Merge to get Date_LoT_Progression
postreport_prog <- postreport_valid %>%
  left_join(drug_episodes, by = c("patientid", "linenumber")) %>%
  filter(Date_TxStart_PostReport <= Date_LoT_Progression) %>%
  select(patientid, Date_LoT_Progression) %>%
  rename(Date_TxProg_PostReport = Date_LoT_Progression)

overlap_valid <- treatment_data %>%
  filter(!is.na(linenumber_TxOverlapReport)) %>%
  mutate(linenumber = linenumber_TxOverlapReport)

overlap_prog <- overlap_valid %>%
  left_join(drug_episodes, by = c("patientid", "linenumber")) %>%
  filter(Date_TxStart_OverlapReport <= Date_LoT_Progression) %>%
  select(patientid, Date_LoT_Progression) %>%
  rename(Date_TxProg_OverlapReport = Date_LoT_Progression)


#######################################################################################


chart_prog <- treatment_data %>%
  select(patientid, Date_TxStart_PostReport, Date_TxEnd_PostReport,
         Date_TxStart_OverlapReport, Date_TxEnd_OverlapReport) %>%
  left_join(prog_data, by = "patientid") %>%
  filter(!is.na(progressiondate)) %>%
  mutate(
    in_post_window = progressiondate >= Date_TxStart_PostReport & progressiondate <= Date_TxEnd_PostReport,
    in_overlap_window = progressiondate >= Date_TxStart_OverlapReport & progressiondate <= Date_TxEnd_OverlapReport
  ) %>%
  group_by(patientid) %>%
  summarise(
    Date_FirstProg_PostReport = min(progressiondate[in_post_window], na.rm = TRUE),
    Date_FirstProg_OverlapReport = min(progressiondate[in_overlap_window], na.rm = TRUE)
  ) %>%
  ungroup()


# Combine all progression signals
all_progression <- treatment_data %>%
  select(patientid) %>%
  distinct() %>%
  left_join(postreport_prog, by = "patientid") %>%
  left_join(overlap_prog, by = "patientid") %>%
  left_join(chart_prog, by = "patientid")





######################################### Discrepancy between progression date and LoT_progression_date #######
# -------- Step 1: Get progression dates after Date_Report from drug_episodes --------
progression_drug <- treatment_data %>%
  select(patientid, Date_Report) %>%
  left_join(drug_episodes %>% select(patientid, Date_LoT_Progression), by = "patientid") %>%
  filter(!is.na(Date_LoT_Progression)) %>%
  filter(Date_LoT_Progression > Date_Report) %>%
  group_by(patientid) %>%
  summarize(first_prog_drug = min(Date_LoT_Progression), .groups = "drop")

# -------- Step 2: Get progression dates after Date_Report from abstracted data --------
progression_text <- treatment_data %>%
  select(patientid, Date_Report) %>%
  left_join(prog_data %>% select(patientid, progressiondate), by = "patientid") %>%
  filter(!is.na(progressiondate)) %>%
  filter(progressiondate > Date_Report) %>%
  group_by(patientid) %>%
  summarize(first_prog_text = min(progressiondate), .groups = "drop")

# -------- Step 3: Combine both to get earliest progression date --------
derived_progression <- treatment_data %>%
  select(patientid, Date_Report, Date_TxStart_PostReport, Date_TxEnd_PostReport) %>%
  filter(!is.na(Date_TxStart_PostReport)) %>%
  left_join(progression_drug, by = "patientid") %>%
  left_join(progression_text, by = "patientid") %>%
  filter(first_prog_drug > Date_Report, first_prog_text > Date_Report, first_prog_drug > Date_TxStart_PostReport, 
         first_prog_text > Date_TxStart_PostReport)


derived_progression <- derived_progression %>%
  slice_sample(n = 150)
# Dumbbell plot to look at the differences between progression_date and LoT_progression_date
ggplot(derived_progression, aes(y = reorder(patientid, first_prog_drug))) +
  
  # Treatment window: faded gray line
  geom_segment(aes(x = Date_TxStart_PostReport, xend = Date_TxEnd_PostReport,
                   yend = patientid),
               color = "gray80", size = 2, alpha = 0.5) +
  
  # Discrepancy line: progression dates
  geom_segment(aes(x = first_prog_drug, xend = first_prog_text, yend = patientid),
               color = "gray50", size = 0.8) +
  
  # Drug-based progression point
  geom_point(aes(x = first_prog_drug), color = "blue", size = 2) +
  
  # Text-based progression point
  geom_point(aes(x = first_prog_text), color = "red", size = 2) +
  
  labs(
    title = "Progression Dates with Treatment Window Overlay",
    subtitle = "Gray = Treatment Period | Blue = LoT drug based | Red = Clinical progression",
    x = "Date",
    y = "Patient ID"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6)  
  )
########################## Clean treatment data ################
treatment_data <- treatment_data %>%
 select(patientid, Date_Report, Date_TxStart_PostReport, Date_TxEnd_PostReport, 
         Date_TxStart_OverlapReport, Date_TxEnd_OverlapReport, 
         linenumber_TxOverlapReport, linenumber_TxPostReport, NTRKfusion) %>%
  filter(!is.na(linenumber_TxOverlapReport) | !is.na(linenumber_TxPostReport))

# Merge with prognosis data
treatment_data <- treatment_data %>%
  left_join(prog_data, by = "patientid")

# Let's look at patients with a missing progression value 
treatment_data <- treatment_data %>%
  filter(is.na(progressiondate))

treatment_data <- treatment_data %>%
  filter(progressiondate > Date_TxStart_PostReport | progressiondate > Date_TxStart_OverlapReport)

# Combine drug episodes with treatment data 
treatment_data <- drug_episodes %>%
  left_join(drug_episodes, by = "patientid")

################################################################### 
patient_list <- prog_data %>%
  filter(!is.na(progressiondate)) %>%
  pull(patientid)

# Let's see how many of the 146 patients have their progression data
d_data_prog <- d_data %>%
  filter(patientid %in% patient_list)

# Progression data 
prog_data_selected <- prog_data %>%
  filter(!is.na(progressiondate)) %>%
  select(patientid, progressiondate)

# Left-join
d_data_prog <- d_data %>%
  left_join(prog_data_selected, by = "patientid")

# Keep patients that were progressionfree at baseline 
d_data_prog <- d_data_prog %>%
  filter(!is.na(progressiondate)) %>%
  mutate(progressiondate = lubridate::ymd(progressiondate)) %>%
  group_by(patientid) %>%
  arrange(progressiondate) %>%
  
  # Count how many progression events were BEFORE the index date
  mutate(
    num_prior_progression_events = sum(progressiondate < Date_Report, na.rm = TRUE),
    
    # Keep only progression dates AFTER index date
    progression_after_index = progressiondate[which(progressiondate > Date_Report)][1]
  ) %>%
  ungroup() %>%
  
  # Rename and clean up
  rename(progressiondate_report = progression_after_index) %>%
  select(-progressiondate) %>%
  
  # Keep one row per patient
  distinct(patientid, .keep_all = TRUE) 

# Remove rows with missing progressiondates to see if there is info in other datasets
d_data_prog <- d_data_prog %>%
  filter(!is.na(progressiondate_report))
  
patients_with_date <- unique(d_data_prog$patientid)

# Rest of progression data 
d_data_lot <- d_data %>%
  filter(!(patientid %in% patients_with_date))

mort_dat <- mort_dat %>%
  select(patientid, parsemonthofdeath)
  
# Let's look at mortality data
d_data_lot <- d_data_lot %>%
  left_join(mort_dat, by = "patientid")

# Let's join drug episode data 
drug_episodes <- drug_episodes %>%
  select(patientid, Date_TxStart, Date_TxEnd, Date_LoT_Progression)

d_data_lot <- d_data_lot %>%
  left_join(drug_episodes, by = "patientid")

# Last clinical follow-up
last_clinical <- prog_data %>%
  select(patientid, lastclinicnotedate) %>%
  distinct(patientid, .keep_all = TRUE)

# Join to data
d_data_lot <- d_data_lot %>%
  left_join(last_clinical, by = "patientid")

################################### LoT progressions #####################
d_data_lot <- d_data_lot %>%
  mutate(progressiondate_report = ifelse((Date_LoT_Progression > Date_Report 
    & Date_LoT_Progression < parsemonthofdeath), Date_LoT_Progression, NA )) %>%
  mutate(death_date = if_else((abs(Date_TxEnd - as.Date(parsemonthofdeath))
    <= 14), Date_TxEnd, as.Date(parsemonthofdeath))) %>%
  mutate(treatment_end_clinical = if_else(Date_TxEnd > as.Date(lastclinicnotedate), Date_TxEnd, as.Date(lastclinicnotedate))) %>%
  group_by(patientid) %>%
  mutate(num_prior_progression_events = n() -1) %>%
  mutate(death_date = min(death_date)) %>%
  ungroup() %>%
  mutate(death_date = if_else(is.na(death_date), as.Date(parsemonthofdeath), death_date)) %>%
  mutate(progressiondate_report = if_else(is.na(progressiondate_report) & Date_LoT_Progression > Date_Report, Date_LoT_Progression, progressiondate_report)) %>%
  distinct(patientid, .keep_all = TRUE) %>%
  mutate(treatment_end_clinical = if_else(!is.na(progressiondate_report), NA, treatment_end_clinical)) %>%
  mutate(fully_missing_outcomes = if_else(
    is.na(progressiondate_report) & is.na(death_date) & is.na(treatment_end_clinical),
    1, 0
  )) %>%
  mutate(treatment_end_clinical = if_else((fully_missing_outcomes == 1 & (Date_TxEnd > Date_Report)), Date_TxEnd, treatment_end_clinical)) 
#  mutate(treatment_end_clinical = if_else(treatment_end_clinical < Date_Report, NA, treatment_end_clinical)) %>%
 # mutate(death_date = if_else(death_date < Date_Report, NA, death_date))
 

# Formatting in PFSreport dataset format 
pfs_report_lot <- d_data_lot %>%
  select(patientid, Date_Report, progressiondate_report, num_prior_progression_events, death_date,
         censor_date = treatment_end_clinical) %>%
  mutate(death_date = ymd(death_date), censor_date = ymd(censor_date), progressiondate_report = ymd(progressiondate_report)) %>%
  mutate(event = ifelse(!is.na(death_date) | !is.na(progressiondate_report), 1, NA)) %>%
  mutate(event = ifelse(!is.na(censor_date), 0, event)) %>%
  mutate(event_type = ifelse(!is.na(death_date), "Death", NA)) %>%
  mutate(event_type = ifelse(!is.na(progressiondate_report), "LoT progression", event_type)) %>%
  mutate(event_type = ifelse(!is.na(censor_date), "censored", event_type)) %>%
  mutate(combined_dates = coalesce(death_date, progressiondate_report, censor_date)) %>%
  mutate(time_to_event = as.numeric(combined_dates - Date_Report)) %>%
  select(-combined_dates)


########################################################################


# Create dataset for date_report 
pfs_report <- d_data_prog %>%
  select(patientid, Date_Report, progressiondate_report, num_prior_progression_events) %>%
  mutate(death_date = NA) %>%
  mutate(censor_date = NA) %>%
  mutate(event_type = "clinical progression") %>%
  mutate(event = 1) %>%
  mutate(time_to_event = as.numeric(progressiondate_report - Date_Report)) %>%
  mutate(death_date = ymd(death_date), censor_date = ymd(censor_date), progressiondate_report = ymd(progressiondate_report))

# Bind rows
pfs_report <- rbind(pfs_report, pfs_report_lot)

flatiron_data <- flatiron_data %>%
  filter(patientid %in% patient_id)

flatiron_data <- flatiron_data %>%
  mutate(latest_date = pmax(Date_LastContact, Date_LastClinicalNote, na.rm = TRUE)) %>%
  select(patientid, latest_date)
  

pfs_report <- pfs_report %>%
  left_join(flatiron_data, by = "patientid") %>%
  mutate(censor_date = coalesce(latest_date, censor_date)) %>%
  mutate(event_type = ifelse(!is.na(censor_date), "censored", event_type)) %>%
  mutate(event = ifelse(!is.na(censor_date), 0, event)) %>%
  mutate(combined_dates = coalesce(death_date, progressiondate_report, censor_date)) %>%
  mutate(time_to_event = as.numeric(combined_dates - Date_Report)) %>%
  select(-combined_dates) %>%
  select(-latest_date)

pfs_report <- pfs_report %>%
  mutate(event_type = ifelse(!is.na(death_date) & (death_date < Date_Report), "censored", event_type)) %>%
  mutate(event = ifelse(!is.na(death_date) & (death_date < Date_Report), 0, event)) %>%
  mutate(time_to_event = ifelse(time_to_event < 0, 0, time_to_event))
  
# Remaining patients in treatment data and progression data ###########################
missing_patient_ids <- pfs_report %>%
  filter(is.na(event)) %>%
  pull(patientid)

prog_data <- prog_data %>%
  select(patientid, lastclinicnotedate)

treatment_data <- treatment_data %>%
  filter(patientid %in% missing_patient_ids) %>%
  left_join(prog_data, by = "patientid")

treatment_data <- treatment_data %>%
  mutate(Date_TxEnd_PostReport = ymd(Date_TxEnd_PostReport), lastclinicnotedate = ymd(lastclinicnotedate)) %>%
  mutate(combined_date = pmax(Date_TxEnd_PostReport, lastclinicnotedate, na.rm = TRUE)) %>%
  filter(!is.na(combined_date))

treatment_data <- treatment_data %>%
  select(patientid, combined_date)

pfs_report <- pfs_report %>%
  left_join(treatment_data, by = "patientid") %>%
  mutate(censor_date = coalesce(combined_date, censor_date)) %>%
  mutate(event_type = ifelse(!is.na(censor_date), "censored", event_type)) %>%
  mutate(event = ifelse(!is.na(censor_date), 0, event)) %>%
  mutate(combined_dates = coalesce(death_date, progressiondate_report, censor_date)) %>%
  mutate(time_to_event = as.numeric(combined_dates - index_date)) %>%
  select(-combined_dates, -combined_date)

xx <- pfs_report %>%
  filter(is.na(event))



################################## Function to clean and derive the progression dates ###################
progression_date_data <- function(cohort_data, progression_data, mortality_data,  treatment_data, episode_data, flatiron_data, index_date_column) {
  # Remove patients that have a missing index date
  cohort_data <- cohort_data %>%
    drop_na(all_of(index_date_column))
  message(glue("Dropped patients without index date, number of rows in dataset: {nrow(cohort_data)}"))
  # Join the progression data
  cohort_data_prog <- cohort_data %>%
    left_join(progression_data, by = "patientid")
  # Count of patients with progression data 
  index_sym <- rlang::sym(index_date_column)  # Convert string to symbol
  # Clinical progression dates only 
  clinical_prog_dates <- cohort_data_prog %>%
    filter(!is.na(progressiondate)) %>%
    mutate(progressiondate = lubridate::ymd(progressiondate), index_date = ymd(!!index_sym)) %>%
    group_by(patientid) %>%
    arrange(progressiondate, .by_group = TRUE) %>%
    mutate(
      num_prior_progression_events = sum(progressiondate < index_date, na.rm = TRUE),
      progression_after_index = first(progressiondate[progressiondate > index_date])
    ) %>%
    ungroup() %>%
    rename(progressiondate_report = progression_after_index) %>%
    distinct(patientid, .keep_all = TRUE) %>%
    drop_na(progressiondate_report)
  message(glue("The number of patients with valid progression dates relative to the index 
       is: {nrow(clinical_prog_dates)}"))
  # Create PFS formatted dataset for patients with progressiondate
  pfs_report <- clinical_prog_dates %>%
    select(patientid, index_date, progressiondate_report, num_prior_progression_events) %>%
    mutate(death_date = NA) %>%
    mutate(censor_date = NA) %>%
    mutate(event_type = "clinical progression") %>%
    mutate(event = 1) %>%
    mutate(time_to_event = as.numeric(progressiondate_report - index_date)) %>%
    mutate(death_date = ymd(death_date), censor_date = ymd(censor_date), progressiondate_report = ymd(progressiondate_report))
  message(glue("Finished writing cohort of patients with clinical progression dates of size: {nrow(pfs_report)}"))
  
  # Remove cohort_data_prog
  rm(cohort_data_prog)
  # LoT progressions, deaths and censored cases
  # Patient ID's 
  cohort_data <- cohort_data %>%
    filter(!(patientid %in% unique(pfs_report$patientid)))
  
  # Loading in mortality data
  message(glue("Loading in Mortality data"))
  mortality_data <- mortality_data %>%
    select(patientid, parsemonthofdeath)
  
  # Let's look at mortality data
  cohort_data <- cohort_data %>%
    left_join(mortality_data, by = "patientid")
  
  # Let's join drug episode data 
  message(glue("Loading in Drug Episode data"))
  drug_episodes <- drug_episodes %>%
    select(patientid, Date_TxStart, Date_TxEnd, Date_LoT_Progression)
  
  cohort_data <- cohort_data %>%
    left_join(drug_episodes, by = "patientid")
  
  # Treatment data 
  message(glue("Loading in Treatment data"))
  treatment_data <- treatment_data %>%
   select(patientid, Date_TxStart_PostReport, Date_TxEnd_PostReport)
  cohort_data <- cohort_data %>%
    left_join(treatment_data, by = "patientid")
  
  # Progression data
  message(glue("Loading in progression data"))
  progression_data <- progression_data %>%
    select(patientid, lastclinicnotedate) %>%
    distinct(patientid, .keep_all = TRUE)
  
  cohort_data <- cohort_data %>%
    left_join(progression_data, by = "patientid")
  
  # Flatiron data
  message(glue("Loading in flatiron data cut for date of last contact"))
  flatiron_data <- flatiron_data %>%
    select(patientid, Date_LastContact) %>%
    distinct(patientid, .keep_all = TRUE)
  cohort_data <- cohort_data %>%
    left_join(flatiron_data, by = "patientid")
  ################################### LoT progressions #####################
  cohort_data <- cohort_data %>%
    mutate(across(starts_with("Date"), ymd)) %>%
    mutate(parsemonthofdeath = ymd(parsemonthofdeath),
           lastclinicnotedate = ymd(lastclinicnotedate),
           index_date = ymd(!!index_sym)) %>%
    
    # Keep progression only if after index_date
    mutate(progressiondate_report = if_else(
      Date_LoT_Progression > index_date,
      Date_LoT_Progression,
      as.Date(NA)
    )) %>%
    
    # Create proxy for death-related events
    mutate(
      coalesced_death_proxy = pmax(
        Date_TxEnd,
        Date_LastContact,
        lastclinicnotedate,
        Date_TxEnd_PostReport,
        na.rm = TRUE
      )
    ) %>%
    
    # Flag deaths before index
    mutate(
      pre_index_death_flag = case_when(
        !is.na(parsemonthofdeath) & parsemonthofdeath <= index_date ~ 1,
        TRUE ~ 0
      )
    ) %>%
    
    # Then assign death date only if after index
    mutate(
      death_date = case_when(
        !is.na(coalesced_death_proxy) &
          format(coalesced_death_proxy, "%Y-%m") == format(parsemonthofdeath, "%Y-%m") &
          coalesced_death_proxy > index_date ~ coalesced_death_proxy,
        parsemonthofdeath > index_date ~ parsemonthofdeath,
        TRUE ~ as.Date(NA)
      )
    ) %>%
  
    
    # Assign censor date only if both progression and death are NA, and proxy is after index
    mutate(
      censor_date = case_when(
        is.na(progressiondate_report) & is.na(death_date) & coalesced_death_proxy > index_date ~ coalesced_death_proxy,
        TRUE ~ as.Date(NA)
      )
    ) %>%
    
    # Count of prior progression events per patient (if grouped earlier)
    group_by(patientid) %>%
    mutate(num_prior_progression_events = n() - 1) %>%
    ungroup() %>%
    distinct(patientid, .keep_all = TRUE)
  
    glue::glue("⚠️ Excluding {sum(cohort_data$pre_index_death_flag == 1)} patients who died before the index date.")
    cohort_data <- cohort_data %>%
      filter(pre_index_death_flag == 0)
    
  # Formatting in PFSreport dataset format 
  pfs_report_lot <- cohort_data %>%
    select(patientid, index_date, progressiondate_report, num_prior_progression_events, 
           death_date, censor_date) %>%
    
    # Make sure all date fields are dates
    mutate(
      across(c(death_date, censor_date, progressiondate_report), lubridate::ymd)
    ) %>%
    
    # Prioritize event flags: progression > death > censor
    mutate(
      event_type = case_when(
        !is.na(progressiondate_report) ~ "LoT progression",
        !is.na(death_date) ~ "Death",
        !is.na(censor_date) ~ "censored",
        TRUE ~ NA_character_
      ),
      event = case_when(
        event_type %in% c("LoT progression", "Death") ~ 1,
        event_type == "censored" ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    
    # Event time: pick first event in this order
    mutate(
      combined_dates = coalesce(progressiondate_report, death_date, censor_date),
      time_to_event = as.numeric(combined_dates - index_date)
    ) %>%
    
    select(-combined_dates)
  
  # Bind rows
  pfs_report <- rbind(pfs_report, pfs_report_lot)
  message(glue("Number of patients with missing events (no known censoring date, 
               last follow-up, death, progression): {sum(is.na(pfs_report$event))}"))
  # Outcome summary logging
  message(glue("Number of patients who died: {sum(pfs_report$event_type == 'Death', na.rm = TRUE)}"))
  message(glue("Number of patients with LoT progression: {sum(pfs_report$event_type == 'LoT progression', na.rm = TRUE)}"))
  message(glue("Number of patients censored: {sum(pfs_report$event_type == 'censored', na.rm = TRUE)}"))
  
  # Dropping all NA's
  pfs_report <- pfs_report %>%
    drop_na(event)
  message(glue("Final cohort of patients with known events to derive PFS: {nrow(pfs_report)}"))
  message(glue("Error checking: number of patients with negative time to event: {sum(pfs_report$time_to_event <0)}"))
  return(pfs_report)
  
}


# Set the line width for separator
line_width <- 60

# Helper function to log section headers
log_section <- function(title) {
  sep_line <- strrep("=", line_width)
  centered_title <- stringr::str_pad(title, width = line_width, side = "both")
  cat("\n", sep_line, "\n", centered_title, "\n", sep_line, "\n\n", sep = "")
}

# Logging setup
log_con <- file("E:/progression_log.txt", open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

# First run
log_section("Running progression_date_data() using Date_Report as index")
date_report_progression_data <- progression_date_data(
  cohort_data = d_data,
  progression_data = prog_data,
  mortality_data = mort_dat,
  flatiron_data = flatiron_data,
  treatment_data = treatment_data,
  episode_data = drug_episodes,
  index_date_column = "Date_Report"
)

# Second run
log_section("Running progression_date_data() using Date_TxStart_PostReport_Combined as index")
date_txreport_progression_data <- progression_date_data(
  cohort_data = d_data,
  progression_data = prog_data,
  mortality_data = mort_dat,
  flatiron_data = flatiron_data,
  treatment_data = treatment_data,
  episode_data = drug_episodes,
  index_date_column = "Date_TxStart_PostReport_Combined"
)

# Close connections
sink(type = "message")
sink()
close(log_con)

write.csv(date_report_progression_data, "E:\\date_report_progression_data.csv")
write.csv(date_txreport_progression_data, "E:\\date_txreport_progression_data.csv")

#################################################################################
# Look at progression in ECOG data 
ecog_data <- read_dta("H:\\PREDiCText\\lingyi\\Flatiron_exploratory_round1and4\\ECOG\\ECOG_allDataSets_round1and4.dta")

# Patient ID's in cohort data 
patient_id <- d_data %>%
  pull(patientid) %>% unique()

# Filter in ECOG data 
ecog_data <- ecog_data %>%
  filter(patientid %in% patient_id)

###############################
# Read in medication administration data 
med_admin_data <- read_dta("H:\\PREDiCText\\ek\\Flatiron_data_prep\\0_MedicationAdministration_allDataSets_round1and4.dta")

# Filter patients in cohort data
med_admin_data <- med_admin_data %>%
  filter(patientid %in% patient_id)

xx <- med_admin_data %>% filter(patientid == "066781F3EBD48612CB8") %>% arrange(administereddate)

# Construct episodes from medication data 
med_data <- med_data %>%
  mutate(administereddate = ymd(administereddate))  # Or your date column

med_data <- med_data %>%
  group_by(patientid) %>%
  arrange(administereddate, .by_group = TRUE) %>%
  mutate(
    time_diff = as.numeric(administereddate - lag(administereddate)),
    class_change = drugcategory != lag(drugcategory),
    new_line = if_else(is.na(time_diff) | time_diff >= 60 | class_change == TRUE, 1, 0),
    line_number = cumsum(replace_na(new_line, 0)) + 1
  ) %>%
  ungroup()

drug_episodes <- med_data %>%
  group_by(patientid, line_number) %>%
  summarise(
    start_date = min(administereddate),
    end_date = max(administereddate),
    regimen = paste(sort(unique(drugname)), collapse = " + "),
    num_drugs = n_distinct(drugname),
    .groups = "drop"
  )

# Step 1: Group by patient and date to form a “combo”
combo_df <- med_admin_data %>%
  group_by(patientid, administereddate) %>%
  summarise(
    regimen_name = paste(sort(unique(commondrugname)), collapse = "+"),
    .groups = "drop"
  )

# Step 2: Create episode windows based on 7-day gaps
combo_df <- combo_df %>%
  arrange(patientid, administereddate) %>%
  group_by(patientid) %>%
  mutate(
    days_since_last = as.numeric(difftime(administereddate, lag(administereddate, default = first(administereddate)), units = "days")),
    new_episode = if_else(row_number() == 1 | days_since_last > 7 | regimen_name != lag(regimen_name), 1, 0),
    episode_id = cumsum(new_episode)
  ) %>%
  group_by(patientid, episode_id) %>%
  summarise(
    regimen_name = first(regimen_name),
    start_date = min(administereddate),
    end_date = max(administereddate),
    .groups = "drop"
  )

# Final result
print(combo_df)

# Check medication order dataset 
med_order <- read_dta("H:\\PREDiCText\\lingyi\\Flatiron_exploratory_round1and4\\MedicationOrder\\MedicationOrder_bladder_round1.dta")

################################# Let's look at the vitals dataset ################################
vitals_dat <- read_dta("H:\\PREDiCText\\ek\\Flatiron_exploratory\\_#_Round4\\Vitals\\Vitals_allDataSets_round1and4.dta")

##################### Read in trial data ###################################
rozlytrek_dat <- readRDS("H:\\PREDiCText\\nirupama\\fromEK\\Rozlytrek\\Rozly_rawR\\pr.rds")

rozlytrek_dat[] <- lapply(rozlytrek_dat, function(col) {
  if (is.character(col)) {
    col[col %in% c("NA", "")] <- NA
  }
  return(col)
})

######################## Explore if Flatiron has any other information on progression ###################
############## Reading in enhanced dataset 
lab_dat <- read_dta("H:\\PREDiCText\\lingyi\\Flatiron_exploratory_round1and4\\Lab\\Lab_crc_round4.dta")

# Join progression data 
prog_data <- read_dta("H:\\PREDiCText\\ek\\Flatiron_exploratory\\_#_Round4\\Progression\\Progression_allDataSets_round1and4.dta")

# Let's look at the gap between the LoT progression date and Flatiron date for patients who have both 
lot_clinical_progression <- drug_episodes_flatiron %>%
  left_join(prog_data, by = "patientid") %>%
  mutate(progressiondate = lubridate::ymd(progressiondate)) %>%
  select(patientid, Date_LoT_Progression, progressiondate) %>%
  group_by(patientid) %>%
  drop_na(Date_LoT_Progression, progressiondate) %>%
  summarise(
    first_lot_progression = min(Date_LoT_Progression, na.rm = TRUE),
    first_progressiondate = min(progressiondate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(gap_dates = as.numeric(first_lot_progression - first_progressiondate))

#################################### Read in one raw episodes dataset #####################################
# Lung cancer data
sclc_ep_round4 <- read_dta("H:\\PREDiCText\\lingyi\\Flatiron_exploratory_round1and4\\DrugEpisodes\\DrugEpisode_sclc_round4.dta")

# Lung cancer data
sclc_ep_round1 <- read_dta("H:\\PREDiCText\\lingyi\\Flatiron_exploratory_round1and4\\DrugEpisodes\\DrugEpisode_sclc_round1.dta")


# Lung cancer data
nsclc_ep_round1 <- read_dta("H:\\PREDiCText\\lingyi\\Flatiron_exploratory_round1and4\\DrugEpisodes\\DrugEpisode_nsclc_round1.dta")

# Lung cancer data
nsclc_ep_round4 <- read_dta("H:\\PREDiCText\\lingyi\\Flatiron_exploratory_round1and4\\DrugEpisodes\\DrugEpisode_nsclc_round4.dta")


lung_data <- rbind(sclc_ep_round4, sclc_ep_round1, nsclc_ep_round1, nsclc_ep_round4)

# Read in last contact data 
last_contact_data <- read_dta("H:\\PREDiCText\\ek\\Flatiron_data_prep\\1_Dates_E_LastContact.dta")

# Read in death data
mort_dat <-  read_dta("H:\\PREDiCText\\lingyi\\Flatiron_exploratory_round1and4\\DeathDates\\DeathDates_AllDataSets_round1and4.dta")

# Create drug category data 
drug_category <- lung_data %>%
  select(linename, detaileddrugcategory) %>%
  distinct()



last_contact_data <- last_contact_data %>%
  left_join(mort_dat, by = "patientid") %>%
  select(patientid, Date_LastContact, Date_LastClinicalNote, Date_TxEnd_PostReport, parsemonthofdeath) %>%
  mutate(parsemonthofdeath = lubridate::ymd(parsemonthofdeath)) %>%
  mutate(Date_LastFollowUp = pmax(Date_LastContact, Date_LastClinicalNote, Date_TxEnd_PostReport, na.rm = TRUE)) %>%
  mutate(
    coalesced_death_proxy = pmax(
      Date_LastContact,
      Date_LastClinicalNote,
      Date_TxEnd_PostReport,
      na.rm = TRUE
    )) %>% 
  mutate(
    death_date = case_when(
      !is.na(coalesced_death_proxy) & !is.na(parsemonthofdeath) &
        format(coalesced_death_proxy, "%Y-%m") == format(parsemonthofdeath, "%Y-%m") ~ coalesced_death_proxy,
      !is.na(parsemonthofdeath) ~ parsemonthofdeath,
      TRUE ~ as.Date(NA)
    )
  ) %>%
  select(patientid, Date_LastFollowUp, death_date)

# Filter for patients with second line as cisplatin
lung_ep_patients <- lung_data %>%
  filter(str_detect(tolower(linename), "cisplatin"), linenumber == 2) %>%
  pull(patientid) %>% unique()

# Joining mortality data 
lung_ep <- lung_data %>%
  select(patientid, linenumber, linename, linestartdate, lineenddate, episodedate, detaileddrugcategory) %>%
  drop_na(linenumber) %>%
  filter(patientid %in% lung_ep_patients) %>%
 # filter(linenumber != 1) %>%
  group_by(patientid, linenumber)%>%
  summarise(linename,
    linestartdate = min(linestartdate),
    lineenddate = max(episodedate),
    .groups = "drop"
  ) %>%
  distinct() %>%
  left_join(last_contact_data, by = "patientid")

# Convert to dates
lung_ep <- lung_ep %>%
  arrange(patientid, linenumber) %>%
  mutate(linestartdate = lubridate::ymd(linestartdate), lineenddate = lubridate::ymd(lineenddate),
         death_date = lubridate::ymd(death_date), Date_LastFollowUp = lubridate::ymd(Date_LastFollowUp))


###################### Error handling for function ###################################
# Look at overlapping lines 
lung_ep_overlap_flagged <- lung_ep %>%
  arrange(patientid, linestartdate, linenumber) %>%
  group_by(patientid) %>%
  mutate(
    prev_end = lag(lineenddate),
    overlaps_previous = if_else(!is.na(prev_end) & linestartdate < prev_end, TRUE, FALSE)
  ) %>%
  ungroup() 


lung_ep_grouped <- lung_ep_overlap_flagged %>%
  arrange(patientid, linestartdate) %>%
  group_by(patientid) %>%
  mutate(
    group_id = cumsum(!overlaps_previous | is.na(overlaps_previous))
  ) %>%
  ungroup()

lung_ep_cleaned <- lung_ep_grouped %>%
  group_by(patientid, group_id) %>%
  summarise(
    linestartdate = min(linestartdate),
    lineenddate   = max(lineenddate),
    linename      = paste(sort(unique(linename)), collapse = " + "),
    .groups = "drop"
  ) %>%
  arrange(patientid, linestartdate) %>%
  group_by(patientid) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()

lung_ep_cleaned <- lung_ep_cleaned %>%
  mutate(
    linename = str_replace_all(linename, "\\b(\\w+)( \\+ \\1)+\\b", "\\1"),
    linename = str_squish(gsub("\\s*\\+\\s*", " + ", linename))  # standardize spacing
  )

############# Look at patients that seem to only have a single dose#########################
patients_single_dose <- lung_ep_cleaned %>%
  group_by(patientid) %>%
  filter(
    as.numeric(lineenddate - linestartdate) == 0 
  )

clean_lung_lines <- function(lung_ep, drug_separator = ",", overlap_threshold = 1) {
 
  # Helper: Split and compare drugs as sets
  split_drugs <- function(name) sort(trimws(unlist(strsplit(name, drug_separator))))
  has_overlap <- function(name1, name2, threshold = overlap_threshold) {
    if (is.na(name1) | is.na(name2)) return(FALSE)
    length(intersect(split_drugs(name1), split_drugs(name2))) >= threshold
  }
  
  # Prep and flag
  lung_ep_flagged <- lung_ep %>%
    arrange(patientid, linestartdate, linenumber) %>%
    group_by(patientid) %>%
    mutate(
      is_single_day = as.numeric(lineenddate - linestartdate) == 0,
      prev_end      = lag(lineenddate),
      prev_name     = lag(linename),
      prev_gap      = as.numeric(linestartdate - prev_end),
      next_start    = lead(linestartdate),
      next_name     = lead(linename),
      next_gap      = as.numeric(next_start - lineenddate)
    ) %>%
    ungroup() %>%
    mutate(
      merge_with_prev = mapply(function(x, y, gap, single) {
        single & !is.na(gap) & has_overlap(x, y)
      }, linename, prev_name, prev_gap, is_single_day),
      
      merge_with_next = mapply(function(x, y, gap, single) {
        single & !is.na(gap) & has_overlap(x, y)
      }, linename, next_name, next_gap, is_single_day),
      
      drop_line = is_single_day & !merge_with_prev & !merge_with_next
    )
  
  # Collapse single-dose into previous or next
  to_merge <- bind_rows(
    lung_ep_flagged %>% filter(merge_with_prev) %>% mutate(target_line = linenumber - 1),
    lung_ep_flagged %>% filter(merge_with_next) %>% mutate(target_line = linenumber + 1)
  )
  
  target_lines <- lung_ep_flagged %>%
    select(patientid, linenumber, linestartdate, lineenddate, linename) %>%
    rename(
      target_line   = linenumber,
      target_start  = linestartdate,
      target_end    = lineenddate,
      target_name   = linename
    )
  
  to_merge_full <- to_merge %>%
    left_join(target_lines, by = c("patientid", "target_line"))
  
  collapsed_lines <- to_merge_full %>%
    rowwise() %>%
    mutate(
      linestartdate = min(linestartdate, target_start, na.rm = TRUE),
      lineenddate   = max(lineenddate, target_end, na.rm = TRUE),
      linename = paste(sort(unique(c(
        split_drugs(linename),
        split_drugs(target_name)
      ))), collapse = " + ")
    ) %>%
    ungroup() %>%
    select(patientid, linestartdate, lineenddate, linename) %>% distinct()
  
  # Remove all merged or dropped lines
  lines_to_remove <- bind_rows(
    to_merge %>% select(patientid, linenumber),
    to_merge %>% select(patientid, linenumber = target_line),
    lung_ep_flagged %>% filter(drop_line) %>% select(patientid, linenumber)
  )
  
  lung_ep_retained <- lung_ep_flagged %>%
    anti_join(lines_to_remove, by = c("patientid", "linenumber")) %>%
    select(patientid, linestartdate, lineenddate, linename)
  
  # Combine retained and collapsed, then reassign line numbers
  lung_ep_final <- bind_rows(lung_ep_retained, collapsed_lines) %>%
    arrange(patientid, linestartdate) %>%
    group_by(patientid) %>%
    mutate(linenumber = row_number()) %>%
    ungroup()
  
  
  
  return(lung_ep_final)
}


# Fix to remove duplicate lines TODO: Monday
lung_ep_cleaned <- clean_lung_lines(lung_ep_cleaned)


################# Look for lines with a very short gap in between #########
gap_lines <- lung_ep_cleaned %>%
  arrange(patientid, linestartdate) %>%
  filter(linenumber >= 1) %>%
  group_by(patientid) %>%
  mutate(
    next_start = lead(linestartdate),
    gap_days = as.numeric(difftime(next_start, lineenddate, units = "days"))
  ) %>%
  ungroup() %>%
  filter(gap_days <= 3)

# Join drug category 
gap_lines <- gap_lines %>%
  left_join(drug_category, by = "linename")


lung_ep_patients <- lung_ep_cleaned %>%
  filter(str_detect(tolower(linename), "cisplatin"), linenumber == 2) %>%
  pull(patientid) %>% unique()

lung_ep_cleaned <- lung_ep_cleaned %>%
  filter(patientid %in% lung_ep_patients) 

lung_ep_cleaned <- lung_ep_cleaned %>%
  filter(linenumber > 1)

# Join last treatment data 
lung_ep_cleaned <- lung_ep_cleaned %>%
  left_join(last_contact_data, by = "patientid")
#####################################################################################
########## Flag patients with a very large number of lines ####################
patients_high_lots <- lung_ep_cleaned %>%
  filter(linenumber > 5)%>%
  pull(patientid) %>% unique()

lot_high <- lung_ep_cleaned %>%
  filter(patientid %in% patients_high_lots)

collapse_recycled_lines <- function(data, n_back = 2, jaccard_threshold = 0.25, gap_days = 365) {
 
  # Helper to split and clean drug names
  split_drugs <- function(name) {
    sort(trimws(unlist(strsplit(name, ",|\\+"))))
  }
  
  # Jaccard similarity between two drug regimens
  jaccard_similarity <- function(drugs1, drugs2) {
    intersect_len <- length(intersect(drugs1, drugs2))
    union_len <- length(union(drugs1, drugs2))
    if (union_len == 0) return(0)
    intersect_len / union_len
  }
  
  data <- data %>%
    arrange(patientid, linestartdate) %>%
    group_by(patientid) %>%
    mutate(
      linename_vec = lapply(linename, split_drugs),
      prev_lines = purrr::map2_lgl(
        row_number(),
        linename_vec,
        function(i, current_drugs) {
          if (i == 1) return(NA)
          jaccard_list <- purrr::map2_lgl(
            linename_vec[max(1, i - n_back):(i - 1)],
            linestartdate[max(1, i - n_back):(i - 1)],
            function(past_drugs, past_date) {
              sim <- jaccard_similarity(current_drugs, past_drugs)
              gap <- as.numeric(linestartdate[i] - past_date)
              
              # Print for debugging
              cat("Line", i, "→ sim:", sim, "gap:", gap, "\n")
              
              sim >= jaccard_threshold & gap <= gap_days
            }
          )
          any(jaccard_list)
        }
      ),
      collapse = ifelse(is.na(prev_lines), FALSE, prev_lines)
    ) %>%
    mutate(
      collapse_group = cumsum(!collapse | is.na(collapse))
    ) %>%
    group_by(patientid, collapse_group) %>%
    summarise(
      linestartdate = min(linestartdate),
      lineenddate   = max(lineenddate),
      linename      = paste(sort(unique(unlist(strsplit(paste(linename, collapse = ","), ",|\\+")))), collapse = " + "),
      .groups = "drop"
    ) %>%
    arrange(patientid, linestartdate) %>%
    group_by(patientid) %>%
    mutate(linenumber = row_number()) %>%
    ungroup()
  
  return(data)
}

lung_ep_collapsed <- collapse_recycled_lines(lung_ep_cleaned, n_back = 2, jaccard_threshold = 0.5, gap_days = 90)

# Remove patients with more than 6 lines
lung_ep_cleaned <- lung_ep_cleaned %>%
  filter(!(patientid %in% patients_high_lots))
###############################################################################
# Off-treatment threshold 
gap_threshold <- 7

# Step 1: Clean lines and calculate gaps
lines <- lung_ep_cleaned %>%
  arrange(patientid, linestartdate) %>%
  filter(linenumber >= 1) %>%
  group_by(patientid) %>%
  mutate(
    next_start = lead(linestartdate),
    gap_days = as.numeric(difftime(next_start, lineenddate, units = "days")),
    has_gap = gap_days >= gap_threshold,
    base_time = first(linestartdate)
  ) %>%
  ungroup()

on_treatment <- lines %>%
  filter(linenumber >= 2) %>%
  mutate(
    state = paste0("On_Treatment_Line", linenumber)
  ) %>%
  filter(!is.na(state)) %>%
  transmute(
    patientid,
    state,
    start_date = linestartdate,
    end_date = lineenddate,
    base_time, 
    event = 1
  )

# Step 3: Off_Treatment between lines with gaps
off_treatment <- lines %>%
  filter(has_gap) %>%
  transmute(
    patientid,
    state = "Off_Treatment",
    start_date = lineenddate,
    end_date = next_start,
    base_time, 
    event = 1
  )

final <- lines %>%
  group_by(patientid) %>%
  slice_max(linestartdate, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    final_end = coalesce(death_date, Date_LastFollowUp),
    died = !is.na(death_date),
    state = case_when(
      died ~ "Off_Treatment",
      lineenddate < final_end ~ "Off_Treatment",
      TRUE ~ paste0("On_Treatment_Line", linenumber)
    ),
    event = as.integer(died)  # dynamically assign event
  ) %>%
  transmute(
    patientid,
    state,
    start_date = lineenddate,
    end_date = final_end,
    base_time,
    event,
    death_date
  ) %>%
  filter(!is.na(end_date) & end_date > start_date)


# Step 5: Death state (absorbing)
death_state <- final %>%
  filter(state == "Off_Treatment", !is.na(end_date), end_date == death_date) %>%
  transmute(
    patientid,
    state = "Death",
    start_date = end_date,
    end_date = end_date,
    base_time, 
    event = 1
  )

# Step 6: Combine
state_durations <- bind_rows(
  on_treatment,
  off_treatment,
  final,
  death_state
) %>%
  mutate(
    start_time = as.numeric(start_date - base_time),
    end_time = as.numeric(end_date - base_time),
    duration = end_time - start_time
  ) %>%
  select(patientid, state, start_time, end_time, duration, event) %>%
  arrange(patientid, start_time)



# STEP 1: Assign state IDs
state_levels <- unique(state_durations$state)
state_map <- tibble(state = state_levels, state_id = seq_along(state_levels))


# Define allowed transitions based on your model
allowed_transitions <- tribble(
  ~from, ~to,
  1,     2,
  1,     3,
  1,     4,
  1,     5,
  1,     6,
  2,     3,
  2,     4,
  2,     5,
  2,     6,
  3,     2,
  3,     4,
  3,     5,
  3,     6,
  4,     2,
  4,     5,
  4,     6,
  5,     2,
  5,     6
)

# Observed transitions 
obs_transitions <- state_durations %>%
  left_join(state_map, by = "state") %>%
  group_by(patientid) %>%
  arrange(start_time) %>%
  mutate(
    from = lag(state_id),
    to = state_id,
    Tstart = lag(start_time),
    Tstop = start_time
  ) %>%
  ungroup() %>%
  filter(!is.na(from)) %>%
  mutate(status = event)

# Possible transitions 
expand_possible <- obs_transitions %>%
  select(patientid, from, Tstart, Tstop) %>%
  distinct() %>%
  inner_join(allowed_transitions, by = "from")

transitions_all <- expand_possible %>%
  left_join(obs_transitions %>% 
              select(patientid, from, to, Tstart, Tstop, status_obs = status),
            by = c("patientid", "from", "to", "Tstart", "Tstop")
  ) %>%
  mutate(
    status = ifelse(is.na(status_obs), 0, status_obs)
  ) %>%
  select(patientid, from, to, Tstart, Tstop, status)

transitions_all <- transitions_all %>%
  mutate(trans = match(paste(from, to), paste(allowed_transitions$from, allowed_transitions$to)))

# Look at rows where Tstop < Tstart
tstop_odd <- transitions_all %>%
  filter(Tstop <= Tstart)

transitions_all <- transitions_all %>%
  filter(Tstop > Tstart)

# Fit model 
crwei <- flexsurvreg(Surv(Tstart, Tstop, status) ~ trans + shape(factor(trans)), data = transitions_all, dist = "weibull")

# Get actual transition values used
trans_ids <- sort(unique(transitions_all$trans))

# Base estimates (for the reference transition — usually lowest trans ID)
base_shape <- crwei$res["shape", "est"]
base_scale <- crwei$res["scale", "est"]

# Initialize offsets
logscale_offset <- numeric(length(trans_ids))
logshape_offset <- numeric(length(trans_ids))

# Loop through each transition ID
for (j in seq_along(trans_ids)) {
  t <- trans_ids[j]
  
  if (t == min(trans_ids)) {
    logscale_offset[j] <- 0
    logshape_offset[j] <- 0
  } else {
    scale_name <- paste0("trans", t)
    shape_name <- paste0("shape(trans", t, ")")
    
    if (scale_name %in% rownames(crwei$res)) {
      logscale_offset[j] <- crwei$res[scale_name, "est"]
    }
    
    if (shape_name %in% rownames(crwei$res)) {
      logshape_offset[j] <- crwei$res[shape_name, "est"]
    }
  }
}

# Combine into parameter table
params <- tibble::tibble(
  trans = trans_ids,
  shape = exp(log(base_shape) + logshape_offset),
  scale = exp(log(base_scale) + logscale_offset)
)

# 
fit_line3 <- flexsurvreg(Surv(start_time, end_time, event) ~ 1,
                         data = state_durations,
                         dist = "weibull",
                         subset = state == "On_Treatment_Line2")

####################### Looking at progression data ##############################
prog_data <- prog_data %>%
  select(patientid, progressiondate)

# Join 
state_prog <- state_durations %>%
  left_join(prog_data)
