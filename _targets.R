library(tarchetypes)
library(targets)
library(cyphr)
library(tidyverse)
library(REDCapR)
library(here)
library(checkmate)
library(assertr)
library(arsenal)
library(survival)

###### Load Key #############
key_hash <- digest::digest(Sys.getenv("CYPHR_KEY"), algo = "sha512")

###### Helper scripts ##########
# key <- cyphr::key_sodium(sodium::hex2bin(Sys.getenv("CYPHR_KEY")))
tread <- function(...) {
  tar_read(..., store = here("_targets/")) |> cyphr::decrypt_object(key)
}

##### Load scripts ########
source_files <- list.files(here("scripts"), pattern = ".R", full.names = T)
for (i in source_files) {
  source(i)
}

####### Paths to Data #############
encounters_data_list_path <- list.files("data/MAT_encounters/", pattern = ".RDS", full.names = TRUE)
patient_key_path <- here("data/patient_key.RDS")
reviewer_key_path <- here("data/reviewer_key.RDS")
hepc_lab_data_path <- here("data/lab_data_pull/hepc_labpull_20240603.RDS")
redcap_data_path <- here("data/redcap_data.RDS")
prescriber_regex <- here("data/prescriber_regex.csv")

####### Load Parameters ############
study_period <- interval(start = "2019-01-01", end = "2023-08-31")
max_interval_length <- 365 * 5
last_observation_date <- as_date("2023-08-31")
law_change_date = ymd("2019-01-01")
max_duration_fib4 = ddays(1)

####### Parameters to download Redcap Data if needed ############
token <- Sys.getenv("REDCAP_TOKEN")
url <- "https://redcap.icts.uiowa.edu/redcap/api/"
DOWNLOAD = FALSE

######### Recipes ################

setup <- list(
  tar_target(
    key, {
      key_hash
      cyphr::key_sodium(sodium::hex2bin(Sys.getenv("CYPHR_KEY")))
    }
  )
)

pipeline <- list(
  tar_target(patient_key,
             cyphr::decrypt(readRDS(patient_key_path), key)
  ),
  tar_target(reviewer_key,
             cyphr::decrypt(readRDS(reviewer_key_path), key)
  ),
  tar_target(encounters_data_list,
             map(encounters_data_list_path, ~decrypt(readRDS(.x), key))
  ),
  tar_target(hepc_lab_data,
             cyphr::decrypt(readRDS(hepc_lab_data_path), key = key)
  ),
  tar_target(patients.manual_review, 
    reviewer_key |> select(PATIENT_ID) |> distinct()
  ),
  tar_force(redcap_data,
    get_redcap_data(url, token, reviewer_key),
    force = DOWNLOAD == TRUE
  ),
  tar_target(chronic_hepc_in_study_period,
    select_chronic_hepc_assessment(
      redcap_data[["non_repeat_data"]],
      reviewer_key
  )),
  tar_target(chronic_hepc_IDs, 
    chronic_hepc_in_study_period |> 
      filter(chronic_hepc == TRUE & complete == TRUE) |> 
      pull(PATIENT_ID)
  ),
  tar_target(redcap_data.chronic_hepc, filter_redcap(
    redcap_data,
    chronic_hepc_IDs
  )),
  tar_target(death_dates, select_death_dates(
    redcap_data.chronic_hepc[["non_repeat_data"]],
    reviewer_key
  )),
  tar_target(hepc_pcr_values, process_hcv_pcr_data(
    redcap_data.chronic_hepc[["hcv_quantitative_pcr"]],
    chronic_hepc_IDs
  )),
  tar_target(antiviral_data, process_prescription_data(
    redcap_data.chronic_hepc[["non_repeat_data"]],
    redcap_data.chronic_hepc[["metadata"]],
    prescriber_regex,
    reviewer_key
  )),
  tar_target(consults_df, process_consult_data(
    redcap_data.chronic_hepc[["consult"]],
    redcap_data.chronic_hepc[["metadata"]],
    reviewer_key
  )),
  tar_target(encounters_data, process_encounters_data(
    encounters_data_list,
    patient_key
  )),
  tar_target(mat_visits_df, create_mat_visits_df(
    encounters_data
  )),
  tar_target(hepc_lab_history, process_hepc_labs_from_epic(
    hepc_lab_data,
    patient_key
  )),
  tar_target(chronic_hepc_intervals, create_chronic_hepc_interval(
    hepc_pcr_values,
    hepc_lab_history,
    antiviral_data |> filter(complete == TRUE) |> select(-discordant, -missing, -complete),
    death_dates |> filter(complete == TRUE) |> select(-discordant, -missing, -complete),
    consults_df |> filter(complete == TRUE) |> select(-discordant, -missing, -complete),
    mat_visits_df,
    reviewer_key,
    study_period
  )),
  tar_target(chronic_hepc_cohort, define_cohort(
    select(patient_key, PATIENT_ID),
    patients.manual_review,
    chronic_hepc_in_study_period,
    chronic_hepc_intervals,
    antiviral_data,
    consults_df
  )),
  tar_target(cohortIDs,
    chronic_hepc_cohort |> filter(include_in_cohort == TRUE) |> pull(PATIENT_ID)
  ),
  tar_target(survival_table_svr, create_survival_table(
    chronic_hepc_intervals |> filter(complete == TRUE),
    cohortIDs,
    consults_df |> filter(complete == TRUE),
    antiviral_data |> filter(complete == TRUE),
    end_date_col = "end_date.svr",
    end_marker_col = "end_marker.svr",
    success_marker = "1",
    censor_marker = c("2","3","4","5"),
    law_change_date,
    "svr"
  )),
  tar_target(survival_table_rx_completion, create_survival_table(
    chronic_hepc_intervals |> filter(complete == TRUE),
    cohortIDs,
    consults_df |> filter(complete == TRUE),
    antiviral_data |> filter(complete == TRUE),
    end_date_col = "end_date.rx_completion",
    end_marker_col = "end_marker.rx_completion",
    success_marker = "1",
    censor_marker = c("2","3", "4"),
    law_change_date,
    "completed_antiviral"
  )),
  tar_target(survival_table_svr.relaxed, filter_survival_table_policy(
    survival_table_svr,
    policy = "relaxed"
  )),
  tar_target(survival_table_rx_completion.relaxed, filter_survival_table_policy(
    survival_table_rx_completion,
    policy = "relaxed"
  )),
  tar_target(combined_hepc_pcr_data, combine_hepc_pcr_data(
    hepc_pcr_results.manual = hepc_pcr_values,
    hepc_pcr_results.epic = hepc_lab_history,
    reviewer_key
  )),
  tar_target(svr_data, process_svr_data(
    antiviral_data |> filter(complete == TRUE),
    combined_hepc_pcr_data,
    reviewer_key
  )),
  tar_target(cohort_demographics, process_cohort_demographics_data(
    encounters_data,
    cohortIDs,
    chronic_hepc_intervals
  )),
  tar_target(clinic_demographics, process_clinic_demographics_data(
    encounters_data,
    cohortIDs
  )),
  tar_target(redcap_data.cohort, filter_redcap(
    redcap_data,
    cohortIDs
  )),
  tar_target(adverse_events_data, process_adverse_events(
    redcap_data.cohort[["non_repeat_data"]],
    antiviral_data,
    reviewer_key
  )),
  tar_target(comorbidities_df, select_comorbidities_data(
    redcap_data.cohort[["non_repeat_data"]],
    reviewer_key
  )),
  tar_target(hepb_df, select_hepb_data(
    redcap_data.cohort[["non_repeat_data"]],
    reviewer_key
  )),
  tar_target(fib4_data, get_fib4_data(
    redcap_data.cohort[["non_repeat_data"]],
    cohort_demographics,
    max_duration_fib4 = max_duration_fib4,
    reviewer_key
  )),
  tar_target(psych_df, process_substanceandpsych_data(
    redcap_data.cohort[["metadata"]],
    redcap_data.cohort[["non_repeat_data"]],
    reviewer_key
  ))
) |>
  tar_hook_outer(hook = encrypt_object(.x, key = cyphr::key_sodium(sodium::hex2bin(Sys.getenv("CYPHR_KEY"))))) |>
  tar_hook_inner(hook = decrypt_object(.x, key = cyphr::key_sodium(sodium::hex2bin(Sys.getenv("CYPHR_KEY")))))

list(setup, pipeline)