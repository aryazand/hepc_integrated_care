define_cohort <- function(patient_id_df,
                          patients.manual_review,
                          chronic_hepc_in_study_period,
                          chronic_hepc_intervals, 
                          consults_df,
                          antiviral_data) {
  # Create df
  df <- patient_id_df |> as_tibble()
  
  # Was the patient screened in for having a positive hep c test, hepc
  # diagnosis, or antiviral prescriptions?
  df <- df |>
    mutate(screened_in = PATIENT_ID %in% patients.manual_review$PATIENT_ID)
  
  # Did the patient have 1st episode of chronic hep c during the study period
  df <- chronic_hepc_in_study_period |> 
    select(PATIENT_ID, chronic_hepc_in_study_period = chronic_hepc) |> 
    distinct() |> 
    right_join(df, by = "PATIENT_ID") |> 
    select(PATIENT_ID, screened_in, chronic_hepc_in_study_period)

  # Had MAT clinic seen patient while they had chronic hep c Define cohort based
  # on strictness: Strict = only include patients that had a MAT visit while HCV
  # infected during study period Not strict = additionally include patients that
  # were diagnosed with chronic HCV at MAT encounter but not MAT visit was
  # recorded
  
  df <- chronic_hepc_intervals |> 
    filter(complete == TRUE) |>
    select(PATIENT_ID, start_date) |> 
    mutate(MAT_clinic_aware_of_hepc_during_study_period = !is.na(start_date)) |> 
    distinct() |> 
    right_join(df, by = "PATIENT_ID") |> 
    select(PATIENT_ID, screened_in, chronic_hepc_in_study_period, 
           MAT_clinic_aware_of_hepc_during_study_period)
  
  # Does the patient have imcomplete data
  data_list <- list(chronic_hepc_in_study_period, 
                    chronic_hepc_intervals,
                    antiviral_data,
                    consults_df)
  incomplete_data <- map(data_list, ~filter(.x, complete == FALSE)) |> bind_rows()
  df$incomplete_data <- df$PATIENT_ID %in% incomplete_data$PATIENT_ID
  df$include_in_cohort <- df$MAT_clinic_aware_of_hepc_during_study_period & !df$incomplete_data
  
  return(df)
}



