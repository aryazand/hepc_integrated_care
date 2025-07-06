process_cohort_demographics_data <- function(encounters_data, cohortIDs, chronic_hepc_intervals) {
  
  # filter to just patient's in cohort
  encounters_df <- encounters_data |> 
    filter(PATIENT_ID %in% cohortIDs)
  
  # Get cohort start date for each patient
  cohort_start_date <- chronic_hepc_intervals |> 
    select(PATIENT_ID, start_date) |> 
    distinct()
  
  # obtain demographics data from encounters df
  # use slice_min to select date closest to cohort start date
  demographics_df <- encounters_df |>
    left_join(cohort_start_date, by = "PATIENT_ID") |> 
    group_by(PATIENT_ID) |> 
    slice_min(abs(visit_date - start_date), with_ties = FALSE) |> 
    select(PATIENT_ID, sex, dob, race, ethnicity, county, state) |> 
    ungroup()
  
  visits_df <- encounters_df |> 
    group_by(PATIENT_ID) |> 
    summarise(
      first_visit = min(visit_date),
      last_visit = max(visit_date),
      n_visits = n_distinct(visit_date)
    ) |>
    ungroup()
  
  demographics_df <- left_join(demographics_df, visits_df, by = "PATIENT_ID")

  # process ethnicity factor names
  demographics_df <- demographics_df |>
    mutate(ethnicity = sub("\\s\\[[[:digit:]]\\]$", "", ethnicity)) |> 
    mutate(ethnicity = replace_na(ethnicity, "Unknown"))
  
  # process factors
  demographics_df <- demographics_df |>
    mutate(across(c(sex, race, ethnicity, state), forcats::as_factor)) |>
    mutate(across(c(sex, race, ethnicity, state), forcats::fct_drop)) 

  return(demographics_df)
}
