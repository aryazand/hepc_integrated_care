process_clinic_demographics_data <- function(encounters_data, cohortIDs) {
  
  # select first encounter in study period
  encounters_df <- encounters_data |> 
    group_by(PATIENT_ID) |> 
    slice_min(visit_date, with_ties = F) |>
    select(PATIENT_ID, sex, dob, race, ethnicity, county, state) |> 
    ungroup()
    
  # assign if patients were in cohort 
  encounters_df <- encounters_df |> 
    mutate(in_cohort = PATIENT_ID %in% cohortIDs) 
  
  # Number of visits during the study period 
  visits_df <- encounters_data |> 
    group_by(PATIENT_ID) |> 
    summarise(
      first_visit = min(visit_date),
      last_visit = max(visit_date),
      n_visits = n_distinct(visit_date)
    ) |>
    ungroup()
  
  demographics_df <- left_join(encounters_df, visits_df, by = "PATIENT_ID")

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
