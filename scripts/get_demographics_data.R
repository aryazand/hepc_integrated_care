get_demographics_data <- function(encounters_data, patient_key) {
  
  # Replace mrn with PATIENT_ID
  encounters_df <- encounters_data |> 
    left_join(patient_key, by = "mrn", relationship = "many-to-one") |> 
    distinct() |> 
    select(-mrn)
  
  # obtain demographics data from encounters df
  # use slice_min to pick first visit in study period
  demographics_df <- encounters_df |>
    group_by(PATIENT_ID) |> 
    slice_min(visit_date, with_ties = FALSE) |> 
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
