select_chronic_hepc_assessment0 <- function(non_repeat_data) {
  
  # Identify which patients marked with having chronic hepc 
  redcap.chronic_hepc <- non_repeat_data |>
    select(PATIENT_ID, study_id, chronic_hepc) 
  
  return(redcap.chronic_hepc)
}


select_chronic_hepc_assessment <- function(non_repeat_data, reviewer_key) {
  
  # Identify which patients marked with having chronic hepc 
  chronic_hepc_df <- select_chronic_hepc_assessment0(non_repeat_data)
  
  # Process concordance
  # Discordant data is removed from object and moved into attr 
  chronic_hepc_df <- chronic_hepc_df |> 
    assess_completeness(id_variables = "PATIENT_ID", reviewer_key = reviewer_key)
  
  # Assess for missing
  
  return(chronic_hepc_df)
  
}