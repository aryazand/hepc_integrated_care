select_death_dates <- function(non_repeat_data, reviewer_key) {
  
  # Get patients that died 
  death <- non_repeat_data |> 
    filter(death == TRUE) |> 
    select(PATIENT_ID, study_id, death_date) 
  
  # Process concordance
  # Discordant data is removed from object and moved into attr   
  death <- death |> 
    assess_completeness(id_variables = "PATIENT_ID", reviewer_key = reviewer_key)

  return(death)
}