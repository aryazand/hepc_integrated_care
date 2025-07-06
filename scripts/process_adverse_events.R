
process_adverse_events <- function(non_repeat_data, antiviral_data, reviewer_key) { 
  
  pts_started_antiviral <- antiviral_data |> 
    filter(antiviral_started == "Yes") |> 
    pull(PATIENT_ID)
  
  adverse_events_data <- non_repeat_data |> 
    filter(PATIENT_ID %in% pts_started_antiviral) |> 
    select( 
      PATIENT_ID, 
      study_id, 
      adverse_events, 
      antiviral_disruption
    ) |> 
    mutate(gi_upset = grepl("GI\\w|acid reflux|nause", adverse_events),
           fatigue = grepl("tired|fatigue", adverse_events), 
           headache = grepl("Headache", adverse_events)) |> 
    select(-adverse_events)
  
    # Process concordance
    # Discordant data is removed from object and moved into attr
    adverse_events_data <- adverse_events_data |>
    assess_completeness(id_variables = "PATIENT_ID", reviewer_key = reviewer_key)

  return(adverse_events_data)
}
