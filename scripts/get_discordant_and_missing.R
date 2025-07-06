get_discordant_and_missing <- function(df, reviewer_key, cohortIDs) {
  df.discordant <- attr(df, "discordant") |> 
    filter(PATIENT_ID %in% cohortIDs)
  
  patients_in_data <- c(df$PATIENT_ID, df.discordant$PATIENT_ID)
  df.missing <- cohortIDs[!(cohortIDs %in% patients_in_data)] 
  
  if(length(df.missing) > 0) {
    df.missing <- reviewer_key |> 
      select(PATIENT_ID, study_id) |> 
      filter(PATIENT_ID %in% df.missing)
    
    df.discordant_or_missing <- full_join(df.discordant, df.missing)
    
  } else {
    df.discordant_or_missing <- df.discordant
  }
  
  return(df.discordant_or_missing)
}