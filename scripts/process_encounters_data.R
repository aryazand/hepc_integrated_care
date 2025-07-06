helper_select_columns <- function(df) {
  
  # Select columns
  df <- df |> 
    select(any_of(c("MRN", "DOB", "Sex", "Race", "Ethnicity")),
          Visit_Date = matches("(Appointment|Visit)\\.Date"),
          Appt_Status = where(~any(grepl("Completed", .x))),
          contains("county"),
          State = contains("state"),
          Labs = contains("labs")
    ) |> 
    tibble() 
  
  # Some of the selection above selected 2 columns, resulting in column names as
  # var1 and var2. We will fix that here
  remove_ending_digit <- function(x) {
    sub("1$", "", x)
  }
  
  df <- df |> 
    select(-matches("2$")) |> 
    rename_at(vars(matches("1$")), remove_ending_digit)
  
  # Rename county code columns 
  # Criteria = contains "county" in name and is only digits 
  df <- df |> 
    rename(County_Code = contains("county") & where(~all(grepl("^\\d+$", .x[!is.na(.x)]))))
  
  # Rename county code columns 
  # Criteria = contains "county" in name and is only digits 
  df <- df |> 
    rename(County = contains("county") & where(~all(!grepl("^\\d+$", .x[!is.na(.x)]))))
  
  return(df)
}


process_encounters_data <- function(encounters_data_list, patient_key) {
  
  # We have multiple raw datasets pulled from Epic with encounter information
  # here we load them up and standardized column names 
  
  encounters_data <- map(encounters_data_list, helper_select_columns)
  encounters_data <- bind_rows(encounters_data)
  
  # Filter out incomplete appts 
  encounters_data <- encounters_data |> 
    filter(!(Appt_Status %in% c("Canceled Appt", "Left w/o Seen", "No Show Appt"))) |> 
    select(-Appt_Status)
  
  # Remove duplicates 
  encounters_data <- encounters_data |> 
    group_by(MRN, Visit_Date) |> 
    fill(Ethnicity, County_Code, .direction = "downup") |> 
    fill(Labs, .direction = "downup") |> 
    ungroup() |> 
    distinct()
  
  # make colnames lowercases
  colnames(encounters_data) <- tolower(colnames(encounters_data))
  
  # Replace mrn with PATIENT_ID
  encounters_data <- encounters_data |> 
    left_join(patient_key, by = "mrn", relationship = "many-to-one") |> 
    select(PATIENT_ID, dob, visit_date, sex, race, ethnicity, county, state, labs)
  
  return(encounters_data)
}
