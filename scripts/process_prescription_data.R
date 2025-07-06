get_antiviral_therapy_data <- function(x) {
  x |> select(c(
    "PATIENT_ID",
    "study_id",
    "antiviral_therapy",
    "prescription_date",
    "prescription_weeks",
    "prescription_clinician",
    "antiviral_started",
    "antiviral_start_date",
    "antiviral_completed",
    "date_completion",
    "prior_authorization_denied"
  ))
}

code_prescriber_to_clinic <- function(prescribers, prescriber_regex) {
  
  # Code to hepatology
  hepatology_regex <- prescriber_regex |> 
    filter(clinic == "Hepatology") |> 
    select(regex) |> 
    c() |> 
    paste(collapse = "|")
  
  prescribers[grep(hepatology_regex, prescribers, ignore.case = TRUE)] <- "Hepatology"

  # Code to ID 
  id_regex <- prescriber_regex |> 
    filter(clinic == "ID") |> 
    select(regex) |> 
    c() |> 
    paste(hepatology_regex, collapse = "|")
  
  prescribers[grep(id_regex, prescribers, ignore.case = TRUE)] <- "iACCESS"

  # Code to MAT Clinic
  mat_regex <- prescriber_regex |> 
    filter(clinic == "MAT") |> 
    select(regex) |> 
    c() |> 
    paste(hepatology_regex, collapse = "|")
  
  prescribers[grep(mat_regex, prescribers, ignore.case = TRUE)] <- "MAT"

  # Code to Other
  other_regex <- prescriber_regex |> 
    filter(clinic == "Other") |> 
    select(regex) |> 
    c() |> 
    paste(hepatology_regex, collapse = "|")
  
  prescribers[grep(other_regex, prescribers, ignore.case = TRUE)] <- "Other"

  return(prescribers)
}

process_prescription_data <- function(non_repeat_data, redcap_metadata, prescriber_regex, reviewer_key) {
  
  # Select prescription data
  antiviral_prescription_data <- get_antiviral_therapy_data(non_repeat_data)

  # Code prescribers to clinic
  antiviral_prescription_data$prescription_clinic <-
    code_prescriber_to_clinic(antiviral_prescription_data$prescription_clinician, prescriber_regex) |>
    as_factor()

  # Code drug prescribed
  antiviral_therapies <- get_factor_lookup(redcap_metadata, "antiviral_therapy")
  antiviral_prescription_data$antiviral_therapy <- antiviral_prescription_data$antiviral_therapy |>
    fct_expand(antiviral_therapies) |>
    fct_recode(!!!antiviral_therapies)

  # Select columns and filter out records without prescriptions
  antiviral_prescription_data <- antiviral_prescription_data |>
    select(
      PATIENT_ID, study_id, prescription_date, prescription_clinic,
      antiviral_started, antiviral_start_date,
      antiviral_therapy, antiviral_completed, date_completion,
      prior_authorization_denied
    ) |>
    filter(!is.na(prescription_date) & !is.na(antiviral_therapy))
  
  # Code start data 
  antiviral_prescription_data$antiviral_started <- 
    fct_recode(antiviral_prescription_data$antiviral_started, 
               "Yes" = "0",
               "Yes" = "1", 
               "No" = "2", 
               "Unknown" = "3")
  
  # Code completion data
  antiviral_prescription_data <- antiviral_prescription_data |> 
    mutate(antiviral_completed = replace(
      antiviral_completed,
      which(is.na(antiviral_completed) & antiviral_started == "No"),
      values = "0"
    )) |> 
    mutate(antiviral_completed = replace(
      antiviral_completed,
      which(is.na(antiviral_completed) & antiviral_started %in% c("Unknown", "Yes")),
      values = "2"
    )) |> 
    mutate(antiviral_completed = replace_na(antiviral_completed, "2")) |>
    mutate(antiviral_completed = fct_recode(antiviral_completed, 
                                            "No" = "0",
                                             "Yes" = "1", 
                                             "Unknown" = "2"))
    
  # Verify output
  antiviral_prescription_data <- antiviral_prescription_data |>
    assertr::verify(all(prescription_clinic %in% c("Hepatology", "iACCESS", "MAT", "Other")))
  
  # Process completeness
  cols_to_check_for_missing <- c("prescription_date", "prescription_clinic", "antiviral_started", "antiviral_therapy")
  antiviral_prescription_data <- antiviral_prescription_data |> 
    assess_completeness(id_variables = "PATIENT_ID", 
                       cols_to_check_for_missing = cols_to_check_for_missing,
                       reviewer_key = reviewer_key)
  
  return(antiviral_prescription_data)
}
