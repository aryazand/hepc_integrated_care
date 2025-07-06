process_consult_data <- function(consults_df, redcap_metadata, reviewer_key) {
  # verify input
  consults_df <- consults_df |>
    assertr::verify(is.character(study_id)) |>
    assertr::verify(lubridate::is.Date(consult_date)) |>
    assertr::verify(is.factor(consulted_service)) |>
    assertr::verify(is.factor(referring_service))

  # Relabel the factors
  consulted_service <- get_factor_lookup(redcap_metadata, field_name_choice = "consulted_service")
  referring_service <- get_factor_lookup(redcap_metadata, field_name_choice = "referring_service")

  consults_df$consulted_service <- consults_df$consulted_service |>
    fct_expand(consulted_service) |>
    fct_recode(!!!consulted_service)

  consults_df$referring_service <- consults_df$referring_service |>
    fct_expand(referring_service) |>
    fct_recode(!!!referring_service)
  
  # Replace NA was no consult in consulted_service
  consults_df$consulted_service <- replace_na(consults_df$consulted_service, "no consult")
  
  # Select relevant columns
  consults_df <- consults_df |>
    select(
      PATIENT_ID, study_id, consult_date, referring_service, consulted_service, consult_performed, consult_appt_date
    )
  
  # filter out no consults
  consults_df <- consults_df |> filter(!is.na(consult_date))
  
  # Process completeness
  cols_to_check_for_missing <- c("consult_date", "referring_service", "consulted_service", "consult_performed")
  consults_df <- consults_df |> 
    assess_completeness(id_variables = c("PATIENT_ID", "consult_date"), 
                        cols_to_check_for_missing = cols_to_check_for_missing,
                        reviewer_key = reviewer_key)
  
  return(consults_df)
}
