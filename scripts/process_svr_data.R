process_svr_data <- function(antiviral_data, hepc_pcr_data, reviewer_key) {
  
  svr_data <- antiviral_data |> 
    left_join(hepc_pcr_data, by = c("PATIENT_ID", "study_id"), relationship = "one-to-many") |> 
    mutate(checked_svr = pcr_date > (date_completion + weeks(12))) |>
    group_by(PATIENT_ID, study_id) |>
    nest(data = c(pcr_date, lab_value, checked_svr)) |> 
    mutate(data = map(data, ~filter(.x, checked_svr == TRUE))) |> 
    unnest(data, keep_empty = TRUE) |> 
    slice_min(pcr_date) |> 
    ungroup() |> 
    mutate(svr = lab_value == "Negative") |> 
    mutate(svr = replace_na(svr, FALSE),
           checked_svr = replace_na(checked_svr, FALSE)) |> 
    rename(checked_svr_date = pcr_date) |> 
    select(PATIENT_ID, study_id, checked_svr, checked_svr_date, svr)
  
  cols_to_check <- c("checked_svr", "svr")
  svr_data <- svr_data |> 
    assess_completeness(id_variables = "PATIENT_ID", 
                        cols_to_check_for_concordance = cols_to_check,
                        cols_to_check_for_missing = cols_to_check,
                        reviewer_key = reviewer_key)
  
  return(svr_data)
}
