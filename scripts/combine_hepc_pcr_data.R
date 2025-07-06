combine_hepc_pcr_data <- function(hepc_pcr_results.manual,
                                  hepc_pcr_results.epic, 
                                  reviewer_key) {
  
  # Process epic pull hepc pcr data
  hepc_pcr_results.epic <- hepc_pcr_results.epic |>
    filter(lab_type == "PCR") |>
    select(PATIENT_ID, pcr_date = lab_date, lab_value) |>
    left_join(reviewer_key, relationship = "many-to-many") |>
    select(-mrn, -reviewer)
  
  hepc_pcr_results.manual <- hepc_pcr_results.manual |>
    select(PATIENT_ID, study_id, pcr_date = hepc_pcr_date, lab_value = hepc_pcr_positive) |>
    mutate(lab_value = factor(lab_value)) |>
    mutate(lab_value = fct_recode(lab_value,
                                  "Negative" = "FALSE",
                                  "Positive" = "TRUE"
    ))
  
  hepc_pcr <- bind_rows(hepc_pcr_results.epic, hepc_pcr_results.manual) |> distinct()
  
  return(hepc_pcr)
}