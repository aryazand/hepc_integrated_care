filter_redcap <- function(redcap_data, patient_ids) {

  contains_pt_data <- names(redcap_data) != "metadata"
  redcap_data[contains_pt_data] <- redcap_data[contains_pt_data] |>
    map(~ filter(.x, PATIENT_ID %in% patient_ids))
  
  return(redcap_data)
}