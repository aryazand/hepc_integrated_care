process_hcv_pcr_data <- function(hcv_quantitative_pcr, pts_with_chronic_hepc) {
  
  # remove incomplete records  #####
  hcv_quantitative_pcr <- hcv_quantitative_pcr |>
    filter(!is.na(hepc_pcr_value)) |>
    filter(!is.na(hepc_pcr_date))

  # Filter out pcr values from records indicated as not having first chronic hep c episode
  hcv_quantitative_pcr <- hcv_quantitative_pcr |>
    filter(PATIENT_ID %in% pts_with_chronic_hepc)
  
  # verify input data  #####
  hcv_quantitative_pcr <- hcv_quantitative_pcr |>
    assertr::verify(is.character(study_id)) |>
    assertr::verify(lubridate::is.Date(hepc_pcr_date)) |>
    assertr::verify(hepc_pcr_value >= 0) |>
    assertr::verify(!(is.na(hepc_pcr_value_type) & hepc_pcr_value > 0))

  # Make all values log based  #####
  # Round log values to 2 decimal points #####
  hcv_quantitative_pcr <- hcv_quantitative_pcr |>
    mutate(log_pcr_value = case_when(
      hepc_pcr_value_type == 1 & hepc_pcr_value != 0 ~ log10(hepc_pcr_value),
      .default = hepc_pcr_value
    )) |> 
    mutate(log_pcr_value = round(log_pcr_value, 2))
  
  # Assess if pcr positive #####
  hcv_quantitative_pcr <- hcv_quantitative_pcr |>
    mutate(hepc_pcr_positive = log_pcr_value > 0)
  
  # Drop unnecessary columns ####
  hcv_quantitative_pcr <- hcv_quantitative_pcr |>
    select(
      PATIENT_ID, study_id, hepc_pcr_date,
      log_pcr_value, hepc_pcr_positive,
      first_hepc_pcr, hepc_pcr_clinic,
      hepc_pcr_source 
    )
  
  # Drop duplicate rows
  hcv_quantitative_pcr <- hcv_quantitative_pcr |> distinct()
  
  # Change first_hepc_pcr to logical
  levels(hcv_quantitative_pcr$first_hepc_pcr) <- c("TRUE", "FALSE")
  hcv_quantitative_pcr$first_hepc_pcr <- 
    as.logical(hcv_quantitative_pcr$first_hepc_pcr)
  
  # Change hepc_pcr_clinic to logical
  levels(hcv_quantitative_pcr$hepc_pcr_clinic) <- c("N", "Y", "Unknown")
  
  # Verify output data
  hcv_quantitative_pcr <- hcv_quantitative_pcr |>
    assertr::verify(is.character(PATIENT_ID)) |>
    assertr::verify(is.character(study_id)) |>
    assertr::verify(is.Date(hepc_pcr_date)) |>
    assertr::verify(is.double(log_pcr_value)) |>
    assertr::verify(is.logical(hepc_pcr_positive)) |>
    assertr::verify(is.logical(first_hepc_pcr)) |>
    assertr::verify(is.factor(hepc_pcr_clinic))
  
  return(hcv_quantitative_pcr)
}
