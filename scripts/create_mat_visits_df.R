create_mat_visits_df <- function(encounters_df) {
  # Combine encounters data and assignments data to
  # associate PATIENT_ID with visit dates

  # Load data
  mat_visits_df <- encounters_df |>
    select(PATIENT_ID, visit_date)
  
  # Verify output
  mat_visits_df <- mat_visits_df |>
    assertr::verify(is.character(PATIENT_ID)) |>
    assertr::verify(lubridate::is.Date(visit_date))

  return(mat_visits_df)
}