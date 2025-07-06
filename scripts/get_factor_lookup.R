get_factor_lookup <- function(redcap_metadata, field_name_choice) {
  choices <- redcap_metadata |> 
    dplyr::filter(field_name == field_name_choice) |>
    dplyr::select(select_choices_or_calculations) |>
    unlist() |>
    strsplit(split = "[\\|,]") |>
    unlist() |>
    stringr::str_trim()
  
  choices_lookup <- choices[seq(1, length(choices) - 1, 2)]
  names(choices_lookup) <- choices[seq(2, length(choices), 2)]
  
  return(choices_lookup)
}
