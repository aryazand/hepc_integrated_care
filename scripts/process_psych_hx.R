convert_choices_dataframe <- function(x) {
  x <- x |>
    strsplit(split = "[\\|,]") |> 
    purrr::map(stringr::str_trim) |> 
    purrr::map(matrix, ncol = 2, byrow = TRUE, 
               dimnames =  list(NULL, c("V1", "V2"))) |> 
    purrr::map(tibble::as_tibble, .name_repair = 'unique')
  
  return(x)
}

group_split_with_groupnames <- function(df) {
  group_names <- df |> dplyr::group_keys() |> unlist()
  df <- df |> dplyr::group_split()
  names(df) <- group_names  
  
  return(df)
}

get_substance_and_psych_forms <- function(redcap_metadata) {
  
  # Find column names from the substance and psych form ######
  substanceuse_and_psych_cols <- redcap_metadata |> 
    dplyr::filter(form_name == "substance_and_psych_history") |>
    dplyr::select(field_name, field_type, choices = select_choices_or_calculations)
  
  # The names of checkbox questions are adjusted by redcap
  # so we have to rename columsn because of that 
  substanceuse_and_psych_cols <- substanceuse_and_psych_cols |> 
    dplyr::group_by(field_type) |>
    group_split()
  
  substanceuse_and_psych_cols[[1]] <- substanceuse_and_psych_cols[[1]] |>
    dplyr::mutate(choices = convert_choices_dataframe(choices)) |> 
    tidyr::unnest(choices) |> 
    dplyr::rename(choices = "V2") |>
    tidyr::unite("field_name", field_name, V1, sep = "___")
    
  
  # put them all together
  substanceuse_and_psych_cols <- substanceuse_and_psych_cols |> bind_rows()
  
  return(substanceuse_and_psych_cols)
}

change_yes_no_unknown_values <- function(vec) {
  renaming_vec <- c(
    "No" = "0", 
    "Yes" = "1",
    "Unknown" = "2"
  )
  
  vec <- vec |>
    forcats::fct_expand(renaming_vec) |>
    forcats::fct_recode(!!!renaming_vec)
  
  return(vec)
} 

process_substanceandpsych_data <- function(redcap_metadata, non_repeat_data, reviewer_key) {
  
  # Find column names from the substance and psych form ######
  substance_and_psych_forms <- get_substance_and_psych_forms(redcap_metadata)
    
  # Select column names #######
  substanceuse_psych_data <- non_repeat_data |> 
    dplyr::select(PATIENT_ID, study_id, substance_and_psych_forms$field_name)  
  
  # Change column names from checkbox forms to be more informative #####
  checkbox_forms <- substance_and_psych_forms |>
    dplyr::filter(field_type == "checkbox") |>
    tidyr::separate(field_name, c("field_name_2", "option"), sep = "___", remove = F) |>
    dplyr::group_by(field_name_2) |>
    group_split_with_groupnames()
  
  checkbox_forms$mat_during_therapy$choices <- 
    sub("^Unknown$", "Unknown_MAT", checkbox_forms$mat_during_therapy$choices)
  
  checkbox_forms$mat_while_infected$choices <- 
    sub("^Unknown$", "Unknown_MAT", checkbox_forms$mat_while_infected$choices)
  
  checkbox_forms$mat_while_infected$choices <- 
    sub("^Other$", "Other_MAT", checkbox_forms$mat_while_infected$choices)
  
  checkbox_forms$psych_hx$choices <- 
    sub("^Other$", "Other_psych_hx", checkbox_forms$psych_hx$choices)
  
  checkbox_forms$substance_use_disorder$choices <- 
    paste0(checkbox_forms$substance_use_disorder$choices, "_use_disorder") 
  
  checkbox_forms$mat_during_therapy$choices <- 
    paste0(checkbox_forms$mat_during_therapy$choices, "_on_hcv_therapy")
  
  checkbox_forms <- checkbox_forms |> unname() |> dplyr::bind_rows()

  renaming_vec <- checkbox_forms$field_name
  names(renaming_vec) <- checkbox_forms$choices
  
  substanceuse_psych_data <- substanceuse_psych_data |> 
    dplyr::rename(all_of(renaming_vec))
  
  # Change yes-no-unknown form values
  yes_no_unknown_cols <- substance_and_psych_forms |> 
    dplyr::filter(choices == "0, No | 1, Yes | 2, Unknown") |>
    dplyr::pull(field_name)
  
  substanceuse_psych_data <- substanceuse_psych_data |> 
    mutate(across(all_of(yes_no_unknown_cols), change_yes_no_unknown_values))
  
  # Assess concordance between between reviewers
  variables_to_check <- c('Opioid_use_disorder', 
                          'Alcohol_use_disorder', 
                          'Methamphetamine_use_disorder', 
                          'Cocaine_use_disorder', 
                          'Benzodiazepine_use_disorder', 
                          'Tobacco_use_disorder', 
                          'Other_use_disorder', 
                          'Bupernorphine', 
                          'Methadone', 
                          'Naltrexone', 
                          'Acamprosate', 
                          'Disulfaram', 
                          'Other_MAT', 
                          'Unknown_MAT', 
                          'Depression', 
                          'Anxiety/Panic', 
                          'PTSD', 
                          'Bipolar', 
                          'Schizophrenia', 
                          'Other_psych_hx', 
                          'iv_drug_use_hx')
  
  substanceuse_psych_data <- substanceuse_psych_data |> 
    assess_completeness(id_variable = "PATIENT_ID", 
                        cols_to_check_for_concordance = variables_to_check,
                        cols_to_check_for_missing = variables_to_check,
                        reviewer_key = reviewer_key)
  
  return(substanceuse_psych_data)
}