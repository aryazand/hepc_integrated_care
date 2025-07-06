select_comorbidities_data <- function(non_repeat_data, reviewer_key) {
  x <- non_repeat_data |>
    dplyr::select(
      PATIENT_ID,
      study_id,
      tidyselect::starts_with("comorbidities__"),
      tidyselect::starts_with("contraindicating_conditions__")
    ) |>
    dplyr::rename(
      Diabetes = comorbidities___0,
      Warfarin = comorbidities___1,
      HIV = comorbidities___2,
      ESRD = comorbidities___3,
      Cirrhosis = contraindicating_conditions___0,
      HCC = contraindicating_conditions___1,
      Transplant = contraindicating_conditions___2,
      Pregnancy = contraindicating_conditions___3,
      Prior_Tx = contraindicating_conditions___4
    )
  
  # Process concordance
  # Discordant data is removed from object and moved into attr 
  x <- x |> 
    assess_completeness(id_variables = "PATIENT_ID", reviewer_key = reviewer_key)
  
  return(x)
  
}

select_hepb_data <- function(non_repeat_data, reviewer_key) {
  x <- non_repeat_data |>
    dplyr::select(PATIENT_ID, study_id, hbsag, hepbdna, antihbc, antihbsab, hepb_tx) |>
    dplyr::mutate(across(3:7, forcats::as_factor)) |>
    dplyr::mutate(checked_hepb = forcats::fct_recode(hbsag,
      Yes = "0",
      Yes = "1",
      No = "3"
    )) |>
    dplyr::mutate(hepb_infection = hbsag == 1)
  
  # Process concordance
  # Discordant data is removed from object and moved into attr 
  x <- x |> 
    assess_completeness(
      id_variables = "PATIENT_ID", 
      cols_to_check_for_concordance = c("checked_hepb", "hepb_infection"),
      cols_to_check_for_missing = c("hbsag", "checked_hepb", "hepb_infection"),
      reviewer_key = reviewer_key
  )
  
  return(x)
}

identify_invalid_fib4 <- function(fib4_data, max_duration_fib4) {
  # assert
  checkmate::assertClass(max_duration_fib4, "Duration")

  # Identify valid fib4 that the dates of alt, ast, and plt test
  # are within 1 week of one another
  valid_fib4 <- fib4_data |>
    dplyr::select(
      study_id,
      tidyselect::ends_with("date")
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::ends_with("date"),
      names_to = "lab",
      values_to = "date"
    ) |>
    dplyr::group_by(study_id) |>
    dplyr::summarise(
      min_date = min(date),
      max_date = max(date)
    ) |>
    dplyr::mutate(date_diff = max_date - min_date) |>
    dplyr::filter(date_diff < max_duration_fib4) |>
    dplyr::pull(study_id) 

  fib4_data <- fib4_data |>
    dplyr::mutate(valid_fib4 = study_id %in% valid_fib4)

  return(fib4_data)
}

calculate_fib4 <- function(non_repeat_data, demographics_df, max_duration_fib4) {
  
  # Select information for fib-4 lab data
  fib4_data <- non_repeat_data |>
    select(study_id,
      PATIENT_ID,
      alt = alt_prescription,
      alt_date = alt_prescription_date,
      ast = ast_prescription,
      ast_date = ast_prescription_date,
      plt = plt_prescription,
      plt_date = plt_prescription_date
    ) 
  
  # Drop invalid data
  fib4_data <- fib4_data |> 
    identify_invalid_fib4(max_duration_fib4 = max_duration_fib4) |> 
    mutate(across(
      c("alt", "alt_date", "ast", "ast_date", "plt", "plt_date"),
      ~replace(.x, valid_fib4 == FALSE, NA)
    ))
    

  # Get average date
  fib4_data <- fib4_data |>
    rowwise() |>
    mutate(date = mean.Date(c(alt_date, ast_date, plt_date))) |>
    select(-alt_date, -ast_date, -plt_date) |>
    ungroup()

  # Get dob from demographics_df
  demographics_df <- demographics_df |>
    select(PATIENT_ID, dob)

  fib4_data <- left_join(fib4_data, demographics_df, by = "PATIENT_ID")

  # get age
  fib4_data <- fib4_data |>
    mutate(age = lubridate::year(
      lubridate::as.period(
        lubridate::interval(start = dob, end = date)
      )
    ))

  # calculate fib-4
  fib4_data <- fib4_data |>
    mutate(fib4 = (age * ast) / (plt * sqrt(alt)))
  
  # organize columns
  fib4_data <- fib4_data |> 
    select(PATIENT_ID, study_id, ast, alt, plt, date, age, valid_fib4, fib4)

  # fibrosis assessment
  fib4_boundaries <- c(0, 3.25, Inf)
  fib4_labels <- c(
    "No",
    "Yes"
  )
  fib4_data$advanced_fibrosis <- cut(fib4_data$fib4,
    breaks = fib4_boundaries,
    labels = fib4_labels
  )
  
  return(fib4_data)
}

process_imaging_text <- function(pattern, target, replacement, ignore.case = T) {
  pattern <- paste(pattern, collapse = "|")
  target[grep(pattern, target, ignore.case = ignore.case)] <- replacement

  return(target)
}

process_fibrosure_data <- function(x) {
  fibrosure_boundaries <- c(0, 0.58, 1)
  fibrosure_labels <- c("No", "Yes")

  x <- x |>
    dplyr::filter(!is.na(fibrosure)) |>
    dplyr::mutate(advanced_fibrosis = cut(fibrosure,
      breaks = fibrosure_boundaries,
      labels = fibrosure_labels
    )) |>
    dplyr::rename(fibrosis_score = "fibrosure",
                  date = "fibrosure_date") |> 
    dplyr::mutate(fibrosis_measurement_mod = "fibrosure")
  
  # standardize fibrosis score to chacter for easy combination with other
  # fibrosis data types
  x$fibrosis_score <- x$fibrosis_score |> 
    round(2) |> 
    format(nsmall = 2) |> 
    as.character()
  
  return(x)
}

process_liver_biopsy_data <- function(x) {
  x <- x |> dplyr::filter(!is.na(liver_biopsy))

  liver_bx_levels <- c(
    "No" = "0",
    "No" = "1",
    "No" = "2",
    "Yes" = "3",
    "Yes" = "4"
  )
  
  Fscore_levels <- c(
    "F0" = "0",
    "F1" = "1",
    "F2" = "2",
    "F3" = "3",
    "F4" = "4"
  )

  x$advanced_fibrosis <- x$liver_biopsy |>
    forcats::fct_expand(liver_bx_levels) |>
    forcats::fct_recode(!!!liver_bx_levels)
  
  x$Fscore <- x$liver_biopsy |>
    forcats::fct_expand(Fscore_levels) |>
    forcats::fct_recode(!!!Fscore_levels)

  x <- x |> 
    dplyr::select(-liver_biopsy) |> 
    dplyr::rename(date = liver_biopsy_date) |>
    dplyr::mutate(fibrosis_measurement_mod = "biopsy")
  
  return(x)
}


process_stiffness_results <- function(stiffness_values, cutoff_method) {
  
  checkmate::assertChoice(cutoff_method, c("ziol", "castera"))

  if (cutoff_method == "ziol") {
    cutoffs <- c(2.5, 8.8, 9.6, 14.6, 75)
  } else {
    cutoffs <- c(2.5, 7.0, 9.5, 12.5, 75)
  }
  
  fibrosis_labels <- c(
    "No",
    "No",
    "Yes",
    "Yes"
  )

  fibrosis_stage <- stiffness_values |>
    gsub("kpa", "", x = _, ignore.case = TRUE) |>
    stringr::str_trim() |>
    as.numeric() |> 
    cut(breaks = cutoffs, labels = fibrosis_labels)

  return(fibrosis_stage)
}

process_ultrasound_descriptions <- function(x) {
  normal_regex <- c(
    "normal echogenicity",
    "normal echotexture",
    "no fibrosis",
    "normal RUQ",
    "^hepatic steatosis$",
    "^Mild hepatic steatosis. No focal lesions\\.$",
    "^No ultrasound findings of cirrhosis or focal mass$",
    "^No focal hepatic lesion; no splenomegaly or ascites$",
    "No focal hepatic abnormalities are evident\\.$"
  )
  x <- process_imaging_text(normal_regex, x, "No")

  cirrhosis_regex <- c("cirrhotic liver", "cirrhotic appearing liver")
  x <- process_imaging_text(cirrhosis_regex, x, "Yes")
  
  indeterminate_regex <- c("minimal coarsening of the hepatic parenchyma", 
                           "Fatty liver and hepatomegaly")
  x <- process_imaging_text(indeterminate_regex, x, "Indeterminate")
  

  x <- x |> tidyr::replace_na("Unknown")

  return(x)
}

process_liver_imaging_data <- function(x, cutoff_method) {
  # remove incomplete data and unnecessary columns ####
  x <- x |>
    dplyr::filter(!is.na(liver_imaging_mod) & !is.na(liver_imaging_date)) |>
    dplyr::select(-liver_fibrosis_img_complete)

  # recode imaging modality #####
  img_levels <- c(
    "liver ultrasound" = "0",
    "transient elastrography" = "1",
    "shear wave elastrography" = "2",
    "magnetic resonance elastography" = "3"
  )

  x$liver_imaging_mod <- x$liver_imaging_mod |>
    fct_expand(f = _, img_levels) |>
    fct_recode(.f = _, !!!img_levels)

  # recode F score ####
  fibrosis_stage_levels <- c(
    "No" = "0",
    "No" = "1",
    "No" = "2",
    "No" = "3",
    "No" = "4",
    "No" = "5",
    "Yes" = "6",
    "Yes" = "7",
    "Yes" = "8",
    "Unknown" = "9"
  )

  Fscore_levels <- c(
    "F0" = "0",
    "F0-F1" = "1",
    "F1" = "2",
    "F1-F2" = "3",
    "F2" = "4",
    "F2-F3" = "5",
    "F3" = "6",
    "F3-F4" = "7",
    "F4" = "8",
    "No F score" = "9"
  )

  x$fibrosis_score <- fct_expand(x$fibrosis_score, Fscore_levels)
  x$advanced_fibrosis <- fct_recode(.f = x$fibrosis_score, !!!fibrosis_stage_levels)
  x$fibrosis_score <- fct_recode(.f = x$fibrosis_score, !!!Fscore_levels)


  # process entries with an unknown fibrosis stage #####
  x$advanced_fibrosis <- x$advanced_fibrosis |> tidyr::replace_na("Unknown")
  x <- split(x, x$advanced_fibrosis)

  x$Unknown <- x$Unknown |> 
    dplyr::mutate(fibrosis_score = names(Fscore_levels)[10])

  # use raw imaging values to determine fibrosis stage
  # split data by imaging type
  x$Unknown <- x$Unknown |>
    split(x$Unknown$liver_imaging_mod)

  # process transient elastography stiffness measurements
  x$Unknown$`transient elastrography`$advanced_fibrosis <-
    process_stiffness_results(
      x$Unknown$`transient elastrography`$fibrosis_score_raw,
      cutoff_method = cutoff_method)

  # process ultrasound results
  x$Unknown$`liver ultrasound`$advanced_fibrosis <-
    process_ultrasound_descriptions(x$Unknown$`liver ultrasound`$fibrosis_score_raw)

  x$Unknown <- dplyr::bind_rows(x$Unknown)
  x <- x |> dplyr::bind_rows() 
  
  x <- x |> dplyr::rename(fibrosis_measurement_mod = liver_imaging_mod,
                          date = liver_imaging_date,
                          Fscore = fibrosis_score,
                          fibrosis_score = fibrosis_score_raw)

  return(x)
}

get_fib4_data <- function(non_repeat_data,
                          demographics_df,
                          max_duration_fib4,
                          reviewer_key) {
  
  
  # process fib4 data #####
  fib4_data <- calculate_fib4(
    non_repeat_data,
    demographics_df,
    max_duration_fib4
  )
  
  fib4_data <- fib4_data |> 
    assess_completeness(
      id_variables = "PATIENT_ID",
      reviewer_key = reviewer_key
    )
  
  return(fib4_data)
}

  
  
get_fibrosis_data <- function(non_repeat_data,
                              liver_imaging_data,
                              demographics_df,
                              max_duration_fib4) {
  # process fib4 data #####
  fib4_data <- calculate_fib4(
    non_repeat_data,
    demographics_df,
    max_duration_fib4
  )

  # process fibrosure data  #####
  fibrosure_data <- non_repeat_data |>
    dplyr::select(PATIENT_ID, study_id, fibrosure, fibrosure_date) |>
    process_fibrosure_data()

  # process liver biopsy data  #####
  liver_bx_data <- non_repeat_data |>
    dplyr::select(PATIENT_ID, study_id, liver_biopsy, liver_biopsy_date) |>
    process_liver_biopsy_data()

  # process liver imaging data  #####
  liver_imaging_data <- liver_imaging_data |>
    process_liver_imaging_data(cutoff_method = "ziol")
  
  # combine data #####'
  fibrosis_data <- 
    bind_rows(fib4_data, fibrosure_data, liver_bx_data, liver_imaging_data) |>
    tidyr::replace_na(list(
                      advanced_fibrosis = "Unknown",
                      Fscore = "No F Score")) 
  
  return(fibrosis_data)
}

check_advanced_fibrosis <- function(fibrosis_data, antiviral_data, reviewer_key) {
  
  df <- left_join(fibrosis_data, antiviral_data, by = c("PATIENT_ID", "study_id"))
  
  # Remove any labs after prescription date 
  df <- df |> filter(!(date > prescription_date))
  
  # Check for any evidence of advanced fibrosis
  df <- df |> 
    group_by(PATIENT_ID, study_id) |> 
    summarise(advanced_fibrosis = case_when(
      any(advanced_fibrosis == "Yes") ~ "Yes",
      any(advanced_fibrosis == "Unknown") ~ "Unknown",
      .default = "No"
    )) |> 
    ungroup() |> 
    select(PATIENT_ID, study_id, advanced_fibrosis)
                
  # Process concordance
  # Discordant data is removed from object and moved into attr 
  df <- df |> 
    assess_completeness(
      id_variables = c("PATIENT_ID"),
      cols_to_check_for_concordance = "advanced_fibrosis",
      reviewer_key = reviewer_key
  )
  
  return(df)
  
}
