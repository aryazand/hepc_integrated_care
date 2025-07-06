create_chronic_hepc_interval <- 
  function(hepc_pcr_results.manual,
           hepc_lab_results, 
           antiviral_data, 
           death_dates, 
           consults_data, 
           mat_visits_data,
           reviewer_key,
           study_period) {
  
  ##############################################################################
  # DEFINE START AND END DATES FOR EACH PATIENT'S HEP C INFECTION DURING THE
  # STUDY PERIOD
  #
  # START DATE is when "MAT Clinic is aware of patients hep c diagnosis", criteria would be: 
  # 1. Earliest of: 
  #     - A MAT visit after positive hepatitis C PCR
  #     - A hepatology consult or antivrial rx for Hepatitis C 
  #     - A positive hepatitis C PCR ordered by MAT clinic 
  # 2. If above start dates are prior to study period, then shift to start of 
  # study period
  #
  # END DATE is defined by one of the following: 
  # 1. SVR 
  # 2. death 
  # 3. study end date
  # 4. More than 1 year since last MAT visit   
  ##############################################################################
  
  # Helper functions 
  min_custom <- function(...) {
      x <- suppressWarnings(min(..., na.rm = T))
      x[x == Inf] <- NA
      return(x)
  }  
  
  which.min_custom <- function(...) {
    if(all(is.na(...))) {
      return(NA)
    } else {
      which.min(...)  
    }
  }  
    
  ############
  # Step 1: Define start date 
  ############
  
  # Criteria 1: First MAT visit after positive hep c pcr 
  # Identify first positive HCV PCR values
  first_pos_pcr <- hepc_pcr_results.manual |>
    group_by(PATIENT_ID, study_id) |>
    filter(hepc_pcr_positive == TRUE) |> 
    slice_min(hepc_pcr_date) |> 
    ungroup() |> 
    rename(mat_clinic_ordered_pcr = hepc_pcr_clinic,
           first_pos_pcr_date = hepc_pcr_date) |> 
    select(-log_pcr_value, -first_hepc_pcr, -hepc_pcr_positive) 

  # First visit after positive HCV PCR
  first_mat_after_pos_pcr <- first_pos_pcr |> 
    left_join(mat_visits_data, relationship = "many-to-many") |> 
    filter(visit_date >= first_pos_pcr_date) |> 
    group_by(PATIENT_ID, study_id) |>
    slice_min(visit_date) |> 
    ungroup() |> 
    distinct() |>
    rename(startdate.mat_after_pos_pcr = visit_date) |>
    select(-mat_clinic_ordered_pcr, -first_pos_pcr_date)
  
  # Criteria 2: Hep C PCR ordered by MAT 
  pos_pcr_ordered_by_mat <- first_pos_pcr |> 
    filter(mat_clinic_ordered_pcr == "Y") |>
    rename(startdate.mat_ordered_pcr = first_pos_pcr_date) |> 
    select(-mat_clinic_ordered_pcr)
    
  # Criteria 3: Consult or antiviral order
  rx_ordered_by_mat <- antiviral_data |>
    filter(prescription_clinic %in% c("MAT", "iACCESS")) |> 
    group_by(study_id) |> 
    filter(prescription_date == min(prescription_date, na.rm=T)) |> 
    ungroup() |> 
    select(PATIENT_ID, study_id, startdate.prescription_by_mat = prescription_date)
  
  # Identify consult order 
  consult_ordered_by_mat <- consults_data |>
    filter(referring_service %in% c("MAT Clinic", "iACCESS")) |> 
    group_by(study_id) |> 
    #filter(consult_date == min(consult_date, na.rm=T)) |> 
    slice_min(consult_date, n = 1) |>
    ungroup() |> 
    select(PATIENT_ID, study_id, startdate.hepatology_consult_by_mat = consult_date) |> 
    distinct()
  
  # Define start date
  start_date_df <- first_pos_pcr |>
    full_join(first_mat_after_pos_pcr) |>
    full_join(pos_pcr_ordered_by_mat) |>
    full_join(rx_ordered_by_mat) |>
    full_join(consult_ordered_by_mat) |> 
    rowwise() |> 
    mutate(start_date = min_custom(c_across(starts_with("startdate."))),
           start_marker = factor(which.min_custom(c_across(starts_with("startdate."))))) |> 
    ungroup()
 
  # if start date prior to study start date, then shift up to study start date 
  study_start_date <- int_start(study_period) |> as_date()
  start_date_df$start_date[start_date_df$start_date < study_start_date] <- study_start_date

  # If start_date is after antiviral was started, then exclude that start_date
  start_date_df <- start_date_df |>
    left_join(select(antiviral_data, PATIENT_ID, study_id, antiviral_start_date)) |> 
    mutate(start_date = replace(
      start_date, 
      start_date > antiviral_start_date, 
      NA))
  
  ############
  # Step 2: Define potential end dates
  ############
  
  # Process epic pull hepc pcr data
  hepc_pcr1 <- hepc_lab_results |> 
    filter(lab_type == "PCR") |>
    select(PATIENT_ID, pcr_date = lab_date, lab_value) |> 
    left_join(reviewer_key, relationship = "many-to-many") |> 
    select(-mrn, -reviewer)
  
  hepc_pcr2 <- hepc_pcr_results.manual |> 
    select(PATIENT_ID, study_id, pcr_date = hepc_pcr_date, lab_value = hepc_pcr_positive) |> 
    mutate(lab_value = factor(lab_value)) |> 
    mutate(lab_value = fct_recode(lab_value,
      "Negative" = "FALSE", 
      "Positive" = "TRUE"
    ))
  
  hepc_pcr <- bind_rows(hepc_pcr1, hepc_pcr2) |> distinct()
  
  # Grab all patients that completed 
  antiviral_completion_data <- antiviral_data |>
    filter(antiviral_completed == "Yes") |> 
    select(PATIENT_ID, study_id, rx_completion_date = date_completion)
  
  # Get SVR date 
  lab_after_rx_completion <- antiviral_completion_data |> 
    left_join(hepc_pcr, relationship = "many-to-many") |> 
    group_by(study_id, lab_value) |> 
    filter(pcr_date >= rx_completion_date + weeks(12)) |> 
    slice_min(pcr_date) |> 
    ungroup() |> 
    pivot_wider(
      id_cols = c(PATIENT_ID, study_id),
      names_from = lab_value, 
      names_prefix = "enddate.pcr_",
      values_from = pcr_date
    ) |> 
    rename(
      enddate.SVR = enddate.pcr_Negative,
      enddate.failed_therapy = enddate.pcr_Positive
    )  

  
  # process death_dates
  death_dates2 <- death_dates |>
    rename(enddate.death = death_date)
  
  # process last mat_visit
  # only include patients in the start_date_df
  # only consider last_visits patients deemed lost to follow-up prior to 2023-05-31, the day the mat encounters were collected
  last_visit <- mat_visits_data |>
    right_join(select(reviewer_key, PATIENT_ID, study_id), relationship = "many-to-many") |> 
    group_by(PATIENT_ID, study_id) |> 
    slice_max(visit_date, with_ties = F, na_rm = T) |>
    ungroup() |>
    rename(last_visit = visit_date) |> 
    mutate(enddate.lost_to_followup = last_visit + years(1)) |>
    mutate(enddate.lost_to_followup = replace(enddate.lost_to_followup, enddate.lost_to_followup > date("2023-05-31"), NA)) |>
    select(-last_visit)
    
  
  # combine data
  end_date_df <- antiviral_completion_data |> 
    full_join(lab_after_rx_completion) |>
    full_join(death_dates2) |> 
    full_join(last_visit)
  
  ############
  # Step 3: Define end date by SVR 
  ############
  
  df <- full_join(start_date_df, end_date_df) |> 
    mutate(enddate.study_end = as_date(int_end(study_period))) |> 
    rowwise() |> 
    mutate(end_date.svr = min_custom(c_across(starts_with("enddate."))),
           end_marker.svr = factor(which.min_custom(c_across(starts_with("enddate."))))) |> 
    ungroup()
  
  ############
  # Step 4: Define end date by antiviral completion
  ############
  
  df <- df |> 
    rowwise() |> 
    mutate(end_date.rx_completion = min_custom(c(rx_completion_date, enddate.death, enddate.lost_to_followup, enddate.study_end)),
           end_marker.rx_completion = factor(which.min_custom(c(rx_completion_date, enddate.death, enddate.lost_to_followup, enddate.study_end)))) |> 
    ungroup()
  
  
  ############
  # Step 5: If not start_date, then no end_date
  ############
  
  df <- df |>
    mutate(
      end_date.svr = replace(end_date.svr, is.na(start_date), NA),
      end_marker.svr = replace(end_marker.svr, is.na(start_date), NA),
      end_date.rx_completion = replace(end_date.rx_completion, is.na(start_date), NA),
      end_marker.rx_completion = replace(end_marker.rx_completion, is.na(start_date), NA)
    ) 
  
  ############
  # Step 6: Assess Completeness
  ############
  cols_to_check_for_concordance <- c("start_date", "start_marker", "end_date.svr", "end_marker.svr", "end_date.rx_completion", "end_marker.rx_completion")
  cols_to_check_for_missing <- c("start_date", "start_marker", "end_date.svr", "end_marker.svr", "end_date.rx_completion", "end_marker.rx_completion")
  df <- df |> 
    assess_completeness(
      id_variables = "PATIENT_ID",
      cols_to_check_for_concordance = cols_to_check_for_concordance,
      cols_to_check_for_missing = cols_to_check_for_missing,
      reviewer_key = reviewer_key
  )

  return(df)
}
