create_survival_table <- function(chronic_hepc_intervals,
                                  cohortIDs,
                                  consults_df,
                                  antiviral_data,
                                  end_date_col, 
                                  end_marker_col,
                                  success_marker, 
                                  censor_marker, 
                                  law_change_date,
                                  endpoint_rename) {

  # Pick relevant columns from chronic_hepc_intervals
  df_ind <- chronic_hepc_intervals |>
    filter(PATIENT_ID %in% cohortIDs) |>
    filter(!is.na(start_date)) |>
    select(PATIENT_ID, start_date, all_of(c(end_date = end_date_col, end_marker = end_marker_col))) |>
    distinct()

  # Define time and endpoints
  df_ind <- df_ind |>
    mutate(time = end_date - start_date) |>
    filter(!is.na(time)) |>
    mutate(endpoint = fct_collapse(end_marker, 
      "0" = censor_marker, 
      "1" = success_marker
    )) |> 
    mutate(endpoint = as.numeric(as.character(endpoint)))

  # Create data frame of time-dependent covariate called action. Action will have
  # 3 factors: antiviral_prescription, hepatology_consult, and no_action

  consults_df2 <- consults_df |>
    select(-study_id) |>
    distinct() |>
    filter(referring_service %in% c("MAT Clinic", "iACCESS") &
      consulted_service == "Hepatology") |>
    inner_join(df_ind |> select(PATIENT_ID, start_date), by = "PATIENT_ID") |>
    mutate(day = consult_date - start_date) |>
    mutate(day = replace(day, day < 0, 0)) |>
    mutate(action = "hepatology_consult") |>
    select(PATIENT_ID, action, day, date = consult_date)

  antiviral_data2 <- antiviral_data |>
    select(-study_id) |>
    distinct() |>
    filter(prescription_clinic %in% c("MAT", "iACCESS")) |>
    inner_join(df_ind |> select(PATIENT_ID, start_date), by = "PATIENT_ID") |>
    mutate(day = prescription_date - start_date) |>
    mutate(day = replace(day, day < 0, 0)) |>
    mutate(action = "antiviral_prescription") |>
    select(PATIENT_ID, action, day, date = prescription_date)

  action_df <- bind_rows(consults_df2, antiviral_data2)

  # Include law change as another time-dependent covariate
  policy_change_df <- df_ind |>
    select(PATIENT_ID, start_date, end_date) |>
    filter(law_change_date < end_date) |>
    mutate(day = law_change_date - start_date) |>
    mutate(day = replace(day, day < 0, 0)) |>
    mutate(medicaid_policy_status = "relaxed") |>
    select(PATIENT_ID, medicaid_policy_status, day) |>
    mutate(date = law_change_date)

  df_dep <- bind_rows(action_df, policy_change_df) |>
    arrange(PATIENT_ID, day) |>
    group_by(PATIENT_ID) |>
    fill(medicaid_policy_status, .direction = "down") |>
    mutate(medicaid_policy_status = replace_na(medicaid_policy_status, "strict")) |>
    fill(action, .direction = "down") |>
    mutate(action = replace_na(action, "no_action")) |>
    ungroup()


  # tmerge
  df0 <- survival::tmerge(df_ind, df_ind, id = PATIENT_ID, endpoint = event(time, endpoint))
  df <- survival::tmerge(df0, df_dep,
    id = PATIENT_ID,
    action = tdc(day, action),
    medicaid_policy_status = tdc(day, medicaid_policy_status)
  )
  df$action <- replace_na(df$action, "no_action")

  # Adjust Variable Factors
  action_levels <- c(
    "usual care" = "no_action",
    "prescribe antiviral" = "antiviral_prescription",
    "consult hepatology" = "hepatology_consult"
  )
  df$action <- factor(df$action) |>
    fct_recode(!!!action_levels) |>
    fct_relevel("usual care")
  df$medicaid_policy_status <- replace_na(df$medicaid_policy_status, "strict")
  df$medicaid_policy_status <- factor(df$medicaid_policy_status) |> fct_relevel("strict")
  df$endpoint_date <- NULL
  df$time <- NULL
  
  # adjust column titles
  df <- df |> rename(treatment_strategy = action)
  colnames(df)[colnames(df) == "endpoint"] <- endpoint_rename

  return(df)
}
