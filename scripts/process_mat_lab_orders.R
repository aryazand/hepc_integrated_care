process_mat_lab_orders <- function(mat_encounters_with_orders, hepc_lab_history, patient_key) {
  
  # Select columns and anonymize by replacing mrn with patient_id
  mat_encounters_with_orders <- mat_encounters_with_orders |>
    select(mrn = MRN, 
           visit_date = Appointment.Date, 
           visit_type = Appointment.Type,
           encounter_labs = Encounter.Labs...Labs.Ordered.This.Encounter) |>
    left_join(patient_key, by = "mrn") |>
    select(-mrn) |> 
    as_tibble()

  #######################
  # For each mat visit determine:
  # 1. which hep c labs were ordered
  # 2. based on lab history
  #####################
  
  # Custom functions
  min_custom <- function(...) {
    x <- suppressWarnings(min(..., na.rm = T))
    x[x == Inf] <- NA
    return(x)
  }
  
  # Get last lab value using survival::neardate function
  get_last_lab_info <- function(id, 
                                index_dates, reference_dates,
                                index_data, reference_data,
                                value_to_select) {
    
    reference_data[[value_to_select]][
      neardate(index_data[[id]],
        reference_data[[id]],
        index_data[[index_dates]],
        reference_data[[reference_dates]],
        best = "prior"
      )
    ]
    
  }
  
  # Select Hep C ab lab history
  hepc_ab_history <- hepc_lab_history |> filter(lab_type == "Ab")

  first_pos_hepc_ab <- hepc_ab_history |>
    group_by(PATIENT_ID) |>
    summarise(first_pos_ab = min_custom(lab_date[lab_value == "Positive"]))

  # Select Hep C PCR history 
  hepc_pcr_history <- hepc_lab_history |> filter(lab_type == "PCR")

  
  mat_encounters_with_orders <- mat_encounters_with_orders |>
    mutate(last_ab = get_last_lab_info("PATIENT_ID", "visit_date", "lab_date", mat_encounters_with_orders, hepc_ab_history, "lab_value")) |>
    mutate(last_ab_date = get_last_lab_info("PATIENT_ID", "visit_date", "lab_date", mat_encounters_with_orders, hepc_ab_history, "lab_date")) |>
    mutate(last_pcr = get_last_lab_info("PATIENT_ID", "visit_date", "lab_date", mat_encounters_with_orders, hepc_pcr_history, "lab_value")) |>
    mutate(last_pcr_date = get_last_lab_info("PATIENT_ID", "visit_date", "lab_date", mat_encounters_with_orders, hepc_pcr_history, "lab_date")) |>
    left_join(first_pos_hepc_ab, by = "PATIENT_ID") |>
    mutate(
      ab_ordered = grepl("HEPATITIS C ANTIBODY", encounter_labs),
      pcr_ordered = grepl("HEPATITIS C VIRUS; QUANTITATIVE PCR", encounter_labs),
      any_hcv_lab_ordered = grepl("HCV|Hepatitis C", encounter_labs, ignore.case = T)
    ) |>
    group_by(PATIENT_ID) |>
    mutate(visit_num = seq(n())) |>
    ungroup() |>
    mutate(ab_indicated = last_ab %in% c(NA, "Unknown") & is.na(last_pcr)) |>
    mutate(pcr_indicated = last_ab == "Positive" & (last_pcr %in% c(NA, "Unknown"))) |>
    mutate(pcr_indicated = replace_na(pcr_indicated, FALSE)) |>
    tibble()

  return(mat_encounters_with_orders)
}
