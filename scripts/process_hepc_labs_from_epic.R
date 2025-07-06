process_hepc_labs_from_epic <- function(hepc_lab_data, patient_key) {
  # Process hep c lab data

  # Select columns
  # Anonymize mrns to patient_id
  # Convert dates into data format
  hepc_lab_data <- hepc_lab_data |>
    select(
      mrn = PAT_MRN_ID,
      encounter_date = `Encounter Date`,
      order_date = `Encounter Date`,
      ORDER_PROC_ID,
      lab_code = PROC_CODE,
      lab_date = `Specimen Taken Date`,
      line = LINE,
      lab_value = ORD_VALUE,
      provider_dept = `Provider Login Dept`
    ) |>
    left_join(patient_key, by = "mrn") |>
    arrange(PATIENT_ID, lab_date, lab_code, line) |>
    mutate_if(is.POSIXct, as.Date)


  # Next we will standardize labs to either Hep C antibody or Hep C PCR as well
  # as standardize results of those labs. Some lab results are difficult to
  # standardize due to either uncommon result values and that some labs provide
  # multiple results. Those non-standarizable labs will be given a value of
  # "unknown"

  lab_levels <- c(
    "Ab" = "CCLAB1248",
    "Ab" = "CCLAB1265",
    "PCR" = "CCLAB446",
    "Ab" = "CCLAB724",
    "PCR" = "LAB2465",
    "Ab" = "LAB3298",
    "Ab" = "LAB5884",
    "Ab" = "LAB627",
    "PCR" = "LAB6729",
    "PCR" = "LAB8185",
    "Ab" = "VBCH83"
  )

  line_values <- as.character(unique(hepc_lab_data$line))

  hepc_lab_data <- hepc_lab_data |>
    mutate(lab_type = factor(lab_code) |> fct_recode(!!!lab_levels)) |>
    pivot_wider(
      id_cols = c(PATIENT_ID, order_date, lab_date, lab_code, ORDER_PROC_ID, provider_dept, lab_type),
      names_from = line, values_from = lab_value
    )

  hepc_lab_data <- hepc_lab_data |>
    mutate_at(line_values, ~ replace(.x, grepl("^\\d", .x), "Positive")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^Reactive", .x, ignore.case = T), "Positive")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^positive", .x, ignore.case = T), "Positive")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^Detected", .x, ignore.case = T), "Positive")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^>", .x, ignore.case = T), "Positive")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^Negative", .x, ignore.case = T), "Negative")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^Undetected", .x, ignore.case = T), "Negative")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^Not Det", .x, ignore.case = T), "Negative")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^Non[-\\s]Det", .x, ignore.case = T), "Negative")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^Non-{0,1}Reac", .x, ignore.case = T), "Negative")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^Non Reac", .x, ignore.case = T), "Negative")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^Neg$", .x, ignore.case = T), "Negative")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^HCV RNA not detected.$", .x, ignore.case = T), "Negative")) |>
    mutate_at(line_values, ~ replace(.x, grepl("^<", .x, ignore.case = T), "Negative")) |>
    rowwise() |>
    mutate(lab_value = factor(case_when(
      "Positive" %in% c(`1`, `2`, `3`, `4`) & !("Negative" %in% c(`1`, `2`, `3`, `4`)) ~ "Positive",
      "Negative" %in% c(`1`, `2`, `3`, `4`) & !("Positive" %in% c(`1`, `2`, `3`, `4`)) ~ "Negative",
      .default = "Indeterminate"
    ), levels = c("Positive", "Negative", "Indeterminate")))

  return(hepc_lab_data)
}
