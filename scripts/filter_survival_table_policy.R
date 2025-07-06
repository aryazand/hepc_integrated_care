filter_survival_table_policy <- function(survival_table,
                                         policy) {
  
  survival_table <- survival_table |> 
    group_by(PATIENT_ID) |>
      arrange(tstart) |> 
      mutate(step = seq(n())) |> 
      filter(medicaid_policy_status == policy) |> 
      mutate(start_shift = min(tstart)) |>
      mutate(tstart = tstart - start_shift,
             tstop = tstop - start_shift)
  
  return(survival_table)
}