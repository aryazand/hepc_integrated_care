create_proportional_hazards_model <- function(survival_table, endpoint, reference) {
  
  survival_table <- survival_table |>
    mutate(treatment_strategy = fct_relevel(treatment_strategy, reference))
    
  cox.model <- coxph(Surv(tstart, tstop, eval(parse(text = endpoint))) ~ treatment_strategy, 
                     id = PATIENT_ID, 
                     data = survival_table)
  
  return(cox.model)
}

