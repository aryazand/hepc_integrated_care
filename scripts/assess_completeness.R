helper_assess_concordance <- function(df, id_variables) {
  
  # Split df by reviewer
  df.split <- df |> 
    group_by(PATIENT_ID) |> 
    mutate(reviewer = case_when(
      study_id == min(study_id) ~ "1",
      study_id == min(study_id) ~ "2"
    )) |>
    select(-study_id) |>
    group_by(reviewer) |>
    group_split() |> 
    map(select, -reviewer)
  
  # Compare two reviewer results
  x <- arsenal::comparedf(df.split[[1]], df.split[[2]], by = id_variables)
  
  return(x)
  
} 

assess_concordance <- function(df0, id_variables, variables_to_check = NULL, reviewer_key) {
  
  if(is.null(variables_to_check)) {
    # If variables to check is null, then select all variables
    variables_to_check <- colnames(df0)[!(colnames(df0) %in% c("PATIENT_ID", "study_id"))]
  } 
  
  # Select variables that are needed for concordance check 
  df.onlycheckvariables <- df0 |> select(PATIENT_ID, study_id, all_of(id_variables), all_of(variables_to_check))
  
  # Make sure all study ids are represented in the data 
  df.onlycheckvariables <- reviewer_key |> select(PATIENT_ID, study_id) |> right_join(df.onlycheckvariables)

  # Create an object that contains concordance data
  concordance_df <- df.onlycheckvariables |> 
    helper_assess_concordance(id_variables) |>
    summary()
  
  # Mark discordance, missing, and completeness 
  df.onlycheckvariables <- df.onlycheckvariables |> 
    mutate(discordant = PATIENT_ID %in% concordance_df$diffs.table$PATIENT_ID) |> 
    select(PATIENT_ID, study_id, discordant)
  
  # Add discordance, missing, and completeness data to original df
  df0 <- left_join(df0, df.onlycheckvariables, relationship = "many-to-many")
  
  # Add discordance summary to attributes
  attr(df0, "concordance_summary") <- concordance_df
  
  return(df0)
}

assess_missing <- function(df0, cols_to_check_for_missing = NULL) {

  if(is.null(cols_to_check_for_missing)) {
    # If variables to check is null, then select all variables
    cols_to_check_for_missing <- colnames(df0)[!(colnames(df0) %in% c("PATIENT_ID", "study_id"))]
  } 
  
  df0 <- df0 |> 
    mutate(missing = if_any(all_of(cols_to_check_for_missing), ~ is.na(.x) | is.null(.x)))
  
  return(df0)
}

assess_completeness <- function(df0, 
                                id_variables, 
                                cols_to_check_for_concordance = NULL, 
                                cols_to_check_for_missing = NULL, 
                                reviewer_key) {
  
  df0 <- assess_concordance(df0, id_variables, cols_to_check_for_concordance, reviewer_key)
  df0 <- assess_missing(df0, cols_to_check_for_missing)
  df0 <- df0 |> mutate(complete = discordant == FALSE & missing == FALSE)
  
  return(df0)
}
