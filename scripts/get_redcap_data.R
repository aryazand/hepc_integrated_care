download_redcap_data <- function(url, token, col_types) {
  # Download Data
  redcap_data <- REDCapR::redcap_read(
    redcap_uri = url, token = token,
    col_types = col_types,
    guess_type = FALSE
  )

  # Extract tibble
  redcap_data <- redcap_data$data |> tibble::as_tibble()

  return(redcap_data)
}

download_redcap_metadata <- function(url, token) {
  redcap_metadata <- REDCapR::redcap_metadata_read(
    redcap_uri = url,
    token = token
  )
  redcap_metadata <- redcap_metadata$data

  return(redcap_metadata)
}

process_redcap_data <- function(redcap_data, field_data) {
  
  non_repeat_group_name = "non_repeat_data"
  
  # Group redcap data by "non repeat data" and repeat data forms
  redcap_data <- redcap_data |>
    dplyr::mutate(
      redcap_repeat_instrument =
        tidyr::replace_na(redcap_repeat_instrument, non_repeat_group_name)
    )
  redcap_data <- redcap_data |> dplyr::group_by(redcap_repeat_instrument)
  group_names <- dplyr::group_data(redcap_data)$redcap_repeat_instrument

  # Separate out groups into individual data frames
  redcap_data <- redcap_data |> group_split()
  names(redcap_data) <- group_names

  #
  # Select relevant columns for each data frame
  #
  
  select_cols_fun <- function(df, field_data = field_data) {
    
    df_name = unique(df$redcap_repeat_instrument)
    
    # Define which columns to select 
    if(df_name == non_repeat_group_name) {
      cols_to_select <- field_data |> 
        dplyr::filter(!(form_name %in% group_names))
    } else { 
      cols_to_select <- field_data |> 
        dplyr::filter(form_name == df_name) 
    }
    
    cols_to_select <- cols_to_select |> 
      dplyr::select(field_name_2) |>
      unlist()
    
    # Make sure study_id is always select 
    cols_to_select <- c("study_id", cols_to_select) |> unique()
    
    # Select columns
    df <- df |> dplyr::select(all_of(cols_to_select)) 
    
    return(df)
  }
  
  # apply column selection function 
  redcap_data <- purrr::map(redcap_data, 
                                   .f = select_cols_fun, 
                                   field_data = field_data)
  
  return(redcap_data)
}

get_redcap_data <- function(url, token, reviewer_key) {
  # Download redcap metadata
  redcap_metadata <- download_redcap_metadata(url, token)

  # Deduce column types for redcap field metadata
  field_data <- metadata_to_coltypes(redcap_metadata)
  
  # Manual adjustment
  field_data$col_type[field_data$field_name_2 == "mrn"] <- "character" 
  
  # Create col_types object  
  col_types <- cols(!!!setNames(field_data$col_type, field_data$field_name_2))

  redcap_data <- 
    download_redcap_data(url, token, col_types) |>
    process_redcap_data(redcap_data = _, field_data = field_data)
 
  # Replace MRN with Patient_ID
  reviewer_key <- reviewer_key |> select(-reviewer, -mrn)
  
  redcap_data <- map(redcap_data, left_join, y = reviewer_key, by = "study_id")
  redcap_data$non_repeat_data <- redcap_data$non_repeat_data |> 
    select(-mrn)
  
  redcap_data$metadata <- redcap_metadata

  return(redcap_data)
}

metadata_to_coltypes <- function(redcap_metadata) {
  ##### Get field information #####

  x <- redcap_metadata |>
    dplyr::rename(
      validation_type = text_validation_type_or_show_slider_number,
      validation_max = text_validation_max,
      validation_min = text_validation_min
    ) |>
    dplyr::mutate(col_type = paste(field_type, validation_type, sep = "_")) |>
    dplyr::mutate(col_type = sub("_NA", "", col_type))

  #### Rename field types to R data types #####

  # Dictionary for assigning data types
  dic <- setNames(
    c(
      "character", "double", "logical", "date", "factor",
      "double", "logical", "character"
    ),
    c(
      "text", "text_number", "yesno", "text_date_mdy", "radio",
      "text_integer", "checkbox", "notes"
    )
  )

  # Verify there isn't any data types not accounted for in dictionary
  x <- x |> verify(col_type %in% names(dic))

  # Assign data type
  x$col_type <- dic[x$col_type]


  ##### Handle Checkbox data #####

  # Helper function to expand checkbox data types to
  # match the way redcap stores the data results. Redcap stores each
  # checkbox option as a new logical variable
  # expand_checkbox <- function(field_name, select_choices_or_calculations) {
  #   n <- select_choices_or_calculations |> str_count(fixed("|"))
  # 
  #   field_name_2 <- paste(field_name, seq(0, n), sep = "___")
  #   col_type <- "logical"
  #   df <- tibble(field_name, field_name_2, col_type)
  #   return(df)
  # }
  # 
  expand_checkbox <- function(df) {
    n <- df$select_choices_or_calculations |> str_count(fixed("|"))
    field_name_2 = paste(df$field_name, seq(0, n), sep = "___")
    
    df <- tibble::tibble(field_name = df$field_name, 
                         field_name_2) |> 
      dplyr::left_join(df, by = "field_name") |> 
      dplyr::mutate(col_type = "logical")
    
    return(df)
  }

  # Apply helper function to checkbox data types
  checkbox_df <- x |> 
    dplyr::filter(field_type == "checkbox") |> 
    group_by(field_name) |> 
    group_map(~ expand_checkbox(.x), .keep = TRUE) |> 
    bind_rows() |> 
    select(form_name, field_name, field_name_2, col_type)

  ##### Handle Form Names #####

  # At the end of each Redcap form there is a radio variable that
  # is defines whether the form is complete. This is also included in the
  # the redcap data download

  formname_df <- x |>
    dplyr::select(form_name) |>
    unique() |>
    dplyr::mutate(field_name_2 = paste0(form_name, "_complete")) |>
    dplyr::mutate(col_type = "factor")


  ######## Bind data #######
  
  x <- x |> 
    dplyr::filter(field_type != "checkbox") |>
    dplyr::mutate(field_name_2 = field_name) |> 
    dplyr::select(form_name, field_name, field_name_2, col_type)

  x <- dplyr::bind_rows(x, checkbox_df, formname_df) 
  
  return(x)
}
