#' Fetch Data from the IRW Database
#'
#' Retrieves one or more datasets from the Item Response Warehouse (IRW) database by their names.
#' The datasets are fetched as data frames (tibbles) and can be accessed programmatically for analysis.
#'
#' @param name A character vector specifying one or more dataset names to fetch.
#' @return If a single dataset is fetched, returns a tibble. If multiple datasets are fetched, 
#'         returns a named list of tibbles or error messages for datasets that failed to load.
#' @examples
#' \dontrun{
#'   # Fetch a single dataset
#'   df <- irw_fetch("example_dataset")
#'   print(df)
#'
#'   # Fetch multiple datasets
#'   datasets <- irw_fetch(c("dataset1", "dataset2"))
#'   print(names(datasets))  # Outputs: "dataset1" and "dataset2"
#' }
#' @export
irw_fetch <- function(name) {
  # Helper function to fetch a single dataset
  fetch_single_data <- function(dataset_name) {
    tryCatch(
      {
        table <- suppressMessages(.fetch_redivis_table(dataset_name)) 
        table$to_tibble()  
      },
      error = function(e) {
        error_message <- paste("Error fetching dataset", shQuote(dataset_name), ":", e$message)
        message(error_message)  # print error immediately
        return(error_message)  # save error message in list output
      }
    )
  }
  
  # Check if fetching a single or multiple datasets
  if (length(name) == 1) {
    return(fetch_single_data(name))  # Return a single tibble or error message
  } else {
    dataset_list <- lapply(name, fetch_single_data)
    names(dataset_list) <- name
    return(dataset_list)  # Return a named list of tibbles/errors
  }
}


#' Retrieve IRW Metadata Table
#'
#' Fetches the metadata table from Redivis and returns it as a tibble.
#' Automatically checks for updates and refreshes only when needed.
#'
#' @return A tibble containing metadata information.
#' @export
irw_metadata <- function() {
  return(.fetch_metadata_table())  
}


#' Filter Tables by Criteria
#'
#' Filters the IRW database metadata to return the names of tables that match the specified criteria.
#' Criteria include ranges for numeric attributes (e.g., `id_count`, `density`) and the presence of specific columns 
#' (e.g., `date`, `response time`, or `rater`).
#'
#' @param id_count A numeric vector of two values specifying the range for the number of unique IDs 
#'        (e.g., `c(100, 1000)`). Default is `NULL`, which ignores this criterion.
#' @param item_count A numeric vector of two values specifying the range for the number of unique items. 
#'        Default is `NULL`.
#' @param resp_count A numeric vector of two values specifying the range for the total number of responses. 
#'        Default is `NULL`.
#' @param density A numeric vector of two values specifying the range for density (between 0 and 1). 
#'        Default is `NULL`.
#' @param has_date Logical; `TRUE` to include tables with a `date` column, `FALSE` to exclude them, 
#'        or `NULL` to ignore this criterion. Default is `NULL`.
#' @param has_rt Logical; `TRUE` to include tables with a `rt` column, `FALSE` to exclude them, 
#'        or `NULL` to ignore this criterion. Default is `NULL`.
#' @param has_rater Logical; `TRUE` to include tables with a `rater` column, `FALSE` to exclude them, 
#'        or `NULL` to ignore this criterion. Default is `NULL`.
#' @return A character vector of table names that match the criteria. If no tables match, it prints a message 
#'         and returns an empty character vector.
#' @examples
#' \dontrun{
#'   irw_filter(id_count = c(100, 1000), density = c(0.1, 0.5), has_date = TRUE)
#' }
#' @export
irw_filter <- function(id_count = NULL, item_count = NULL, resp_count = NULL, density = NULL,
                          has_date = NULL, has_rt = NULL, has_rater = NULL) {
  
  # Helper function to check if value is within range (inclusive)
  is_within_range <- function(value, range) {
    if (is.null(range)) return(TRUE)
    value >= range[1] & value <= range[2] & !is.na(value)
  }
  
  # Validate ranges
  validate_range <- function(range, name) {
    if (!is.null(range) && (!is.numeric(range) || length(range) != 2)) {
      stop(sprintf("'%s' must be a numeric vector of length 2.", name))
    }
  }
  validate_range(id_count, "id_count")
  validate_range(item_count, "item_count")
  validate_range(resp_count, "resp_count")
  validate_range(density, "density")
  
  # Check required columns
  required_columns <- c("id_count", "item_count", "resp_count", "density", "has_date", "has_rt", "has_rater")
  missing_columns <- setdiff(required_columns, names(metadata))
  if (length(missing_columns) > 0) {
    stop(sprintf("The following required columns are missing from metadata: %s", paste(missing_columns, collapse = ", ")))
  }
  
  # Apply numeric filters
  numeric_filters <- list(id_count = id_count, item_count = item_count, resp_count = resp_count, density = density)
  for (filter_name in names(numeric_filters)) {
    range <- numeric_filters[[filter_name]]
    if (!is.null(range)) {
      metadata <- metadata[is_within_range(metadata[[filter_name]], range), ]
    }
  }
  
  # Apply column presence filters
  if (!is.null(has_date)) metadata <- metadata[metadata$has_date == as.numeric(has_date), ]
  if (!is.null(has_rt)) metadata <- metadata[metadata$has_rt == as.numeric(has_rt), ]
  if (!is.null(has_rater)) metadata <- metadata[metadata$has_rater == as.numeric(has_rater), ]
  
  # Check for matching tables
  if (nrow(metadata) == 0) {
    message("No tables match the specified criteria.")
    return(character(0))
  }
  
  # Return matching table names
  return(metadata$table_name)
}



#' Filter Tables by Criteria
#'
#' Filters the IRW database metadata to return the names of tables that match the specified criteria.
#' Criteria include ranges for numeric attributes (e.g., `id_count`, `density`) and the presence of specific columns 
#' (e.g., `date`, `response time`, or `rater`).
#'
#' @param id_count A numeric vector of two values specifying the range for the number of unique IDs 
#'        (e.g., `c(100, 1000)`). Default is `NULL`, which ignores this criterion.
#' @param item_count A numeric vector of two values specifying the range for the number of unique items. 
#'        Default is `NULL`.
#' @param resp_count A numeric vector of two values specifying the range for the total number of responses. 
#'        Default is `NULL`.
#' @param density A numeric vector of two values specifying the range for density (between 0 and 1). 
#'        Default is `NULL`.
#' @param has_date Logical; `TRUE` to include tables with a `date` column, `FALSE` to exclude them, 
#'        or `NULL` to ignore this criterion. Default is `NULL`.
#' @param has_rt Logical; `TRUE` to include tables with a `rt` column, `FALSE` to exclude them, 
#'        or `NULL` to ignore this criterion. Default is `NULL`.
#' @param has_rater Logical; `TRUE` to include tables with a `rater` column, `FALSE` to exclude them, 
#'        or `NULL` to ignore this criterion. Default is `NULL`.
#' @return A character vector of table names that match the criteria. If no tables match, it prints a message 
#'         and returns an empty character vector.
#' @examples
#' \dontrun{
#'   irw_filter(id_count = c(100, 1000), density = c(0.1, 0.5), has_date = TRUE)
#' }
#' @export
irw_filter <- function(id_count = NULL, item_count = NULL, resp_count = NULL, density = NULL,
                       has_date = NULL, has_rt = NULL, has_rater = NULL) {
  
  # Helper function to check if value is within range (inclusive)
  is_within_range <- function(value, range) {
    if (is.null(range)) return(TRUE)
    value >= range[1] & value <= range[2] & !is.na(value)
  }
  
  # Validate ranges
  validate_range <- function(range, name) {
    if (!is.null(range) && (!is.numeric(range) || length(range) != 2)) {
      stop(sprintf("'%s' must be a numeric vector of length 2.", name))
    }
  }
  validate_range(id_count, "id_count")
  validate_range(item_count, "item_count")
  validate_range(resp_count, "resp_count")
  validate_range(density, "density")
  
  # Check required columns
  required_columns <- c("id_count", "item_count", "resp_count", "density", "has_date", "has_rt", "has_rater")
  missing_columns <- setdiff(required_columns, names(metadata))
  if (length(missing_columns) > 0) {
    stop(sprintf("The following required columns are missing from metadata: %s", paste(missing_columns, collapse = ", ")))
  }
  
  # Apply numeric filters
  numeric_filters <- list(id_count = id_count, item_count = item_count, resp_count = resp_count, density = density)
  for (filter_name in names(numeric_filters)) {
    range <- numeric_filters[[filter_name]]
    if (!is.null(range)) {
      metadata <- metadata[is_within_range(metadata[[filter_name]], range), ]
    }
  }
  
  # Apply column presence filters
  if (!is.null(has_date)) metadata <- metadata[metadata$has_date == as.numeric(has_date), ]
  if (!is.null(has_rt)) metadata <- metadata[metadata$has_rt == as.numeric(has_rt), ]
  if (!is.null(has_rater)) metadata <- metadata[metadata$has_rater == as.numeric(has_rater), ]
  
  # Check for matching tables
  if (nrow(metadata) == 0) {
    message("No tables match the specified criteria.")
    return(character(0))
  }
  
  # Return matching table names
  return(metadata$table_name)
}


#' Beta: Filter Available Datasets in IRW
#'
#' Identifies datasets in the Item Response Warehouse (IRW) based on user-defined criteria.
#' This function filters datasets using **precomputed metadata**, which contains summary statistics
#' for each dataset (e.g., number of responses, number of participants, density scores, etc.).
#'
#' ## Exploring Metadata
#' To understand available dataset properties before filtering, run `summary(irw_metadata())`.
#' This provides an overview of key characteristics such as response counts, participant numbers, and density.
#' Users can then apply `irw_filter_beta()` to select datasets matching their criteria.
#'
#' @param n_responses A numeric vector of length 2 specifying the range for total responses.
#' @param n_categories A numeric vector of length 2 specifying the range for unique response categories.
#' @param n_participants A numeric vector of length 2 specifying the range for the number of participants.
#' @param n_items A numeric vector of length 2 specifying the range for the number of items.
#' @param responses_per_participant A numeric vector of length 2 specifying the range for average responses per participant.
#' @param responses_per_item A numeric vector of length 2 specifying the range for average responses per item.
#' @param density A numeric vector of length 2 specifying the range for data density. 
#'                Defaults to `c(0.5, 1)`. To disable this filter, set `density = NULL`.
#' @param var A character vector specifying one or more available variables (`"treat"`, `"wave"`, `"date"`, `"rt"`, `"rater"`).
#'            Only datasets containing all specified variables will be returned.
#' @return A character vector of dataset names matching **all specified criteria** or an empty result if invalid input is provided.
#' @export
irw_filter_beta <- function(n_responses = NULL, 
                            n_categories = NULL, 
                            n_participants = NULL, 
                            n_items = NULL, 
                            responses_per_participant = NULL, 
                            responses_per_item = NULL, 
                            density = c(0.5, 1),  # Default density filter
                            var = NULL) {
  
  metadata <- irw_metadata()  # Load latest metadata
  
  # Store the number of datasets before filtering
  initial_count <- nrow(metadata)
  
  # Define available variables for filtering
  available_vars <- c("treat", "wave", "date", "rt", "rater")
  
  # Check for invalid variable names
  if (!is.null(var)) {
    invalid_vars <- setdiff(var, available_vars)
    
    if (length(invalid_vars) > 0) {
      warning("The following variables are not available for filtering: ", 
              paste(invalid_vars, collapse = ", "), 
              ". Currently, you can only filter by: ",
              paste(available_vars, collapse = ", "), ".")
      return(character(0))  # Return empty result
    }
    
    # Filter by valid variable names
    for (v in var) {
      metadata <- metadata[metadata[[paste0("has_", v)]] == TRUE, ]
    }
    
    # If filtering by `var` alone removed all datasets, return early
    if (nrow(metadata) == 0) {
      return(character(0))  # No datasets left after `var` filtering
    }
  }
  
  # Store the count before applying density filtering
  count_before_density <- nrow(metadata)
  
  # Numeric filters
  filters <- list(
    n_responses = n_responses,
    n_categories = n_categories,
    n_participants = n_participants,
    n_items = n_items,
    responses_per_participant = responses_per_participant,
    responses_per_item = responses_per_item,
    density = density
  )
  
  # Remove NULL filters (unused parameters)
  filters <- filters[!sapply(filters, is.null)]
  
  # Check if the user explicitly set density
  user_specified_density <- !missing(density)
  
  # Apply numeric range filters dynamically
  for (filter_name in names(filters)) {
    filter_value <- filters[[filter_name]]
    
    if (is.numeric(filter_value) && length(filter_value) == 2) {
      metadata <- metadata[metadata[[filter_name]] >= filter_value[1] &
                             metadata[[filter_name]] <= filter_value[2], ]
    } else {
      metadata <- metadata[metadata[[filter_name]] %in% filter_value, ]
    }
  }
  
  # Check if the default density filter was applied **and** actually removed some datasets
  num_removed_by_density <- count_before_density - nrow(metadata)
  if (!user_specified_density && identical(density, c(0.5, 1)) && num_removed_by_density > 0) {
    message(sprintf("Note: The default density filter (0.5 to 1) was applied and removed %d dataset(s). To disable it, set `density = NULL`.", num_removed_by_density))
  }
  
  return(metadata$dataset_name)
}