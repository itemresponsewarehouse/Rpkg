#' Fetch Data from the IRW Database
#'
#' Retrieves one or more datasets from the Item Response Warehouse (IRW) database by their names.
#' The datasets are fetched as data frames and can be accessed programmatically for analysis.
#'
#' @param name A character vector specifying one or more dataset names to fetch.
#' @return If a single dataset is fetched, returns a data frame. If multiple datasets are fetched, 
#'         returns a named list of data frames, where each name corresponds to the dataset name.
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
  # Helper function to fetch and convert a single dataset
  fetch_single_data <- function(dataset_name) {
    # Attempt to fetch the table
    table <- tryCatch(
      .fetch_redivis_table(dataset_name),
      error = function(e) {
        stop(paste("Unable to fetch the dataset", shQuote(dataset_name),
                   "from the IRW database. Please check the dataset name."))
      }
    )
    
    # Convert the table to a data frame
    df <- tryCatch(
      table$to_data_frame(),
      error = function(e) {
        stop(paste("Failed to convert the dataset", shQuote(dataset_name),
                   "to a data frame. Please ensure the dataset is in a compatible format."))
      }
    )
    
    return(df)
  }
  
  # Check if fetching a single or multiple datasets
  if (length(name) == 1) {
    # Return a single data frame
    return(fetch_single_data(name))
  } else {
    # Return a named list of data frames if multiple datasets
    dataset_list <- lapply(name, fetch_single_data)
    names(dataset_list) <- name
    return(dataset_list)
  }
}


#' Filter Tables by Criteria
#'
#' Filters the IRW database metadata to return the names of tables that match the specified criteria.
#' Criteria include ranges for numeric attributes (e.g., `id_count`, `sparsity`) and the presence of specific columns 
#' (e.g., `date`, `response time`, or `rater`).
#'
#' @param id_count A numeric vector of two values specifying the range for the number of unique IDs 
#'        (e.g., `c(100, 1000)`). Default is `NULL`, which ignores this criterion.
#' @param item_count A numeric vector of two values specifying the range for the number of unique items. 
#'        Default is `NULL`.
#' @param resp_count A numeric vector of two values specifying the range for the total number of responses. 
#'        Default is `NULL`.
#' @param sparsity A numeric vector of two values specifying the range for sparsity (between 0 and 1). 
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
#'   irw_filter(id_count = c(100, 1000), sparsity = c(0.1, 0.5), has_date = TRUE)
#' }
#' @export
irw_filter <- function(id_count = NULL, item_count = NULL, resp_count = NULL, sparsity = NULL,
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
  validate_range(sparsity, "sparsity")
  
  # Check required columns
  required_columns <- c("id_count", "item_count", "resp_count", "sparsity", "has_date", "has_rt", "has_rater")
  missing_columns <- setdiff(required_columns, names(metadata))
  if (length(missing_columns) > 0) {
    stop(sprintf("The following required columns are missing from metadata: %s", paste(missing_columns, collapse = ", ")))
  }
  
  # Apply numeric filters
  numeric_filters <- list(id_count = id_count, item_count = item_count, resp_count = resp_count, sparsity = sparsity)
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

