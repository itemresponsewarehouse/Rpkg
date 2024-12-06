#' Filter Tables by Criteria
#'
#' Filters the metadata to return the names of tables that match the specified criteria.
#'
#' @param id_count A numeric vector of two values specifying the range for the number of unique IDs 
#'        (e.g., `c(100, 1000)`).
#' @param item_count A numeric vector of two values specifying the range for the number of unique items.
#' @param resp_count A numeric vector of two values specifying the range for the total number of responses.
#' @param sparsity A numeric vector of two values specifying the range for sparsity (between 0 and 1).
#' @param has_date Logical; `TRUE` to include tables with a `date` column, `FALSE` to exclude them, 
#'        or `NULL` to ignore this criterion.
#' @param has_rt Logical; `TRUE` to include tables with a `rt` column, `FALSE` to exclude them, 
#'        or `NULL` to ignore this criterion.
#' @param has_rater Logical; `TRUE` to include tables with a `rater` column, `FALSE` to exclude them, 
#'        or `NULL` to ignore this criterion.
#' @return A character vector of table names that match the criteria. If no tables match, it prints a message 
#'         and returns an empty character vector.
#' @examples
#' filter_tables(id_count = c(100, 1000), sparsity = c(0.1, 0.5), has_date = TRUE)
#' @importFrom utils read.csv
#' @export
filter_tables <- function(id_count = NULL, item_count = NULL, resp_count = NULL, sparsity = NULL,
                          has_date = NULL, has_rt = NULL, has_rater = NULL) {
  
  metadata <- get_metadata()
  
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