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
#' # Example 1: Filter tables with ID count between 100 and 1000
#' filter_tables(id_count = c(100, 1000))
#'
#' # Example 2: Filter tables with multiple numeric criteria
#' filter_tables(id_count = c(100, 1000), sparsity = c(0.1, 0.5))
#'
#' # Example 3: Filter tables with specific columns
#' filter_tables(has_date = TRUE, has_rt = TRUE)
#'
#' # Example 4: Combine numeric and column presence criteria
#' filter_tables(id_count = c(100, 1000), has_date = TRUE)
#' 
#' @importFrom utils read.csv
#' @export
filter_tables <- function(id_count = NULL, item_count = NULL, resp_count = NULL, sparsity = NULL,
                          has_date = NULL, has_rt = NULL, has_rater = NULL) {
  
  metadata <- get_metadata()
  
  # Helper function to check if a value is within the specified range
  is_within_range <- function(value, range) {
    if (is.null(range)) {
      return(TRUE)  # No range specified, include all values
    }
    value >= range[1] & value <= range[2]
  }
  
  # Apply numeric filters
  if (!is.null(id_count)) {
    metadata <- metadata[is_within_range(metadata$id_count, id_count), ]
  }
  if (!is.null(item_count)) {
    metadata <- metadata[is_within_range(metadata$item_count, item_count), ]
  }
  if (!is.null(resp_count)) {
    metadata <- metadata[is_within_range(metadata$resp_count, resp_count), ]
  }
  if (!is.null(sparsity)) {
    metadata <- metadata[is_within_range(metadata$sparsity, sparsity), ]
  }
  
  # Apply column presence filters
  if (!is.null(has_date)) {
    metadata <- metadata[metadata$has_date == as.numeric(has_date), ]
  }
  if (!is.null(has_rt)) {
    metadata <- metadata[metadata$has_rt == as.numeric(has_rt), ]
  }
  if (!is.null(has_rater)) {
    metadata <- metadata[metadata$has_rater == as.numeric(has_rater), ]
  }
  
  # Check if there are any matching tables
  if (nrow(metadata) == 0) {
    message("No tables match the specified criteria.")
    return(character(0))  # Return an empty character vector
  }
  
  # Return the names of the matching tables
  return(metadata$table_name)
}