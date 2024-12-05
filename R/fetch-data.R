#' Fetch Data from the IRW Database
#'
#' Retrieves one or more datasets from the Item Response Warehouse (IRW) database by their names.
#'
#' @param name A character vector specifying one or more dataset names to fetch.
#' @return If a single dataset is fetched, returns a data frame; if multiple datasets are fetched, returns a named list of data frames.
#' @examples
#' \dontrun{
#'   # Fetch a single dataset
#'   df <- fetch_data("dataset_name")
#'   print(df)
#'
#'   # Fetch multiple datasets
#'   datasets <- fetch_data(c("dataset1", "dataset2"))
#'   print(names(datasets))  # Displays "dataset1" and "dataset2"
#' }
fetch_data <- function(name) {
  # Helper function to fetch and convert a single dataset
  fetch_single_data <- function(dataset_name) {
    # Attempt to fetch the table
    table <- tryCatch(
      fetch_table(dataset_name),
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


#' #' Filter Tables by Minimum Rows and Required Columns
#' #'
#' #' This function filters tables in the IRW database to include only those
#' #' that have at least a specified number of rows and contain specified columns.
#' #' It uses precomputed metadata for faster access.
#' #'
#' #' @param n_rows An integer specifying the minimum number of rows required in a table.
#' #' @param required_columns A character vector specifying the names of required columns.
#' #' @return A character vector of table names that meet the criteria.
#' #' @importFrom utils data
#' #' @export
#' filter_tables <- function(n_rows = 0, required_columns = NULL) {
#'   # Load precomputed metadata
#'   if (file.exists("data/course-data.csv")) {
#'     metadata_summary <- utils::read.csv("data/metadata.csv")
#'   } else {
#'     stop("No metadata found.")
#'   }
#' 
#'   # Initialize a vector to store matching table names
#'   matching_tables <- character(0)
#' 
#'   # Iterate through each row in metadata_summary to apply filters
#'   for (i in seq_len(nrow(metadata_summary))) {
#'     table_info <- metadata_summary[i, ]
#' 
#'     # Check minimum number of rows
#'     if (!is.null(n_rows) && table_info$numRows < n_rows) next
#' 
#'     # Check if table contains all required columns
#'     if (!is.null(required_columns)) {
#'       # Get column names in the table
#'       table_columns <- unlist(table_info$columns)
#' 
#'       # Skip table if it does not contain all required columns
#'       if (!all(required_columns %in% table_columns)) next
#'     }
#' 
#'     # If both criteria are met, add the table name to the results
#'     matching_tables <- c(matching_tables, table_info$table_name)
#'   }
#' 
#'   # Return the list of table names that meet the criteria
#'   return(matching_tables)
#' }


