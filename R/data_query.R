#' Fetch Data from the IRW Database
#'
#' Retrieves a dataset from the Item Response Warehouse (IRW) database by its name.
#'
#' @param name A character string specifying the name of the dataset to fetch.
#' @return A data frame containing the dataset.
#' @examples
#' \dontrun{
#'   df <- fetch_data("dataset_name")
#' }
fetch_data <- function(name) {
  # Attempt to fetch the table
  table <- tryCatch(
    fetch_table(name),
    error = function(e) {
      stop(paste("Unable to fetch the dataset", shQuote(name),
                 "from the IRW database. Please check the dataset name."))
    }
  )

  # Convert the table to a data frame
  df <- tryCatch(
    table$to_data_frame(),
    error = function(e) {
      stop(paste("Failed to convert the dataset", shQuote(name),
                 "to a data frame. Please ensure the dataset is in a compatible format."))
    }
  )

  return(df)
}


#' Filter Tables by Minimum Rows and Required Columns
#'
#' This function filters tables in the IRW database to include only those
#' that have at least a specified number of rows and contain specified columns.
#' It uses precomputed metadata for faster access.
#'
#' @param n_rows An integer specifying the minimum number of rows required in a table.
#' @param required_columns A character vector specifying the names of required columns.
#' @return A character vector of table names that meet the criteria.
#' @importFrom utils data
#' @export
filter_tables <- function(n_rows = 0, required_columns = NULL) {
  # Load the precomputed metadata summary
  data(metadata_summary, package="irwpkg")

  # Initialize a vector to store matching table names
  matching_tables <- character(0)

  # Iterate through each row in metadata_summary to apply filters
  for (i in seq_len(nrow(metadata_summary))) {
    table_info <- metadata_summary[i, ]

    # Check minimum number of rows
    if (!is.null(n_rows) && table_info$numRows < n_rows) next

    # Check if table contains all required columns
    if (!is.null(required_columns)) {
      # Get column names in the table
      table_columns <- unlist(table_info$columns)

      # Skip table if it does not contain all required columns
      if (!all(required_columns %in% table_columns)) next
    }

    # If both criteria are met, add the table name to the results
    matching_tables <- c(matching_tables, table_info$table_name)
  }

  # Return the list of table names that meet the criteria
  return(matching_tables)
}


