# Functions for querying and manipulating data tables from the IRW database.

#' Fetch Data from a Specified Table
#'
#' Retrieves a specified table's data from the datasource, converting it to a data frame.
#' If the conversion to a data frame fails, it stops and returns an informative error message.
#'
#' This function initializes the connection to the datasource (if not already initialized),
#' retrieves the specified table, and converts it to a data frame. The function requires
#' the user to have authenticated with Redivis and initialized the datasource.
#'
#' @param name A character string specifying the name of the table to fetch.
#' @return A data frame containing the data from the specified table.
#' @examples
#' \dontrun{
#'   df <- fetch_data("abortion")
#'   head(df)
#' }
#' @export
fetch_data <- function(name) {
  table <- fetch_table(name)

  # Try to convert the table to a data frame, catching errors if the conversion fails
  df <- tryCatch(
    table$to_data_frame(),
    error = function(e) {
      stop(paste("Unable to fetch the dataset", shQuote(name),
                 "from the IRW database. Please check the dataset name."))
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
#' @export
filter_tables <- function(n_rows = 0, required_columns = NULL) {
  # Load the precomputed metadata summary
  metadata_path <- "data/metadata_summary.RData"
  if (!file.exists(metadata_path)) {
    stop("Metadata summary file not found. Run generate_metadata_summary() first.")
  }
  load(metadata_path)

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


