# Core functions for data querying from Redivis

#' Fetch Table Data (Internal Helper)
#'
#' An internal helper function to initialize the datasource and retrieve a specific table by name.
#'
#' This function is used by \code{fetch_data} to access the specified table from
#' the datasource after ensuring that the datasource connection is initialized.
#'
#' @param name A character string specifying the name of the table to retrieve.
#' @return A Redivis table object for the specified table.
#' @keywords internal
fetch_table <- function(name) {
  # Initialize datasource if not already set
  ds <- initialize_datasource()

  # Access the specified table
  table_data <- ds$table(name)
  return(table_data)
}

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




