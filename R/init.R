# init.R: Initialize the Redivis data source connection and provide helper functions for accessing tables within the IRW database.


# Declare global variable to suppress check note
utils::globalVariables(c("metadata_summary", "datasource"))

#' Initialize Datasource
#'
#' This function initializes the Redivis datasource connection if it is not already set.
#' If the global variable `datasource` doesn't exist or is NULL, it sets up a new connection.
#' @return The initialized datasource connection.
initialize_datasource <- function() {
  # Check if 'datasource' exists in the global environment and is non-NULL
  if (!exists("datasource", envir = .GlobalEnv) || is.null(datasource)) {
    datasource <<- redivis::user("datapages")$dataset("item_response_warehouse") # init a reference to IRW
    datasource$get() # performing an API request with the initialized reference
  }
  return(datasource)
}



#' Fetch Table Data
#'
#' This function initializes the datasource (if not already initialized) and retrieves a specified table by name.
#' It is primarily used by other functions to access a table from the datasource.
#'
#' @param name A character string specifying the name of the table to retrieve.
#' @return A Redivis table object for the specified table.
fetch_table <- function(name) {
  # Initialize datasource if not already set
  ds <- initialize_datasource()

  # Access the specified table
  table_data <- ds$table(name)
  table_data$get() # performing an API request to fetch the table data
  return(table_data)
}

