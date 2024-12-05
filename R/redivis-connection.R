# Initialize Redivis data source connection and fetch tables  


# defining a package environment
.irw_env <- new.env(parent = emptyenv())

#' Initialize Datasource
#'
#' This function initializes the Redivis datasource connection if it is not already set.
#' If the global variable `datasource` doesn't exist or is NULL, it sets up a new connection.
#' @return The initialized datasource connection.
initialize_datasource <- function() {
  if (!exists("datasource", envir = .irw_env) ||
      is.null(.irw_env$datasource)) {
    datasource <- redivis::user("datapages")$dataset("item_response_warehouse")
    datasource$get() # Perform an API request with the initialized reference
    .irw_env$datasource <- datasource
  }
  return(.irw_env$datasource)
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
