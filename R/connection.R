# Internal utility functions for Redivis connections

# Define a package environment to store global variables
.irw_env <- new.env(parent = emptyenv())

#' Initialize Datasource
#'
#' This function initializes the Redivis datasource connection if it is not already set.
#' If the global variable `datasource` doesn't exist or is NULL, it sets up a new connection.
#' @return The initialized datasource connection.
#' @keywords internal
.initialize_datasource <- function() {
  if (!exists("datasource", envir = .irw_env) || is.null(.irw_env$datasource)) {
    tryCatch({
      datasource <- redivis::user("datapages")$dataset("item_response_warehouse")
      datasource$get() # Test the connection
      .irw_env$datasource <- datasource
    }, error = function(e) {
      stop("Failed to initialize the datasource: ", e$message)
    })
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
#' @keywords internal
.fetch_redivis_table <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    stop("The 'name' parameter must be a single character string.")
  }
  
  # Initialize datasource if not already set
  ds <- .initialize_datasource()
  
  # Access the specified table
  tryCatch({
    table_data <- ds$table(name)
    table_data$get() # Perform an API request to fetch the table data
    return(table_data)
  }, error = function(e) {
    stop("Failed to fetch the table '", name, "': ", e$message)
  })
}