# Internal utility functions for Redivis connections

# Define a package environment to store global variables
.irw_env <- new.env(parent = emptyenv())

#' Retry with Exponential Backoff
#'
#' Helper function that retries a given expression up to `max_attempts` times.
#' Wait time increases exponentially on each failure.
#'
#' @param expr The expression to evaluate (typically a function call).
#' @param max_attempts Maximum number of attempts before giving up.
#' @param base_delay Initial wait time (seconds) before retrying.
#' @return The result of `expr` if successful, otherwise stops with an error.
#' @keywords internal
.retry_with_backoff <- function(expr, max_attempts = 3, base_delay = 1) {
  attempt <- 1
  
  while (attempt <= max_attempts) {
    tryCatch({
      return(eval(expr))  # Try evaluating the expression
    }, error = function(e) {
      if (attempt == max_attempts) {
        stop("Redivis request failed after ", max_attempts, " attempts: ", e$message)
      }
      
      # Calculate backoff delay
      delay <- base_delay * (2 ^ (attempt - 1))  # Exponential growth (1s, 2s, 4s)
      message("Attempt ", attempt, " failed: ", e$message, ". Retrying in ", delay, " seconds...")
      
      Sys.sleep(delay)  # Wait before retrying
      attempt <- attempt + 1
    })
  }
}


#' Initialize Datasource (With Retry Logic)
#'
#' Ensures a stable connection to Redivis. If it fails, retries automatically.
#' @return The initialized datasource connection.
#' @keywords internal
.initialize_datasource <- function() {
  if (!exists("datasource", envir = .irw_env) || is.null(.irw_env$datasource)) {
    .irw_env$datasource <- .retry_with_backoff(quote({
      datasource <- redivis::user("datapages")$dataset("item_response_warehouse")
      datasource$get()  # Test connection
      datasource
    }))
  }
  return(.irw_env$datasource)
}

#' Fetch Table Data with Retry
#'
#' Retrieves a specified table from Redivis with automatic retry logic.
#' @param name A character string specifying the table name.
#' @return A Redivis table object.
#' @keywords internal
.fetch_redivis_table <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    stop("The 'name' parameter must be a single character string.")
  }
  
  # Initialize datasource if not set
  ds <- .initialize_datasource()
  
  # Fetch table with retries
  .retry_with_backoff(quote({
    table_data <- ds$table(name)
    table_data$get()  # Perform API request
    table_data
  }))
}