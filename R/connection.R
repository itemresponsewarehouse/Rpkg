# Define a package environment to store global variables
.irw_env <- new.env(parent = emptyenv())

#' Retry with Exponential Backoff
#'
#' Automatically retries an API call if it fails due to transient issues.
#' Uses exponential backoff and applies a timeout to prevent excessive waiting.
#'
#' @param expr A function that executes the API call.
#' @param max_attempts Integer. Maximum number of retry attempts. Default is 3.
#' @param base_delay Numeric. Initial wait time (seconds) before retrying. Default is 1.
#' @param timeout_sec Numeric. Maximum time (seconds) allowed for each API call before forcing a retry. Default is 10.
#' @return The result of the API call if successful; otherwise, stops with an error.
#' @importFrom R.utils withTimeout
#' @keywords internal
.retry_with_backoff <- function(expr, max_attempts = 3, base_delay = 1, timeout_sec = 10) {
  attempt <- 1
  result <- NULL
  
  while (attempt <= max_attempts) {
    result <- tryCatch(
      {
        withTimeout(expr(), timeout = timeout_sec, onTimeout = "error")
      },
      TimeoutException = function(e) {
        if (getOption("irwpkg.verbose", FALSE)) 
          message("Attempt ", attempt, " timed out after ", timeout_sec, " seconds.")
        NULL
      },
      error = function(e) {
        if (getOption("irwpkg.verbose", FALSE)) 
          message("Attempt ", attempt, " failed: ", e$message)
        NULL
      }
    )
    
    if (!is.null(result)) return(result)
    
    if (attempt == max_attempts) {
      stop("Redivis request failed after ", max_attempts, " attempts.")
    }
    
    delay <- base_delay * (2 ^ (attempt - 1))
    if (getOption("irwpkg.verbose", FALSE)) 
      message("Retrying in ", delay, " seconds...")
    
    Sys.sleep(delay)
    attempt <- attempt + 1
  }
  
  stop("Redivis request failed after ", max_attempts, " attempts.")
}

#' Initialize Datasource
#'
#' Establishes a connection to the Redivis datasource. Ensures `get()` is called 
#' to properly retrieve the dataset.
#'
#' @return A Redivis dataset object representing the connected datasource.
#' @keywords internal
.initialize_datasource <- function() {
  if (!exists("datasource", envir = .irw_env) || is.null(.irw_env$datasource)) {
    .irw_env$datasource <- .retry_with_backoff(function() {
      datasource <- redivis::user("datapages")$dataset("item_response_warehouse")
      datasource$get()
      datasource
    })
  }
  return(.irw_env$datasource)
}

#' Fetch Table Data
#'
#' Retrieves a specified table from Redivis with automatic retry logic.
#' This function returns a Redivis table object, which can later be converted to a tibble.
#'
#' @param name A character string specifying the table name.
#' @return A Redivis table object.
#' @keywords internal
.fetch_redivis_table <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    stop("The 'name' parameter must be a single character string.")
  }
  
  ds <- .initialize_datasource()
  
  tryCatch({
    table_data <- ds$table(name)
    table_data$get()  # etch data immediately (without retry)
    table_data
  }, error = function(e) {
    error_message <- e$message
    
    # If dataset does not exist, stop immediately (don't retry)
    if (grepl("not_found_error", error_message, ignore.case = TRUE)) {
      stop(paste("Dataset", shQuote(name), "does not exist in the IRW database."), call. = FALSE)
    }
    
    # For transient errors, apply automatic retry logic
    .retry_with_backoff(function() {
      table_data <- ds$table(name)
      table_data$get()
      table_data
    })
  })
}