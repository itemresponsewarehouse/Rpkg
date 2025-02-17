# Define a package environment to store global variables
.irw_env <- new.env(parent = emptyenv())

#' Retry with Exponential Backoff
#'
#' Automatically retries an API call if it fails due to transient issues.
#' Uses exponential backoff and applies a timeout to prevent excessive waiting.
#'
#' @param expr A function that executes the API call.
#' @param max_attempts Integer. Maximum number of retry attempts. Default is 5.
#' @param base_delay Numeric. Initial wait time (seconds) before retrying. Default is 1.
#' @param timeout_sec Numeric. Maximum time (seconds) allowed for each API call before forcing a retry. Default is 10.
#' @return The result of the API call if successful; otherwise, stops with an error.
#' @importFrom R.utils withTimeout
#' @keywords internal
.retry_with_backoff <- function(expr, max_attempts = 5, base_delay = 1, timeout_sec = 10) {
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
    table_data$get()  # fetch data immediately (without retry)
    table_data
  }, error = function(e) {
    error_message <- e$message
    
    # If dataset does not exist, stop immediately (don't retry)
    if (grepl("not_found_error", error_message, ignore.case = TRUE)) {
      stop(paste("\nTable", shQuote(name), "does not exist in the IRW database."), call. = FALSE)
    }
    
    # If dataset does not exist, stop immediately (don't retry)
    if (grepl("invalid_request_error", error_message, ignore.case = TRUE)) {
      stop(paste("\nTable", shQuote(name), "cannot be fetched due to an invalid format."),
           call. = FALSE)
    }
    
    # For transient errors, apply automatic retry logic
    .retry_with_backoff(function() {
      table_data <- ds$table(name)
      table_data$get()
      table_data
    })
  })
}

#' Fetch Metadata Table
#'
#' Retrieves the metadata table from Redivis user("bdomingu")$dataset("irw_meta")$table("metadata").
#' Only fetches new data if the table version tag has changed.
#'
#' @return A cached or newly fetched tibble containing metadata information.
#' @keywords internal
.fetch_metadata_table <- function() {
  dataset <- redivis::user("bdomingu")$dataset("irw_meta")
  
  # Ensure we have the latest dataset metadata
  .retry_with_backoff(function() {
    dataset$get()
  })
  
  # Retrieve version tag
  latest_version_tag <- dataset$properties$version$tag
  
  # If metadata exists in cache and the version tag is the same, return cached tibble
  if (!is.null(latest_version_tag) &&
      exists("metadata_tibble", envir = .irw_env) &&
      exists("metadata_version", envir = .irw_env) &&
      identical(.irw_env$metadata_version, latest_version_tag)) {
    return(.irw_env$metadata_tibble)  # Return cached metadata tibble
  }
  
  # Fetch new metadata table and convert it to a tibble
  table <- dataset$table("metadata")
  
  .irw_env$metadata_tibble <- .retry_with_backoff(function() {
    table$to_tibble()
  })
  
  # Store the new version tag
  .irw_env$metadata_version <- latest_version_tag
  
  return(.irw_env$metadata_tibble)
}


#' Fetch Bibliography Metadata Table
#'
#' Retrieves the metadata table from Redivis user("bdomingu")$dataset("irw_meta")$table("biblio").
#' Only fetches new data if the table version tag has changed.
#' This version filters out any tables that do not exist in the IRW database.
#'
#' @return A tibble containing filtered biblio information, with only the tables that exist in the IRW database.
#' @keywords internal
.fetch_biblio_table <- function() {
  # Initialize the datasource
  ds <- .initialize_datasource()
  
  # Retrieve the list of tables from the datasource
  tables <- ds$list_tables()
  table_name_list <- sapply(tables, function(table) table$name)  # List of available table names in IRW
  
  # Fetch the biblio table from the Redivis dataset
  dataset <- redivis::user("bdomingu")$dataset("irw_meta")
  .retry_with_backoff(function() {
    dataset$get()
  })
  latest_version_tag <- dataset$properties$version$tag
  
  # If biblio exists in cache and the version tag is the same, return cached tibble
  if (!is.null(latest_version_tag) &&
      exists("biblio_tibble", envir = .irw_env) &&
      exists("biblio_version", envir = .irw_env) &&
      identical(.irw_env$biblio_version, latest_version_tag)) {
    return(.irw_env$biblio_tibble)  # Return cached biblio tibble
  }
  
  # Fetch new biblio table and convert it to a tibble
  table <- dataset$table("biblio")
  biblio_tibble <- .retry_with_backoff(function() {
    table$to_tibble()
  })
  
  # Store the new version tag
  .irw_env$biblio_version <- latest_version_tag
  # Filter biblio table to only include tables that exist in the IRW database
  .irw_env$biblio_tibble = biblio_tibble[biblio_tibble$table %in% table_name_list, ]
  
  return(.irw_env$biblio_tibble)
}
