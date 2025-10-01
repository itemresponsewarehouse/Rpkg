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
        if (is.null(timeout_sec)) {
          expr()
        } else {
          R.utils::withTimeout(expr(), timeout = timeout_sec, onTimeout = "error")
        }
      },
      TimeoutException = function(e) NULL,
      error = function(e) NULL
    )
    if (!is.null(result)) return(result)
    if (attempt == max_attempts) break
    Sys.sleep(base_delay * (2^(attempt - 1)))
    attempt <- attempt + 1
  }
  stop("Request failed after ", max_attempts, " attempts.", call. = FALSE)
}

# helper for multiple redivis datasets
.get_all_irw_datasets <- function() {
  if (!exists("irw_datasets", envir = .irw_env) || is.null(.irw_env$irw_datasets)) {
    .irw_env$irw_datasets <- list(
      redivis::redivis$user("datapages")$dataset("item_response_warehouse:as2e"),
      redivis::redivis$user("datapages")$dataset("item_response_warehouse_2:epbx")
    )
    # Call get() for each dataset
    lapply(.irw_env$irw_datasets, function(ds) .retry_with_backoff(function() ds$get()))
  }
  .irw_env$irw_datasets
}

#' Initialize Datasource(s)
#'
#' Returns a list of Redivis dataset objects for the IRW data sources.
#' If sim = FALSE, returns both main IRW datasets (dataset1 first, dataset2 second).
#' If sim = TRUE, returns the simulation dataset only.
#'
#' @param sim Logical. If TRUE, connects to the IRW simulation dataset (`irw_simsyn`).
#' @return A list of one or more Redivis dataset objects.
#' @keywords internal
.initialize_datasource <- function(sim = FALSE) {
  if (!is.logical(sim) || length(sim) != 1) stop("'sim' must be a single TRUE or FALSE value.")
  if (isFALSE(sim)) {
    if (!exists("datasource_list", envir = .irw_env) || is.null(.irw_env$datasource_list)) {
      .irw_env$datasource_list <- list(
        redivis::redivis$user("datapages")$dataset("item_response_warehouse:as2e"),
        redivis::redivis$user("datapages")$dataset("item_response_warehouse_2:epbx")
      )
      lapply(.irw_env$datasource_list, function(ds) ds$get())
    }
    return(.irw_env$datasource_list)
  } else {
    if (!exists("sim_datasource", envir = .irw_env) || is.null(.irw_env$sim_datasource)) {
      ds <- redivis::redivis$user("bdomingu")$dataset("irw_simsyn:0btg")
      ds$get()
      .irw_env$sim_datasource <- ds
    }
    return(list(.irw_env$sim_datasource))
  }
}



#' Fetch Table Data
#'
#' Retrieves a specified table from Redivis with automatic retry logic.
#' This function returns a Redivis table object, which can later be converted to a tibble.
#'
#' @param name A character string specifying the table name.
#' @param sim Logical. If TRUE, fetches from the IRW simulation dataset (`irw_simsyn`).
#' @return A Redivis table object.
#' @keywords internal
.fetch_redivis_table <- function(name, sim = FALSE) {
  if (!is.character(name) || length(name) != 1) stop("The 'name' parameter must be a single character string.")
  ds_list <- .initialize_datasource(sim = sim)
  
  for (ds in ds_list) {
    ds$get()
    
    result <- tryCatch(
      {
        withCallingHandlers({
          tbl <- ds$table(name)
          tbl$get()
          tbl
        },
        warning = function(w) {
          if (grepl("No reference id was provided for the table", conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        })
      },
      error = function(e) {
        msg <- e$message
        if (grepl("not_found_error", msg, ignore.case = TRUE)) {
          return(NULL)  # try next dataset
        }
        if (grepl("invalid_request_error", msg, ignore.case = TRUE)) {
          stop(paste("\nTable", shQuote(name), "cannot be fetched due to an invalid format."), call. = FALSE)
        }
        if (grepl("could not find function \"stream_callback\"", msg, ignore.case = TRUE)) {
          return(.retry_with_backoff(function() {
            withCallingHandlers({
              tbl <- ds$table(name)
              tbl$get()
              tbl
            },
            warning = function(w) {
              if (grepl("No reference id was provided for the table", conditionMessage(w))) {
                invokeRestart("muffleWarning")
              }
            })
          }, max_attempts = 3, timeout_sec = NULL))
        }
        stop(paste("\nAn unknown error occurred:", msg), call. = FALSE)
      }
    )
    if (!is.null(result)) return(result)
  }
  
  stop(paste("\nTable", shQuote(name), "does not exist in the IRW database."), call. = FALSE)
}


#' Fetch Metadata Table
#'
#' Retrieves the metadata table from Redivis user("bdomingu")$dataset("irw_meta")$table("metadata").
#' Only fetches new data if the table version tag has changed.
#'
#' @return A cached or newly fetched tibble containing metadata information.
#' @keywords internal
.fetch_metadata_table <- function() {
  dataset <- redivis::redivis$user("bdomingu")$dataset("irw_meta:bdxt")
  dataset$get()
  # Retrieve version tag
  latest_version_tag <- dataset$properties$version$tag

  # If metadata exists in cache and the version tag is the same, return cached tibble
  if (!is.null(latest_version_tag) &&
    exists("metadata_tibble", envir = .irw_env) &&
    exists("metadata_version", envir = .irw_env) &&
    identical(.irw_env$metadata_version, latest_version_tag)) {
    return(.irw_env$metadata_tibble) # Return cached metadata tibble
  }

  # Fetch new metadata table and convert it to a tibble
  table <- dataset$table("metadata:h5gs")

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
  dataset <- redivis::redivis$user("bdomingu")$dataset("irw_meta:bdxt")
  dataset$get() 
  latest_version_tag <- dataset$properties$version$tag
  
  # Return cached if version unchanged
  if (!is.null(latest_version_tag) &&
      exists("biblio_tibble", envir = .irw_env) &&
      identical(.irw_env$biblio_version, latest_version_tag)) {
    return(.irw_env$biblio_tibble)
  }
  
  # Fetch fresh biblio table
  table <- dataset$table("biblio:qahg")
  biblio_tibble <- .retry_with_backoff(function() table$to_tibble())
  
  # Get all table names from all datasets
  ds_list <- .initialize_datasource(sim = FALSE)
  table_name_list <- tolower(unlist(lapply(ds_list, function(ds) {
    ds$get()  
    vapply(ds$list_tables(), function(tbl) tbl$name, character(1))
  })))
  
  # Filter biblio table
  biblio_tibble$table_lower <- tolower(biblio_tibble$table)
  filtered_biblio <- biblio_tibble[biblio_tibble$table_lower %in% table_name_list, ]
  filtered_biblio$table_lower <- NULL
  
  # Cache
  .irw_env$biblio_version <- latest_version_tag
  .irw_env$biblio_tibble <- filtered_biblio
  
  return(filtered_biblio)
}

#' Fetch Tags Metadata Table
#'
#' Retrieves the tags metadata table from Redivis user("bdomingu")$dataset("irw_meta")$table("tags").
#' Only fetches new data if the table version tag has changed.
#' Filters out any tags referring to tables that do not exist in the IRW database.
#'
#' @return A tibble containing filtered tags information.
#' @keywords internal
.fetch_tags_table <- function() {
  dataset <- redivis::redivis$user("bdomingu")$dataset("irw_meta:bdxt")
  dataset$get()
  latest_version_tag <- dataset$properties$version$tag
  
  # Return cached if version unchanged
  if (!is.null(latest_version_tag) &&
      exists("tags_tibble", envir = .irw_env) &&
      identical(.irw_env$tags_version, latest_version_tag)) {
    return(.irw_env$tags_tibble)
  }
  
  # Fetch and clean tags table
  table <- dataset$table("tags:7nkh")
  tags_tibble <- .retry_with_backoff(function() table$to_tibble())
  
  tags_tibble <- as.data.frame(tags_tibble)
  tags_tibble[] <- lapply(tags_tibble, function(col) {
    if (is.character(col)) col[col == "NA"] <- NA
    col
  })
  tags_tibble <- tibble::as_tibble(tags_tibble)
  
  # Get all table names from all datasets
  ds_list <- .initialize_datasource(sim = FALSE)
  table_name_list <- tolower(unlist(lapply(ds_list, function(ds) {
    ds$get()  
    vapply(ds$list_tables(), function(tbl) tbl$name, character(1))
  })))
  
  # Filter tags table
  tags_tibble$table_lower <- tolower(tags_tibble$table)
  filtered_tags <- tags_tibble[tags_tibble$table_lower %in% table_name_list, ]
  filtered_tags$table_lower <- NULL
  
  # Cache
  .irw_env$tags_version <- latest_version_tag
  .irw_env$tags_tibble <- filtered_tags
  
  return(filtered_tags)
}


#' Access the IRW item text dataset object
#'
#' Returns the Redivis dataset object for IRW item text metadata,
#' and ensures metadata is loaded via \code{get()}.
#'
#' @return A Redivis dataset object.
#' @keywords internal
.get_irw_itemtext_dataset <- function() {
  if (!exists("itemtext_dataset", envir = .irw_env) || is.null(.irw_env$itemtext_dataset)) {
    dataset <- redivis::redivis$user("bdomingu")$dataset("irw_text:07b6")
    dataset$get() 
    .irw_env$itemtext_dataset <- dataset
  }
  .irw_env$itemtext_dataset
}


#' Fetch item text table from IRW Redivis dataset 
#'
#' Retrieves an item text table from the IRW Redivis text dataset,
#' given a known base table name. If not available, it returns `NULL` with a message.
#'
#' @param table_name Character. The base table name (e.g., "gilbert_meta_49").
#' @return A tibble with item text metadata or `NULL`.
#' @keywords internal
.fetch_itemtext_table <- function(table_name) {
  available_tables <- irw_list_itemtext_tables()
  
  if (!(table_name %in% available_tables)) {
    message(glue::glue("Item text not available for table: '{table_name}'"))
    return(NULL)
  }
  
  dataset <- .get_irw_itemtext_dataset()
  full_name <- paste0(table_name, "__items")
  
  table <- dataset$table(full_name)
  suppressWarnings(.retry_with_backoff(function() table$to_tibble()))
}

