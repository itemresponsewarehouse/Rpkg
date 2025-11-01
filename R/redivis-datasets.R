#' #' Retry with Exponential Backoff
#' #'
#' #' Automatically retries an API call if it fails due to transient issues.
#' #' Uses exponential backoff and applies a timeout to prevent excessive waiting.
#' #'
#' #' @param expr A function that executes the API call.
#' #' @param max_attempts Integer. Maximum number of retry attempts. Default is 5.
#' #' @param base_delay Numeric. Initial wait time (seconds) before retrying. Default is 1.
#' #' @param timeout_sec Numeric. Maximum time (seconds) allowed for each API call before forcing a retry. Default is 10.
#' #' @return The result of the API call if successful; otherwise, stops with an error.
#' #' @importFrom R.utils withTimeout
#' #' @keywords internal
#' .retry_with_backoff <- function(expr, max_attempts = 5, base_delay = 1, timeout_sec = 10) {
#'   attempt <- 1
#'   result <- NULL
#'   while (attempt <= max_attempts) {
#'     result <- tryCatch(
#'       {
#'         if (is.null(timeout_sec)) {
#'           expr()
#'         } else {
#'           R.utils::withTimeout(expr(), timeout = timeout_sec, onTimeout = "error")
#'         }
#'       },
#'       TimeoutException = function(e) NULL,
#'       error = function(e) NULL
#'     )
#'     if (!is.null(result)) return(result)
#'     if (attempt == max_attempts) break
#'     Sys.sleep(base_delay * (2^(attempt - 1)))
#'     attempt <- attempt + 1
#'   }
#'   stop("Request failed after ", max_attempts, " attempts.", call. = FALSE)
#' }

#' Retry with Exponential Backoff (Deprecated)
#'
#' This function is a no-op placeholder.
#'
#' @param expr A function that executes the API call.
#' @param ... Ignored.
#' @return The result of evaluating `expr()`.
#' @keywords internal
.retry_with_backoff <- function(expr, ...) {
  if (is.function(expr)) {
    expr()
  } else {
    stop("`.retry_with_backoff()` now expects a function argument, e.g. `.retry_with_backoff(function() expr)`")
  }
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

#' Initialize Redivis Datasource(s)
#'
#' Returns a list of Redivis dataset objects based on the selected source:
#' - If `sim = TRUE`, returns the IRW simulation dataset (`irw_simsyn:0btg`)
#' - If `comp = TRUE`, returns the IRW competition dataset (`irw_competitions:cmd7`)
#' - Otherwise, returns the main IRW production datasets
#'
#' Note: `sim` and `comp` are mutually exclusive. Setting both to TRUE will raise an error.
#'
#' @param sim Logical. If TRUE, connects to the IRW simulation dataset.
#' @param comp Logical. If TRUE, connects to the IRW competition dataset.
#'
#' @return A list of one or more Redivis dataset objects.
#' @keywords internal
.initialize_datasource <- function(sim = FALSE, comp = FALSE) {
  if (!is.logical(sim) || length(sim) != 1) stop("'sim' must be a single TRUE or FALSE value.")
  if (!is.logical(comp) || length(comp) != 1) stop("'comp' must be a single TRUE or FALSE value.")
  if (sim && comp) stop("Cannot set both 'sim = TRUE' and 'comp = TRUE'. Please choose one data source.")
  
  if (sim) {
    if (!exists("sim_datasource", envir = .irw_env) || is.null(.irw_env$sim_datasource)) {
      ds <- redivis::redivis$user("bdomingu")$dataset("irw_simsyn:0btg")
      ds$get()
      .irw_env$sim_datasource <- ds
    }
    return(list(.irw_env$sim_datasource))
  } else if (comp) {
    if (!exists("comp_datasource", envir = .irw_env) || is.null(.irw_env$comp_datasource)) {
      ds <- redivis::redivis$user("bdomingu")$dataset("irw_competitions:cmd7")
      ds$get()
      .irw_env$comp_datasource <- ds
    }
    return(list(.irw_env$comp_datasource))
  } else {
    if (!exists("datasource_list", envir = .irw_env) || is.null(.irw_env$datasource_list)) {
      .irw_env$datasource_list <- list(
        redivis::redivis$user("datapages")$dataset("item_response_warehouse:as2e"),
        redivis::redivis$user("datapages")$dataset("item_response_warehouse_2:epbx")
      )
      lapply(.irw_env$datasource_list, function(ds) ds$get())
    }
    return(.irw_env$datasource_list)
  }
}