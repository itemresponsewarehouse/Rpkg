#' Whether a Redivis/BigQuery error indicates a missing table
#'
#' @param msg Error message text.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.irw_is_not_found_error <- function(msg) {
  grepl("not_found_error", msg, ignore.case = TRUE) ||
    grepl("not\\s*found", msg, ignore.case = TRUE)
}

#' Classify a Redivis client error
#'
#' @param msg Error message text.
#' @return One of \code{"invalid"}, \code{"not_found"}, \code{"auth"}, \code{"other"}.
#' @keywords internal
#' @noRd
.irw_redivis_error_type <- function(msg) {
  if (grepl("invalid_request_error", msg, ignore.case = TRUE)) {
    return("invalid")
  }
  if (.irw_is_not_found_error(msg)) {
    return("not_found")
  }
  if (grepl(
    "unauthorized|unauthenticated|not authenticated|authentication|permission_denied|access_denied|forbidden|invalid_grant|login required|not authorized",
    msg,
    ignore.case = TRUE
  )) {
    return("auth")
  }
  "other"
}

#' Remove internal Redivis dataset paths from an error message
#'
#' @param msg Error message text.
#' @keywords internal
#' @noRd
.irw_sanitize_redivis_error <- function(msg) {
  if (is.null(msg) || !nzchar(msg)) {
    return(msg)
  }

  msg <- gsub(
    "(?i)(?:not found|error fetching):\\s*[^:]+:[^:]+:[^:]+\\.",
    "",
    msg,
    perl = TRUE
  )
  msg <- gsub("item_response_warehouse(?:_\\d+)?:[a-z0-9]+", "IRW", msg, ignore.case = TRUE)
  msg <- gsub("\\s+", " ", trimws(msg))
  msg
}

#' User-facing message when a table is missing from all IRW warehouses
#'
#' @param table_name Table name.
#' @keywords internal
#' @noRd
.irw_table_not_found_message <- function(table_name) {
  paste0("\nTable ", shQuote(table_name), " does not exist in IRW.")
}

#' User-facing message for Redivis authentication failures
#'
#' @keywords internal
#' @noRd
.irw_auth_error_message <- function() {
  paste0(
    "\nRedivis authentication failed. ",
    "Sign in via the browser when prompted, or see the package README troubleshooting section."
  )
}

#' Handle a Redivis error while searching datasources
#'
#' @return \code{NULL} to try the next datasource, or invokes \code{stop()}.
#' @keywords internal
#' @noRd
.irw_handle_datasource_error <- function(msg, table_name, ds_list) {
  err_type <- .irw_redivis_error_type(msg)

  if (err_type == "invalid") {
    stop(
      paste("\nTable", shQuote(table_name), "cannot be fetched due to an invalid format."),
      call. = FALSE
    )
  }
  if (err_type == "auth") {
    stop(.irw_auth_error_message(), call. = FALSE)
  }
  if (err_type == "not_found") {
    return(invisible(NULL))
  }
  if (length(ds_list) > 1L) {
    return(invisible(NULL))
  }

  clean <- .irw_sanitize_redivis_error(msg)
  stop(paste("\nAn error occurred while accessing IRW:", clean), call. = FALSE)
}
