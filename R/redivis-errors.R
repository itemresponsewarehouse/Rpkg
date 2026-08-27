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

#' Whether a Redivis/BigQuery error indicates an export quota or rate limit
#'
#' Redivis caps exported bytes per rolling window; the client reports this as a
#' 400 invalid_request. Rate limiting and resource exhaustion are grouped here
#' because they share the same user response: wait or use a server-side query.
#'
#' @param msg Error message text.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.irw_is_quota_error <- function(msg) {
  grepl(
    "cannot export more than|export(ed)? .{0,40}(within|in the past)|quota|rate.?limit|too many requests|resource_exhausted|\\b429\\b",
    msg,
    ignore.case = TRUE
  )
}

#' Classify a Redivis client error
#'
#' @param msg Error message text.
#' @return One of \code{"quota"}, \code{"invalid"}, \code{"not_found"}, \code{"auth"},
#'   \code{"other"}.
#' @keywords internal
#' @noRd
.irw_redivis_error_type <- function(msg) {
  # Checked first: quota errors arrive as invalid_request and would otherwise be
  # classified as a malformed table.
  if (.irw_is_quota_error(msg)) {
    return("quota")
  }
  if (grepl("invalid_request_error", msg, ignore.case = TRUE)) {
    return("invalid")
  }
  if (.irw_is_not_found_error(msg)) {
    return("not_found")
  }
  if (grepl(
    paste0(
      "unauthorized|unauthenticated|not authenticated|authentication|permission_denied|",
      "access_denied|forbidden|invalid_grant|invalid_token|login required|not authorized|",
      "must be logged in|\\b401\\b"
    ),
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

#' User-facing message for Redivis export quota / rate limit failures
#'
#' @param msg Underlying error message text.
#' @keywords internal
#' @noRd
.irw_quota_error_message <- function(msg) {
  paste0(
    "\nRedivis export quota or rate limit reached, so the table could not be downloaded.\n",
    "This is an account-wide limit on exported bytes, not a problem with the table.\n",
    "Use `irw_table_sets()` for item/response value sets and per-item summaries: it runs a\n",
    "server-side query and does not count against the export quota.\n",
    "Underlying error: ", .irw_sanitize_redivis_error(msg)
  )
}

#' Handle a Redivis error while searching datasources
#'
#' Errors that are specific to this table and terminal (invalid format), or that
#' apply to every datasource (auth, quota), stop immediately. \code{"not_found"}
#' means "try the next datasource". Any other error is recorded in
#' \code{errors} so the caller can re-raise it instead of reporting the table as
#' missing once every datasource has been tried.
#'
#' @param msg Error message text.
#' @param table_name Table name.
#' @param ds_list List of datasources being searched.
#' @param errors Optional environment with a \code{msgs} character vector, used
#'   to collect unclassified errors across datasources.
#' @return \code{NULL} to try the next datasource, or invokes \code{stop()}.
#' @keywords internal
#' @noRd
.irw_handle_datasource_error <- function(msg, table_name, ds_list, errors = NULL) {
  err_type <- .irw_redivis_error_type(msg)

  if (err_type == "quota") {
    stop(.irw_quota_error_message(msg), call. = FALSE)
  }
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

  if (!is.null(errors)) {
    errors$msgs <- c(errors$msgs, msg)
    return(invisible(NULL))
  }
  if (length(ds_list) > 1L) {
    return(invisible(NULL))
  }

  clean <- .irw_sanitize_redivis_error(msg)
  stop(paste("\nAn error occurred while accessing IRW:", clean), call. = FALSE)
}

#' Collector for datasource errors that are neither not-found nor terminal
#'
#' @keywords internal
#' @noRd
.irw_new_error_collector <- function() {
  e <- new.env(parent = emptyenv())
  e$msgs <- character(0)
  e
}

#' Message describing why a datasource search failed, if it was not "missing"
#'
#' @param errors Collector from \code{.irw_new_error_collector()}.
#' @return A character string, or \code{NULL} if only not-found errors occurred.
#' @keywords internal
#' @noRd
.irw_collected_error_message <- function(errors) {
  if (is.null(errors) || length(errors$msgs) == 0L) {
    return(NULL)
  }
  clean <- unique(vapply(errors$msgs, .irw_sanitize_redivis_error, character(1), USE.NAMES = FALSE))
  paste("\nAn error occurred while accessing IRW:", paste(clean, collapse = "; "))
}
