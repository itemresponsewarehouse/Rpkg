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

#' User-facing message when a table is listed in the metadata but absent
#'
#' The published metadata is the authority for which core tables exist
#' (ben-domingue/irw#3127), so a table can be listed and no longer be in the
#' warehouse for the two to seven days between a withdrawal and the next
#' \code{irw_meta} publish. Saying only "does not exist in IRW" for a table
#' the user has just seen in a listing is the worse half of that trade, and
#' explaining it is the load-bearing part of the ruling, not a nicety.
#'
#' @param table_name Table name.
#' @keywords internal
#' @noRd
.irw_metadata_only_table_message <- function(table_name) {
  paste0(
    "\nTable ", shQuote(table_name), " is in the IRW metadata but not in the ",
    "warehouse. It was most likely withdrawn after the metadata was last ",
    "published; the listing catches up on the next pipeline run."
  )
}

#' Whether a name is listed in the core metadata
#'
#' Reads the table-to-shard map piece A already builds, so this costs nothing
#' once a session has it. Never raises and never warns: it only chooses which
#' of two messages to show, and a metadata read that fails must leave the
#' original not-found message standing rather than replace one error with
#' another.
#'
#' @param table_name Table name.
#' @param source IRW data source; only \code{"core"} has the map.
#' @keywords internal
#' @noRd
.irw_table_listed_in_metadata <- function(table_name, source = "core") {
  if (!identical(source, "core")) {
    return(FALSE)
  }
  map <- tryCatch(.irw_core_shard_map(build = TRUE), error = function(e) NULL)
  if (is.null(map) || length(map) == 0L) {
    return(FALSE)
  }
  tolower(table_name) %in% names(map)
}

#' User-facing message for a table that is missing, with the reason if known
#'
#' @param table_name Table name.
#' @param source IRW data source.
#' @keywords internal
#' @noRd
.irw_missing_table_message <- function(table_name, source = "core") {
  if (.irw_table_listed_in_metadata(table_name, source = source)) {
    return(.irw_metadata_only_table_message(table_name))
  }
  .irw_table_not_found_message(table_name)
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

#' Whether Redivis credentials are available without a login prompt
#'
#' Mirrors the redivis client's own check: an API token in the environment, a
#' cached-credentials file, or credentials already loaded this session. The
#' client is read, never modified. Older redivis builds (before 0.12.14) fixed
#' the credentials path when the package was built, so that path is checked
#' too.
#'
#' @keywords internal
#' @noRd
.irw_redivis_credentials_available <- function() {
  if (nzchar(Sys.getenv("REDIVIS_API_TOKEN"))) {
    return(TRUE)
  }
  if (file.exists(file.path(Sys.getenv("HOME"), ".redivis", "r_credentials"))) {
    return(TRUE)
  }
  auth_vars <- tryCatch(
    get("auth_vars", envir = asNamespace("redivis")),
    error = function(e) NULL
  )
  if (is.environment(auth_vars)) {
    if (!is.null(auth_vars$cached_credentials)) {
      return(TRUE)
    }
    cf <- auth_vars$credentials_file
    if (is.character(cf) && length(cf) == 1 && file.exists(cf)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Check whether irw can read from the warehouse without a login prompt
#'
#' Fetching and listing IRW tables needs no Redivis login: the warehouse is
#' public, and \pkg{redivis} 0.12.14 and later send those requests
#' anonymously. A login is still needed for the server-side queries behind
#' \code{\link{irw_table_sets}}, and for every warehouse call on an older
#' \pkg{redivis}, which authenticates unconditionally. In an interactive
#' session irw opens the browser login when one is needed; in a script or other
#' non-interactive session it stops with instructions instead. This function
#' reports whether the \pkg{redivis} client and a login are already in place,
#' so code can skip the calls that would otherwise stop.
#'
#' @return \code{TRUE} if \pkg{redivis} is installed and credentials are
#'   available (a \code{REDIVIS_API_TOKEN} environment variable or a cached
#'   login), otherwise \code{FALSE}.
#' @examples
#' irw_has_credentials()
#' @export
irw_has_credentials <- function() {
  requireNamespace("redivis", quietly = TRUE) &&
    .irw_redivis_credentials_available()
}

#' Whether the installed redivis client reads public data anonymously
#'
#' redivis 0.12.14 (redivis/redivis-r#47) stopped attaching a bearer token to
#' every request: with no credentials on hand the request goes out anonymously,
#' and the client logs in only if the server rejects it. Before that the client
#' fetched a token first, so in a session with no cached login the very first
#' read started the browser flow and waited. Redivis then opened the
#' dataset-level endpoints to anonymous reads (redivis/redivis-r#49), so from
#' 0.12.14 on, opening a dataset, listing its tables and exporting a table all
#' succeed with no account at all.
#'
#' @keywords internal
#' @noRd
.irw_redivis_supports_anonymous <- function() {
  if (!requireNamespace("redivis", quietly = TRUE)) {
    return(FALSE)
  }
  isTRUE(tryCatch(
    utils::packageVersion("redivis") >= "0.12.14",
    error = function(e) FALSE
  ))
}

#' Instructions for a session that has no cached Redivis login
#'
#' @param what Text naming what needs the login, ending in a full stop.
#' @keywords internal
#' @noRd
.irw_no_login_message <- function(what) {
  paste0(
    "\n", what, "\n",
    "Run any irw function once in an interactive R session to log in through ",
    "the browser; the login is cached for later scripts.\n",
    "For a server or CI job, set the REDIVIS_API_TOKEN environment variable ",
    "to a Redivis API token instead."
  )
}

# The install command we print has to work where it is pasted. A fresh
# non-interactive session (Rscript, a CI job) has no CRAN mirror set:
# getOption("repos") is the placeholder "@CRAN@", and install.packages() then
# stops with "trying to use CRAN without setting a mirror". So resolve the
# mirror here and print a literal URL rather than a getOption() call
# (reported by a user hitting it under Rscript).
.irw_redivis_install_cmd <- function() {
  cran <- as.character(getOption("repos")["CRAN"])
  if (length(cran) != 1 || is.na(cran) || !nzchar(cran) || cran == "@CRAN@") {
    cran <- "https://cloud.r-project.org"
  }
  sprintf(
    'install.packages("redivis", repos = c("https://redivis.r-universe.dev", "%s"))',
    cran
  )
}

#' Stop unless the redivis client is installed
#'
#' redivis is not on CRAN, so it lives in Suggests and is installed from
#' r-universe. Every call into \code{redivis::} goes through this check first,
#' rather than failing with "there is no package called 'redivis'".
#'
#' @keywords internal
#' @noRd
.irw_require_redivis_client <- function() {
  if (!requireNamespace("redivis", quietly = TRUE)) {
    stop(
      "\nThis function needs the redivis package, which is not installed.\n",
      "redivis is not on CRAN; install it from r-universe with:\n",
      "  ", .irw_redivis_install_cmd(), "\n\n",
      "The parts of irw that do not download from the warehouse work without it:\n",
      "  irw_simdata(), irw_simdata_comp(), irw_simu_diff(), irw_imv(), irw_predict(),\n",
      "  irw_long2resp(), irw_resp2long(), irw_check_resp(), irw_covariates().",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Stop unless a warehouse read can run without a login prompt
#'
#' The gate every dataset handle passes through. On redivis 0.12.14 and later
#' this is a plain install check: the endpoints behind opening a dataset,
#' listing its tables and exporting one are public and the client asks for them
#' anonymously. An older client authenticates every request, and with no
#' credentials that means a browser login, which is right in an interactive
#' session but hangs a script, a CI job or a CRAN check -- the hang this guard
#' was added for. So the stop stays, narrowed to the clients that need it, and
#' names upgrading as the other way out.
#'
#' @keywords internal
#' @noRd
.irw_require_redivis <- function() {
  .irw_require_redivis_client()
  if (!.irw_redivis_supports_anonymous() && !interactive() &&
        !.irw_redivis_credentials_available()) {
    stop(
      .irw_no_login_message(paste0(
        "Reading from the Item Response Warehouse needs a Redivis login on ",
        "redivis ", utils::packageVersion("redivis"), ", and none was found.\n",
        "redivis 0.12.14 and later read public IRW tables with no account, so ",
        "upgrading is the other way out:\n  ", .irw_redivis_install_cmd()
      )),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Stop unless a server-side Redivis query can run without a login prompt
#'
#' Reading tables is anonymous (see \code{.irw_require_redivis()}), but
#' \code{POST /queries} is not: Redivis rejects it without credentials, and the
#' client answers a 401 by logging in, which in a script waits for a browser
#' that never comes. So the query path keeps the gate that reading no longer
#' needs. Keeping the boundary here, rather than at the dataset handle, is what
#' lets anonymous reading work while \code{R CMD check} still cannot hang.
#'
#' @keywords internal
#' @noRd
.irw_require_redivis_query <- function() {
  .irw_require_redivis_client()
  if (!interactive() && !.irw_redivis_credentials_available()) {
    stop(
      .irw_no_login_message(paste0(
        "This runs a server-side Redivis query, which needs a Redivis login, ",
        "and none was found.\n",
        "Reading tables does not: irw_fetch(), irw_list_tables() and ",
        "irw_info() work without an account."
      )),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
