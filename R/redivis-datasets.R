#' Retry with Exponential Backoff (Deprecated)
#'
#' This function is a no-op placeholder.
#'
#' @param expr A function that executes the API call.
#' @param ... Ignored.
#' @return The result of evaluating `expr()`.
#' @keywords internal
#' @noRd
.retry_with_backoff <- function(expr, ...) {
  if (is.function(expr)) {
    expr()
  } else {
    stop("`.retry_with_backoff()` now expects a function argument, e.g. `.retry_with_backoff(function() expr)`")
  }
}


# helper for multiple redivis datasets
.irw_sources <- c("core", "nom", "sim", "comp")

#' Resolve data source from \code{source} or deprecated \code{nom}/\code{sim}/\code{comp}
#'
#' @param source Character. One of \code{"core"}, \code{"nom"}, \code{"sim"}, \code{"comp"}.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#' @return Single character string from \code{.irw_sources}.
#' @keywords internal
#' @noRd
.irw_resolve_source <- function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
  if (isTRUE(sim) || isTRUE(comp) || isTRUE(nom)) {
    warning(
      "Arguments 'sim', 'comp', and 'nom' are deprecated and will be removed in a future release. ",
      "Use source = \"sim\", source = \"comp\", or source = \"nom\" instead.",
      call. = FALSE
    )
    if (isTRUE(sim)) source <- "sim"
    else if (isTRUE(comp)) source <- "comp"
    else source <- "nom"
  }
  match.arg(source, .irw_sources)
}

#' Clear session caches that depend on live IRW warehouse table listings
#'
#' @keywords internal
#' @noRd
.irw_clear_live_table_caches <- function() {
  for (nm in c(
    "datasource_list",
    "metadata_tibble",
    "metadata_version",
    "biblio_tibble",
    "biblio_version",
    "tags_tibble",
    "tags_version",
    "nominal_tags_tibble",
    "nominal_tags_version",
    "core_live_catalog_fingerprint"
  )) {
    if (exists(nm, envir = .irw_env)) {
      rm(list = nm, envir = .irw_env)
    }
  }
}

#' Invalidate caches when the configured core warehouse list changes
#'
#' @keywords internal
#' @noRd
.irw_sync_core_warehouse_caches <- function() {
  fp <- .irw_core_warehouse_fingerprint()
  if (!exists("core_warehouse_fingerprint", envir = .irw_env) ||
      !identical(.irw_env$core_warehouse_fingerprint, fp)) {
    .irw_env$core_warehouse_fingerprint <- fp
    .irw_clear_live_table_caches()
  }
}

#' Fingerprint of core warehouse config plus each warehouse version tag
#'
#' Used so metadata filtered to live tables refreshes when a warehouse updates
#' even if \code{irw_meta} has not changed.
#'
#' @keywords internal
#' @noRd
.irw_core_live_catalog_fingerprint <- function() {
  ds_list <- .initialize_datasource(source = "core")
  tags <- vapply(ds_list, function(ds) {
    ds$get()
    tag <- ds$properties$version$tag
    if (is.null(tag) || length(tag) == 0L) "" else as.character(tag[[1]])
  }, character(1))
  paste(.irw_core_warehouse_fingerprint(), paste(tags, collapse = ","), sep = "|")
}

#' Return datasources in preferred search order (newest core warehouse first)
#'
#' @param ds_list List of Redivis dataset objects.
#' @param source Resolved source name.
#' @keywords internal
#' @noRd
.irw_order_datasources <- function(ds_list, source) {
  if (source == "core" && length(ds_list) > 1L) {
    rev(ds_list)
  } else {
    ds_list
  }
}

#' Open every core warehouse, skipping any that are currently unavailable
#'
#' A freshly created warehouse has no released version yet, so opening it with a
#' read-only token fails. One such warehouse must not take down every IRW
#' lookup, so unavailable warehouses are dropped with a warning. An
#' authentication failure applies to all of them, so that still stops.
#'
#' @return A non-empty list of Redivis dataset objects, in config order.
#' @keywords internal
#' @noRd
.irw_open_core_datasources <- function() {
  opened <- list()
  failures <- character(0)

  for (spec in .irw_datasource_specs$core) {
    ds <- tryCatch(
      .irw_open_dataset(spec),
      error = function(e) {
        msg <- conditionMessage(e)
        if (.irw_redivis_error_type(msg) == "auth") {
          stop(.irw_auth_error_message(), call. = FALSE)
        }
        failures <<- c(failures, .irw_sanitize_redivis_error(msg))
        NULL
      }
    )
    if (!is.null(ds)) {
      opened <- c(opened, list(ds))
    }
  }

  if (length(opened) == 0L) {
    stop(
      paste(
        "\nAn error occurred while accessing IRW:",
        paste(unique(failures), collapse = "; ")
      ),
      call. = FALSE
    )
  }

  if (length(failures) > 0L) {
    warning(
      "Skipping ", length(failures), " unavailable IRW datasource(s): ",
      paste(unique(failures), collapse = "; "),
      call. = FALSE
    )
  }

  opened
}

#' Initialize Redivis Datasource(s)
#'
#' Returns a list of Redivis dataset objects based on the selected source:
#' - If \code{source = "sim"}, returns the IRW simulation dataset (\code{irw_simsyn:0btg})
#' - If \code{source = "comp"}, returns the IRW competition dataset (\code{irw_competitions:cmd7})
#' - If \code{source = "nom"}, returns the IRW nominal dataset (\code{irw_nominal:614n})
#' - If \code{source = "core"} (default), returns all main IRW production datasets
#'
#' @param source Character. One of \code{"core"}, \code{"nom"}, \code{"sim"}, \code{"comp"}.
#'   Default is \code{"core"}.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#'
#' @return A list of one or more Redivis dataset objects.
#' @keywords internal
#' @noRd
.initialize_datasource <- function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)

  if (source == "core") {
    .irw_sync_core_warehouse_caches()
    if (!exists("datasource_list", envir = .irw_env) || is.null(.irw_env$datasource_list)) {
      .irw_env$datasource_list <- .irw_open_core_datasources()
    }
    return(.irw_env$datasource_list)
  }

  cache_key <- .irw_single_datasource_cache_key(source)
  if (!exists(cache_key, envir = .irw_env) || is.null(.irw_env[[cache_key]])) {
    .irw_env[[cache_key]] <- .irw_open_dataset(.irw_datasource_specs[[source]][[1L]])
  }
  list(.irw_env[[cache_key]])
}

#' Table names currently listed in Redivis for a source
#'
#' @param source Character. One of \code{"core"}, \code{"nom"}, \code{"sim"}, \code{"comp"}.
#' @return Lowercase character vector of unique table names.
#' @keywords internal
#' @noRd
.irw_live_table_names <- function(source = "core") {
  ds_list <- .irw_order_datasources(.initialize_datasource(source = source), source = source)
  names_list <- lapply(ds_list, function(ds) {
    tryCatch(
      {
        ds$get()
        tables <- ds$list_tables()
        vapply(tables, function(tbl) tbl$name, character(1))
      },
      error = function(e) {
        msg <- conditionMessage(e)
        err_type <- .irw_redivis_error_type(msg)
        if (err_type == "auth") {
          stop(.irw_auth_error_message(), call. = FALSE)
        }
        if (err_type != "not_found") {
          warning(
            "Could not list tables from one IRW datasource: ",
            .irw_sanitize_redivis_error(msg),
            call. = FALSE
          )
        }
        character(0)
      }
    )
  })
  unique(tolower(unlist(names_list, use.names = FALSE)))
}

#' Keep metadata/biblio/tags rows whose tables exist in Redivis
#'
#' @param df Data frame with a \code{table} column (or \code{table_col}).
#' @param source Character. IRW data source passed to \code{.initialize_datasource()}.
#' @param table_col Name of the table-name column.
#' @keywords internal
#' @noRd
.irw_filter_rows_to_live_tables <- function(df, source = "core", table_col = "table") {
  if (!table_col %in% names(df) || nrow(df) == 0L) {
    return(df)
  }
  live <- .irw_live_table_names(source = source)
  df[tolower(df[[table_col]]) %in% live, , drop = FALSE]
}

#' Deduplicate table listing rows, keeping the first row per table name
#'
#' Callers should pass rows ordered with preferred warehouse first (newest core warehouse).
#'
#' @param tables_info Data frame with a \code{name} column.
#' @keywords internal
#' @noRd
.irw_dedup_table_info <- function(tables_info) {
  if (!"name" %in% names(tables_info) || nrow(tables_info) == 0L) {
    return(tables_info)
  }
  keep <- !duplicated(tables_info$name)
  out <- tables_info[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}
