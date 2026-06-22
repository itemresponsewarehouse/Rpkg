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

#' Initialize Redivis Datasource(s)
#'
#' Returns a list of Redivis dataset objects based on the selected source:
#' - If \code{source = "sim"}, returns the IRW simulation dataset (\code{irw_simsyn:0btg})
#' - If \code{source = "comp"}, returns the IRW competition dataset (\code{irw_competitions:cmd7})
#' - If \code{source = "nom"}, returns the IRW nominal dataset (\code{irw_nominal:614n})
#' - If \code{source = "core"} (default), returns the main IRW production datasets
#'
#' @param source Character. One of \code{"core"}, \code{"nom"}, \code{"sim"}, \code{"comp"}.
#'   Default is \code{"core"}.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#'
#' @return A list of one or more Redivis dataset objects.
#' @keywords internal
.initialize_datasource <- function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)

  if (source == "sim") {
    if (!exists("sim_datasource", envir = .irw_env) || is.null(.irw_env$sim_datasource)) {
      ds <- redivis::redivis$user("bdomingu")$dataset("irw_simsyn:0btg")
      ds$get()
      .irw_env$sim_datasource <- ds
    }
    return(list(.irw_env$sim_datasource))

  } else if (source == "comp") {
    if (!exists("comp_datasource", envir = .irw_env) || is.null(.irw_env$comp_datasource)) {
      ds <- redivis::redivis$user("bdomingu")$dataset("irw_competitions:cmd7")
      ds$get()
      .irw_env$comp_datasource <- ds
    }
    return(list(.irw_env$comp_datasource))

  } else if (source == "nom") {
    if (!exists("nom_datasource", envir = .irw_env) || is.null(.irw_env$nom_datasource)) {
      ds <- redivis::redivis$user("bdomingu")$dataset("irw_nominal:614n")
      ds$get()
      .irw_env$nom_datasource <- ds
    }
    return(list(.irw_env$nom_datasource))

  } else {
    if (!exists("datasource_list", envir = .irw_env) || is.null(.irw_env$datasource_list)) {
      .irw_env$datasource_list <- list(
        redivis::redivis$user("datapages")$dataset("item_response_warehouse:as2e"),
        redivis::redivis$user("datapages")$dataset("item_response_warehouse_2:epbx"),
        redivis::redivis$user("datapages")$dataset("item_response_warehouse_3:5xaj")
      )
      lapply(.irw_env$datasource_list, function(ds) ds$get())
    }
    return(.irw_env$datasource_list)
  }
}

#' Table names currently listed in Redivis for a source
#'
#' @param source Character. One of \code{"core"}, \code{"nom"}, \code{"sim"}, \code{"comp"}.
#' @return Lowercase character vector of unique table names.
#' @keywords internal
#' @noRd
.irw_live_table_names <- function(source = "core") {
  ds_list <- .initialize_datasource(source = source)
  unique(tolower(unlist(lapply(ds_list, function(ds) {
    ds$get()
    tables <- ds$list_tables()
    vapply(tables, function(tbl) tbl$name, character(1))
  }))))
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