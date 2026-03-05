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
        redivis::redivis$user("datapages")$dataset("item_response_warehouse_2:epbx")
      )
      lapply(.irw_env$datasource_list, function(ds) ds$get())
    }
    return(.irw_env$datasource_list)
  }
}