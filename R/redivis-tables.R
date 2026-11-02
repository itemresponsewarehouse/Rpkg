
#' Fetch Table from Redivis
#'
#' Retrieves a specified table from Redivis with automatic retry logic.
#' Used internally to obtain a Redivis table object, which can later be converted to a tibble.
#'
#' @param name A character string specifying the table name.
#' @param source Character. The data family, one of \code{"core"}, \code{"nom"}, \code{"sim"}, \code{"comp"}.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#'
#' @return A Redivis table object. The returned object has attribute \code{dataset_version} attached.
#' @keywords internal
.fetch_redivis_table <- function(name, source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
  if (!is.character(name) || length(name) != 1) stop("The 'name' parameter must be a single character string.")
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)
  ds_list <- .irw_shard_first(
    .irw_order_datasources(.initialize_datasource(source = source), source = source),
    name = name, source = source, build = FALSE
  )
  errors <- .irw_new_error_collector()

  i <- 0L
  while (i < length(ds_list)) {
    i <- i + 1L
    if (i == 2L) {
      ds_list <- .irw_shard_first_after_miss(ds_list, name = name, source = source)
    }
    ds <- ds_list[[i]]
    ds$get()
    result <- tryCatch(
      {
        withCallingHandlers({
          tbl <- ds$table(name)
          tbl$get()
          attr(tbl, "dataset_version") <- ds$properties$version$tag
          tbl
        },
        warning = function(w) {
          if (grepl("No reference id was provided for the table", conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        })
      },
      error = function(e) {
        .irw_handle_datasource_error(conditionMessage(e), table_name = name, ds_list = ds_list, errors = errors)
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }

  failure <- .irw_collected_error_message(errors)
  if (!is.null(failure)) {
    stop(failure, call. = FALSE)
  }

  pinned <- .irw_pinned_not_found_message(name, source)
  if (!is.null(pinned)) {
    stop(pinned, call. = FALSE)
  }

  stop(.irw_missing_table_message(name, source = source), call. = FALSE)
}
