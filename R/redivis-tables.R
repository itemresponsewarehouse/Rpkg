
#' Fetch Table from Redivis
#'
#' Retrieves a specified table from Redivis with automatic retry logic.
#' Used internally to obtain a Redivis table object, which can later be converted to a tibble.
#'
#' @param name A character string specifying the table name.
#' @param source Character. One of \code{"core"}, \code{"nom"}, \code{"sim"}, \code{"comp"}.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#'
#' @return A Redivis table object. The returned object has attribute \code{dataset_version} attached.
#' @keywords internal
.fetch_redivis_table <- function(name, source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
  if (!is.character(name) || length(name) != 1) stop("The 'name' parameter must be a single character string.")
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)
  ds_list <- .initialize_datasource(source = source)
  
  for (ds in ds_list) {
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
        msg <- conditionMessage(e)
        # Redivis may surface "not_found_error" in JSON; BigQuery often returns "Not found: ..."
        if (grepl("not_found_error", msg, ignore.case = TRUE)) return(NULL)
        if (grepl("not found:", msg, ignore.case = TRUE)) return(NULL)
        if (grepl("invalid_request_error", msg, ignore.case = TRUE)) {
          stop(paste("\nTable", shQuote(name), "cannot be fetched due to an invalid format."), call. = FALSE)
        }
        stop(paste("\nAn unknown error occurred:", msg), call. = FALSE)
      }
    )
    if (!is.null(result)) return(result)
  }
  
  stop(paste("\nTable", shQuote(name), "does not exist in the IRW database."), call. = FALSE)
}