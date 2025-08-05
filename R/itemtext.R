#' List IRW tables with available item text metadata
#'
#' Returns a character vector of IRW table names for which item text is available.
#'
#' @return A sorted character vector of IRW table names with item text metadata.
#'
#' @examples
#' \dontrun{
#'   irw_list_itemtext_tables()
#' }
#'
#' @export
irw_list_itemtext_tables <- function() {
  if (!exists("itemtext_table_names", envir = .irw_env) || is.null(.irw_env$itemtext_table_names)) {
    dataset <- .get_irw_itemtext_dataset()
    tables <- dataset$list_tables()
    names <- vapply(tables, function(table) table$properties$name, character(1))
    .irw_env$itemtext_table_names <- sort(sub("__items$", "", names))
  }
  .irw_env$itemtext_table_names
}

#' Retrieve item text metadata for an IRW table
#'
#' Returns item-level text metadata for a given IRW table, if available.
#' If no item text table is available for the specified table, the function returns \code{NULL} 
#' and prints a message.
#'
#' @param table_name Character. The name of the IRW table to look up (e.g., \code{"gilbert_meta_49"}).
#'
#' @return A tibble containing item text metadata, or \code{NULL} if unavailable.
#'
#' @examples
#' \dontrun{
#'   irw_itemtext("gilbert_meta_49")
#' }
#'
#' @export
irw_itemtext <- function(table_name) {
  if (missing(table_name)) {
    stop(
      "Please provide the IRW table name to look up.\n",
      "Tip: Use `irw_list_itemtext_tables()` to see available tables.",
      call. = FALSE
    )
  }
  .fetch_itemtext_table(table_name)
}
