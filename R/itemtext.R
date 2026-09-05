#' List IRW tables with available item text metadata
#'
#' Returns a character vector of IRW table names for which item text is available.
#'
#' @return A sorted character vector of IRW table names with item text metadata.
#'
#' @section Disclaimer:
#' IRW item text is reconstructed from published articles, codebooks, and
#' supplementary materials using a largely automated pipeline with partial human
#' review, and is provided \strong{for research purposes only}. We make no
#' guarantee that the text is accurate, complete, or correctly aligned with the
#' \code{item} identifiers in the corresponding IRW response data; the original
#' source document is always authoritative. Users are responsible for verifying
#' item text against that source before drawing substantive conclusions.
#' Documented discrepancies are listed at
#' \url{https://itemresponsewarehouse.org/itemtext_issues.html}.
#'
#' Note also that the licenses recorded for IRW response data do not extend to
#' the instruments themselves. Inclusion of item text here implies no license to
#' reuse an instrument; copyright and related rights remain with the original
#' rights holders, and users are responsible for obtaining any permissions their
#' intended use requires.
#'
#' @examples
#' \dontrun{
#'   irw_list_itemtext_tables()
#' }
#'
#' @export
irw_list_itemtext_tables <- function() {
  if (!identical(getOption("irw.itemtext_disclaimer"), FALSE)) .itemtext_disclaimer()
  # The union across every item text shard, deduplicated. Building the index is
  # the same listing pass, and it also populates `itemtext_table_names`.
  .irw_itemtext_table_index()
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
#' @section Disclaimer:
#' IRW item text is reconstructed from published articles, codebooks, and
#' supplementary materials using a largely automated pipeline with partial human
#' review, and is provided \strong{for research purposes only}. We make no
#' guarantee that the text is accurate, complete, or correctly aligned with the
#' \code{item} identifiers in the corresponding IRW response data; the original
#' source document is always authoritative. Users are responsible for verifying
#' item text against that source before drawing substantive conclusions.
#' Documented discrepancies are listed at
#' \url{https://itemresponsewarehouse.org/itemtext_issues.html}.
#'
#' Note also that the licenses recorded for IRW response data do not extend to
#' the instruments themselves. Inclusion of item text here implies no license to
#' reuse an instrument; copyright and related rights remain with the original
#' rights holders, and users are responsible for obtaining any permissions their
#' intended use requires.
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
  if (!identical(getOption("irw.itemtext_disclaimer"), FALSE)) .itemtext_disclaimer()
  .fetch_itemtext_table(table_name)
}

.itemtext_disclaimer <- function() {
  if (isTRUE(.irw_env$itemtext_disclaimer_shown)) return(invisible(NULL))
  .irw_env$itemtext_disclaimer_shown <- TRUE
  message(
    "Note: IRW item text is reconstructed from published sources using a largely\n",
    "automated pipeline and is provided for research purposes only. We make no\n",
    "guarantee as to its accuracy, completeness, or alignment with the `item`\n",
    "identifiers in the response data; verify against the original source.\n",
    "Inclusion here implies no license to reuse an instrument; copyright remains\n",
    "with the original rights holders.\n",
    "See https://itemresponsewarehouse.org/itemtext_issues.html\n",
    "(silence with options(irw.itemtext_disclaimer = FALSE))"
  )
  invisible(NULL)
}

