#' Licence on a table's item wording
#'
#' Most IRW item text carries no licence of its own: the response-data licence
#' does not extend to the instrument, and inclusion implies no licence to reuse
#' it. A minority of tables are different: their wording was released by its
#' source under an open licence (CC BY-SA 4.0, CC BY-NC 4.0, ...), and IRW
#' republishes it under that licence, with attribution and a note of its
#' changes. This function returns that record for one table, or \code{NULL}
#' when the wording carries no licence of its own.
#'
#' The record is the IRW repository's \code{itemtext/itemtext_licences.csv},
#' fetched once per session. \code{irw_itemtext()} prints the same line after
#' fetching a licensed table.
#'
#' @param table_name Character. The IRW table name (case-insensitive).
#'
#' @return A one-row data frame with columns \code{table}, \code{licence},
#'   \code{licence_url}, \code{attribution}, \code{changes} and \code{basis},
#'   or \code{NULL} if the wording carries no licence of its own (or the record
#'   could not be fetched, in which case a warning says so).
#'
#' @examples
#' \donttest{
#'   irw_itemtext_licence("iowa_gambling_task")
#' }
#'
#' @export
irw_itemtext_licence <- function(table_name) {
  if (missing(table_name) || !is.character(table_name) || length(table_name) != 1) {
    stop("`table_name` must be a single table name.", call. = FALSE)
  }
  licences <- .irw_itemtext_licences()
  if (is.null(licences)) return(NULL)
  hit <- licences[tolower(licences$table) == tolower(table_name), , drop = FALSE]
  if (nrow(hit) == 0) return(NULL)
  rownames(hit) <- NULL
  hit[1, , drop = FALSE]
}

.irw_itemtext_licences_url <- paste0(
  "https://raw.githubusercontent.com/ben-domingue/irw/main/",
  "itemtext/itemtext_licences.csv"
)

# Fetch and cache the wording-licence table for this session. NULL (with one
# warning per session) when it cannot be fetched: a missing record must not
# look like "no licence", so callers distinguish NULL-from-failure by the
# warning rather than silently treating the table as unlicensed.
.irw_itemtext_licences <- function() {
  if (!is.null(.irw_env$itemtext_licences)) return(.irw_env$itemtext_licences)
  if (isTRUE(.irw_env$itemtext_licences_failed)) return(NULL)
  response <- tryCatch(
    httr::GET(.irw_itemtext_licences_url, httr::timeout(15)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::http_error(response)) {
    .irw_env$itemtext_licences_failed <- TRUE
    warning(
      "Could not fetch the item-text licence record from ", .irw_itemtext_licences_url,
      "; wording licences will not be reported this session.",
      call. = FALSE
    )
    return(NULL)
  }
  licences <- tryCatch(
    utils::read.csv(
      text = httr::content(response, as = "text", encoding = "UTF-8"),
      stringsAsFactors = FALSE, check.names = FALSE
    ),
    error = function(e) NULL
  )
  needed <- c("table", "licence", "licence_url", "attribution", "changes", "basis")
  if (is.null(licences) || !all(needed %in% names(licences))) {
    .irw_env$itemtext_licences_failed <- TRUE
    warning("The item-text licence record has an unexpected shape; wording licences will not be reported this session.",
            call. = FALSE)
    return(NULL)
  }
  .irw_env$itemtext_licences <- licences
  licences
}

# One message per licensed table, printed by irw_itemtext() after the fetch.
.itemtext_licence_message <- function(table_name) {
  hit <- irw_itemtext_licence(table_name)
  if (is.null(hit)) return(invisible(NULL))
  message(
    "Item wording licence for ", hit$table, ": ", hit$licence, " (", hit$licence_url, ")\n",
    "  Source: ", hit$attribution, "\n",
    "  Changes: ", hit$changes, "\n",
    "  This licence covers the item text as published here; it implies no licence\n",
    "  to reuse the original source instrument."
  )
  invisible(hit)
}
