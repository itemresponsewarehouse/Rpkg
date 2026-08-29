#' List IRW Collections
#'
#' Returns the collection registry: one row per collection, with its kind,
#' definition, the rule that produced it, and how complete its coverage is.
#'
#' Collections are labelled groupings of IRW tables — study designs (`rct`,
#' `clustered`, `q_matrix`), instrument families (`big_five`, `promis`) and
#' constructs (`depression`, `math`). A table can belong to any number of them.
#'
#' @section Coverage:
#' `coverage` says how much of the warehouse the collection's rule actually
#' searched, and it matters when you need "all" of something:
#' \describe{
#'   \item{`metadata-complete`}{The rule read `irw_metadata()`, so it saw every
#'     documented table. Note this is not the same as the whole warehouse —
#'     `irw_metadata()` itself has gaps.}
#'   \item{`tagged-subset-only`}{The rule read the tags table, which covers
#'     roughly 62% of documented tables, and coverage is much lower in the newer
#'     warehouses. These collections are biased toward older tables and should
#'     not be described as exhaustive.}
#'   \item{`curated-only`}{Every member was chosen by hand.}
#' }
#'
#' @param kind Optional character vector. Filter to `"design"`, `"instrument"`
#'   and/or `"construct"`.
#' @return A tibble, one row per collection.
#' @seealso [irw_collection()], [irw_collection_members()], [irw_filter()]
#' @examples
#' \dontrun{
#' irw_collections()
#' irw_collections(kind = "instrument")
#' }
#' @export
irw_collections <- function(kind = NULL) {
  reg <- .fetch_collections_table()

  ##`n_tables` in the published registry is the count at build time. Membership
  ##is live-filtered on fetch, so recompute rather than pass a number through
  ##that may now overstate what you can actually retrieve.
  members <- .fetch_collection_members_table()
  if (nrow(members) > 0L) {
    live <- table(members$collection)
    reg$n_tables <- as.integer(live[match(reg$collection, names(live))])
    reg$n_tables[is.na(reg$n_tables)] <- 0L
  }

  if (!is.null(kind)) {
    valid <- unique(reg$kind)
    unknown <- setdiff(kind, valid)
    if (length(unknown) > 0L) {
      stop("Unknown kind(s): ", paste(sprintf("\"%s\"", unknown), collapse = ", "),
           ". Valid: ", paste(sprintf("\"%s\"", valid), collapse = ", "), ".",
           call. = FALSE)
    }
    reg <- reg[reg$kind %in% kind, , drop = FALSE]
  }

  reg[order(reg$kind, reg$collection), , drop = FALSE]
}


#' Get the Tables in an IRW Collection
#'
#' Returns the names of every IRW table in one collection, so they can be passed
#' straight to [irw_fetch()], [irw_save_bibtex()] or any other vectorised
#' accessor.
#'
#' Prints the collection's definition and, when the collection does not cover
#' the whole warehouse, says so — see the Coverage section of
#' [irw_collections()]. Suppress with `quiet = TRUE`.
#'
#' @param name Character. A single collection name, e.g. `"big_five"`.
#' @param quiet Logical. Suppress the definition/coverage message.
#' @return A sorted character vector of table names.
#' @seealso [irw_collections()], [irw_filter()]
#' @examples
#' \dontrun{
#' tabs <- irw_collection("depression")
#' irw_save_bibtex(tabs, output_file = "depression.bib")
#'
#' # Compose with the numeric filters
#' irw_filter(collection = "depression", n_participants = c(500, Inf))
#'
#' # Cross-collection: math assessments administered in booklets
#' intersect(irw_collection("math"), irw_collection("multistage"))
#' }
#' @export
irw_collection <- function(name, quiet = FALSE) {
  if (missing(name) || !is.character(name) || length(name) != 1L || is.na(name)) {
    stop("`name` must be a single collection name. See `irw_collections()`.",
         call. = FALSE)
  }

  members <- .fetch_collection_members_table()
  reg <- .fetch_collections_table()

  if (!name %in% reg$collection) {
    ##A wrong slug is the common mistake, so spend a line suggesting the
    ##intended one rather than only saying no.
    near <- reg$collection[agrepl(name, reg$collection, max.distance = 0.34, ignore.case = TRUE)]
    hint <- if (length(near) > 0L) {
      paste0("\nDid you mean: ", paste(sprintf("\"%s\"", near), collapse = ", "), "?")
    } else {
      paste0("\nSee `irw_collections()` for the ", nrow(reg), " available.")
    }
    stop("No collection named \"", name, "\".", hint, call. = FALSE)
  }

  tables <- sort(unique(members$table[members$collection == name]))

  if (!quiet) {
    row <- reg[reg$collection == name, , drop = FALSE]
    message(sprintf("%s: %d table%s", name, length(tables),
                    if (length(tables) == 1L) "" else "s"))
    if (nrow(row) > 0L && !is.na(row$definition[1])) message("  ", row$definition[1])
    if (nrow(row) > 0L && !identical(row$coverage[1], "metadata-complete")) {
      message("  Coverage: ", row$coverage[1],
              " -- this collection does not search the whole warehouse; ",
              "see `irw_collections()`.")
    }
  }

  tables
}


#' IRW Collection Membership
#'
#' The long membership table: one row per (table, collection) pair, with the
#' `basis` on which that membership was decided — a rule expression, or a
#' curator. Use this to ask the inverse question: what collections is a given
#' table in?
#'
#' @param tables Optional character vector. Restrict to these table names.
#' @param collection Optional character vector. Restrict to these collections.
#' @return A tibble with columns `table`, `collection`, `basis`.
#' @seealso [irw_collections()], [irw_collection()]
#' @examples
#' \dontrun{
#' irw_collection_members(tables = "frac20")
#' irw_collection_members(collection = "q_matrix")
#' }
#' @export
irw_collection_members <- function(tables = NULL, collection = NULL) {
  members <- .fetch_collection_members_table()

  if (!is.null(tables)) {
    members <- members[tolower(members$table) %in% tolower(tables), , drop = FALSE]
  }
  if (!is.null(collection)) {
    members <- members[members$collection %in% collection, , drop = FALSE]
  }

  members[order(members$table, members$collection), , drop = FALSE]
}
