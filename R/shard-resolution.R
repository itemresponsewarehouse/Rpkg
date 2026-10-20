#' Shard resolution: finding the one warehouse a table lives in
#'
#' A core table lives in exactly one of the core warehouses, but nothing in its
#' name says which, so \code{irw_fetch()} has always found out by asking: open
#' each warehouse newest-first, ask it for the table, and keep going until one
#' answers. With seven shards configured -- and shard discovery appending any
#' that released after this package did -- a table in the oldest warehouse
#' costs fourteen round trips to locate, thirteen of which are 404s, against a
#' service that answers 429 under load (ben-domingue/irw#3091).
#'
#' The warehouse a table lives in is already published: the \code{metadata}
#' table in \code{irw_meta} carries a \code{dataset} column naming it, one row
#' per table. Reading that column once per session turns the search into a
#' lookup, and the fetch then opens one shard instead of all of them.
#'
#' Reading it is not free, though -- it is two columns of a 6,000-row table --
#' so when the map is not already in hand the first shard is tried *before* it
#' is read. New tables go to the newest warehouse, which is the first one
#' searched, so the common first fetch of a session pays exactly what it paid
#' before and never reads the map at all. Once that shard has missed, the
#' remaining six are resolved instead of walked. A session that already has the
#' map -- a second fetch, or anything that has called \code{irw_metadata()} --
#' resolves up front and makes one shard's worth of calls.
#'
#' What this is careful about:
#'
#' \itemize{
#'   \item It **reorders, never truncates.** The resolved shard is moved to the
#'     front of the list and the rest follow in the order they had. So a map
#'     that is missing a table, or names a shard the table has since left, costs
#'     one wasted round trip and then behaves exactly as before -- including the
#'     not-found message, which is produced by the same code path on the same
#'     list. \code{.irw_handle_datasource_error()} also decides whether a
#'     non-404 error is terminal from \code{length(ds_list)}, and reordering
#'     leaves that length alone.
#'   \item A table published in two shards still resolves to its newest copy.
#'     Core does that silently today (ARCHITECTURE.md section 2), so the map
#'     keeps the highest-numbered shard when a name appears more than once.
#'   \item It is read from the metadata table **without** the live-table
#'     intersection that \code{.fetch_metadata_table()} applies, because that
#'     intersection is the very thing being avoided -- it lists tables from
#'     every shard. An already-cached metadata tibble is used when there is
#'     one, so a session that has called \code{irw_metadata()} pays nothing.
#'   \item It never raises. A map that cannot be read is \code{NULL} and the
#'     search runs as it always did.
#' }
#'
#' The one case it gets wrong, deliberately: a table present in **two** shards
#' where the metadata is stale about which is newer returns the older copy,
#' where the full search would return the newer. Two shards holding one name is
#' an upload fault rather than a state we keep (the drift report watches for it,
#' and there are none today), and the cost of carrying that case would be the
#' search this removes. Switch the whole thing off with
#' \code{options(irw.shard_resolution = FALSE)} or \code{IRW_SHARD_RESOLUTION=0}.
#'
#' @name shard-resolution
#' @keywords internal
NULL

#' Whether shard resolution is switched on
#'
#' @keywords internal
#' @noRd
.irw_shard_resolution_enabled <- function() {
  opt <- getOption("irw.shard_resolution")
  if (!is.null(opt)) {
    return(isTRUE(opt))
  }
  env <- Sys.getenv("IRW_SHARD_RESOLUTION", unset = "")
  if (nzchar(env)) {
    return(!tolower(env) %in% c("0", "false", "no", "off"))
  }
  TRUE
}

#' Position of a shard within its family
#'
#' The unsuffixed base name is shard 1 and a numeric suffix is its own position,
#' so \code{item_response_warehouse_10} sorts after \code{_2} rather than
#' lexically before it.
#'
#' @param key Dataset key, without a reference id.
#' @return Integer position, \code{NA} if the key is not of that shape.
#' @keywords internal
#' @noRd
.irw_shard_index <- function(key) {
  suffix <- ifelse(grepl("_[0-9]+$", key), sub("^.*_([0-9]+)$", "\\1", key), "1")
  idx <- suppressWarnings(as.integer(suffix))
  ifelse(is.na(idx), 1L, idx)
}

#' Build the table-to-shard map from a metadata tibble
#'
#' Kept apart from the read so the rules can be tested without a network.
#'
#' @param meta Data frame with \code{table} and \code{dataset} columns.
#' @return Named character vector, lowercase table name to dataset key, or
#'   \code{NULL} if \code{meta} cannot supply one.
#' @keywords internal
#' @noRd
.irw_shard_map_from_metadata <- function(meta) {
  if (!is.data.frame(meta) || nrow(meta) == 0L ||
      !all(c("table", "dataset") %in% names(meta))) {
    return(NULL)
  }
  tab <- tolower(as.character(meta$table))
  shard <- .irw_dataset_key(as.character(meta$dataset))
  keep <- !is.na(tab) & nzchar(tab) & !is.na(shard) & nzchar(shard)
  tab <- tab[keep]
  shard <- shard[keep]
  if (length(tab) == 0L) {
    return(NULL)
  }

  # Newest copy wins. Sorted by shard position, the last row for a name is the
  # highest-numbered shard it appears in, which is the copy core resolves to.
  ord <- order(.irw_shard_index(shard), na.last = TRUE)
  tab <- tab[ord]
  shard <- shard[ord]
  last <- !duplicated(tab, fromLast = TRUE)
  stats::setNames(shard[last], tab[last])
}

#' The table-to-shard map, read once per session
#'
#' @param build Whether to read the map when it is not cached yet. \code{FALSE}
#'   asks only what this session already knows, which is what lets the fetch
#'   path use a map it has without paying to build one.
#' @return Named character vector, or \code{NULL} when unavailable.
#' @keywords internal
#' @noRd
.irw_core_shard_map <- function(build = TRUE) {
  if (exists("core_shard_map", envir = .irw_env)) {
    return(.irw_env$core_shard_map)
  }
  if (!build) {
    return(NULL)
  }
  # Errors become NULL -- the search still works -- but warnings must not,
  # which is the trap here: the Redivis client warns about a missing reference
  # id on perfectly good reads, and a `warning = function(w) NULL` handler
  # turned every successful map read into a failure.
  map <- tryCatch(
    withCallingHandlers(
      .irw_read_core_shard_map(),
      warning = function(w) {
        if (grepl("No reference id was provided", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(e) NULL
  )
  .irw_env$core_shard_map <- map
  map
}

#' Read the two columns the map needs
#'
#' Only \code{table} and \code{dataset} are requested, so this is a fraction of
#' the metadata table rather than all of it.
#'
#' @keywords internal
#' @noRd
.irw_read_core_shard_map <- function() {
  if (exists("metadata_tibble", envir = .irw_env) &&
      !is.null(.irw_env$metadata_tibble)) {
    cached <- .irw_shard_map_from_metadata(.irw_env$metadata_tibble)
    if (!is.null(cached)) {
      return(cached)
    }
  }
  dataset <- .irw_open_meta_dataset()
  table <- dataset$table("metadata")
  meta <- .retry_with_backoff(function() {
    table$to_tibble(variables = c("table", "dataset"))
  })
  .irw_shard_map_from_metadata(meta)
}

#' The dataset key of an opened Redivis dataset
#'
#' From \code{properties$scopedReference} ("item_response_warehouse_3:5xaj:v14_0"),
#' not the \code{name} field: once a dataset has been fetched -- and
#' \code{.irw_open_dataset()} always fetches -- \code{name} is the *display*
#' name, so a core shard reads back as "Item Response Warehouse 3" and matches
#' nothing. This is the same trap \code{.irw_parse_dataset_catalog()} documents
#' for catalog rows. The \code{name} field is still the fallback, for a handle
#' that has not been fetched.
#'
#' @return The key, or \code{NA_character_} when neither is usable.
#' @keywords internal
#' @noRd
.irw_opened_dataset_key <- function(ds) {
  key <- tryCatch(ds$properties$scopedReference, error = function(e) NULL)
  if (!is.character(key) || length(key) != 1L) {
    key <- tryCatch(ds$name, error = function(e) NULL)
  }
  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
    return(NA_character_)
  }
  .irw_dataset_key(key)
}

#' Put the shard a table lives in first in the search order
#'
#' @param ds_list Opened datasources, already in search order.
#' @param name Table name being looked for.
#' @param source Resolved source name; only \code{"core"} has a published
#'   table-to-shard column today.
#' @param build Whether the map may be read if this session has not read it
#'   yet. \code{FALSE} is the up-front call, which must not add a read to a
#'   fetch that was about to succeed on its first try.
#' @return \code{ds_list}, with the resolved shard moved to the front when one
#'   could be resolved and is present.
#' @keywords internal
#' @noRd
.irw_shard_first <- function(ds_list, name, source = "core", build = TRUE) {
  if (!identical(source, "core") || length(ds_list) < 2L ||
      !.irw_shard_resolution_enabled()) {
    return(ds_list)
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    return(ds_list)
  }
  map <- .irw_core_shard_map(build = build)
  if (is.null(map)) {
    return(ds_list)
  }
  target <- map[tolower(name)]
  if (is.na(target)) {
    return(ds_list)
  }
  keys <- vapply(ds_list, .irw_opened_dataset_key, character(1))
  pos <- match(unname(target), keys)
  if (is.na(pos) || pos == 1L) {
    return(ds_list)
  }
  c(ds_list[pos], ds_list[-pos])
}

#' Re-order the shards still to be searched, after the newest one has missed
#'
#' The first shard in the search order is tried before the map is read, so a
#' table in the newest warehouse -- where new tables are uploaded, and so where
#' a first fetch most often looks -- costs exactly what it costs today and the
#' map is never built. Once that shard has missed, the remaining six are worth
#' resolving rather than walking.
#'
#' @param ds_list The full list, in search order.
#' @param name Table name being looked for.
#' @param source Resolved source name.
#' @return \code{ds_list} with its tail re-ordered; the same length, and with
#'   the already-tried first element still first.
#' @keywords internal
#' @noRd
.irw_shard_first_after_miss <- function(ds_list, name, source = "core") {
  if (length(ds_list) < 3L) {
    return(ds_list)
  }
  c(ds_list[1L], .irw_shard_first(ds_list[-1L], name = name, source = source))
}
