#' Shard discovery: finding warehouses created after this package was released
#'
#' The shard lists in \code{R/redivis-config.R} are literals in package source,
#' so corpus growth and package releases are coupled. When a new shard is
#' seeded, every installed copy of irw is blind to it -- and blind *silently*:
#' a shard that is not in the spec list produces no loop iteration in
#' \code{.irw_open_datasources()}, so neither of that function's two warning
#' paths can fire. The tables simply drop out of \code{irw_list_tables()},
#' \code{irw_metadata()} and \code{irw_filter()}, and the counts come back short
#' with nothing to say so. That is what happened with the seventh core
#' warehouse: it released on 2026-10-15 and no released client could see its
#' 761 tables (ben-domingue/irw#3091).
#'
#' So the shard lists are treated as a floor rather than the whole truth. Once
#' per session the Redivis organization catalog is read and any shard that
#' continues a family already in the config is appended.
#'
#' Three properties make this safe to do by default:
#'
#' \itemize{
#'   \item It only ever *extends* a family already in the config, by matching
#'     the family's base name plus a numeric suffix. A new data family is
#'     deliberately not adopted: that needs a \code{source} value, filters and
#'     metadata wiring, so it must keep failing loudly as it does today.
#'   \item It only appends, never removes or reorders. The configured specs stay
#'     exactly as they are, so a session with no network, or one that opts out,
#'     behaves byte-for-byte as before.
#'   \item A shard is adopted only if the catalog shows it has a published
#'     version and is publicly readable, which are the two conditions for
#'     opening it without a warning.
#' }
#'
#' Opt out with \code{options(irw.shard_discovery = FALSE)} or
#' \code{IRW_SHARD_DISCOVERY=0}.
#'
#' @name shard-discovery
#' @keywords internal
NULL

#' The organization whose datasets hold IRW
#'
#' @keywords internal
#' @noRd
.irw_catalog_organization <- "datapages"

#' Catalog endpoint for the IRW organization's datasets
#'
#' A documented REST endpoint
#' (\url{https://docs.redivis.com/api/rest-api/datasets/list}) that answers
#' without credentials for public datasets, which is what lets discovery work
#' in the no-login sessions that are now the default (Rpkg#202). 1000 is the
#' maximum the endpoint accepts and is far above the ~32 datasets the
#' organization holds, so one request is the whole catalog and there is no
#' paging to get wrong.
#'
#' @keywords internal
#' @noRd
.irw_catalog_url <- paste0(
  "https://redivis.com/api/v1/organizations/",
  .irw_catalog_organization,
  "/datasets?maxResults=1000"
)

#' Whether shard discovery is switched on
#'
#' @keywords internal
#' @noRd
.irw_shard_discovery_enabled <- function() {
  opt <- getOption("irw.shard_discovery")
  if (!is.null(opt)) {
    return(isTRUE(opt))
  }
  env <- Sys.getenv("IRW_SHARD_DISCOVERY", unset = "")
  if (nzchar(env)) {
    return(!tolower(env) %in% c("0", "false", "no", "off"))
  }
  TRUE
}

#' Parse the dataset catalog response
#'
#' Kept apart from the request so the parsing rules can be tested against a
#' fixture without touching the network.
#'
#' The reference *name* is the first field of \code{scopedReference}, not
#' \code{name}: \code{name} is the display name, which does not resolve
#' (\code{irw_rank} shows as "IRW_rank", and the seventh core warehouse as
#' "Item Response Warehouse 7"). A \code{scopedReference} carries its version as
#' a third field only when a version has been published, so requiring three
#' fields is also the released-or-not test.
#'
#' @param text Response body, JSON.
#' @return Data frame with \code{reference}, \code{reference_id},
#'   \code{version}, \code{public}, \code{table_count}; zero rows if nothing
#'   parsed.
#' @keywords internal
#' @noRd
.irw_parse_dataset_catalog <- function(text) {
  empty <- data.frame(
    reference = character(0), reference_id = character(0),
    version = character(0), public = logical(0), table_count = integer(0),
    stringsAsFactors = FALSE
  )
  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  results <- parsed$results
  if (!is.list(results) || length(results) == 0L) {
    return(empty)
  }

  rows <- lapply(results, function(r) {
    ref <- r$scopedReference
    if (!is.character(ref) || length(ref) != 1L) {
      return(NULL)
    }
    parts <- strsplit(ref, ":", fixed = TRUE)[[1]]
    # name:refid:version. Two fields means no published version yet.
    if (length(parts) != 3L) {
      return(NULL)
    }
    data.frame(
      reference = parts[[1]],
      reference_id = parts[[2]],
      version = parts[[3]],
      public = identical(r$publicAccessLevel, "data"),
      table_count = if (is.numeric(r$tableCount)) as.integer(r$tableCount) else NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(empty)
  }
  do.call(rbind, rows)
}

#' The IRW organization's dataset catalog, read once per session
#'
#' Never raises and never retries within a session: discovery is an improvement
#' on the configured list, not a dependency of it, so a catalog that cannot be
#' read has to leave the package exactly as it was. The failure is remembered
#' so a session with no network pays for one timeout rather than one per call.
#'
#' @return Catalog data frame, or \code{NULL} when unavailable.
#' @keywords internal
#' @noRd
.irw_dataset_catalog <- function() {
  if (exists("dataset_catalog", envir = .irw_env)) {
    return(.irw_env$dataset_catalog)
  }
  catalog <- tryCatch(
    {
      response <- httr::GET(.irw_catalog_url, httr::timeout(15))
      if (httr::status_code(response) != 200) {
        NULL
      } else {
        parsed <- .irw_parse_dataset_catalog(
          httr::content(response, as = "text", encoding = "UTF-8")
        )
        if (nrow(parsed) == 0L) NULL else parsed
      }
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )
  .irw_env$dataset_catalog <- catalog
  catalog
}

#' Base name of a family of shards
#'
#' The oldest spec in a list names the family: the core warehouses start at
#' \code{item_response_warehouse} and continue \code{_2}, \code{_3}, ...; item
#' text starts at \code{irw_text}; conjoint at \code{irw_conjoint}. The
#' single-dataset sources follow the same shape, which is why discovery is
#' applied to them too -- \code{irw_simsyn} is nowhere near the 1000-table cap
#' today, but "which families did we remember to wire up" is how the seventh
#' core warehouse came to be missed, and the uniform rule has no such gap.
#'
#' @param specs Spec list, oldest first.
#' @keywords internal
#' @noRd
.irw_family_base <- function(specs) {
  if (length(specs) == 0L) {
    return(NA_character_)
  }
  first <- .irw_dataset_key(specs[[1]]$dataset)
  sub("_[0-9]+$", "", first)
}

#' Shards of a family that are live but not in the configured list
#'
#' @param specs Configured spec list for one family, oldest first.
#' @param catalog Catalog from \code{.irw_dataset_catalog()}.
#' @return A list of specs to append, ordered oldest-first by suffix.
#' @keywords internal
#' @noRd
.irw_extra_shard_specs <- function(specs, catalog) {
  if (is.null(catalog) || nrow(catalog) == 0L || length(specs) == 0L) {
    return(list())
  }
  base <- .irw_family_base(specs)
  if (is.na(base) || !nzchar(base)) {
    return(list())
  }

  # `base` or `base_<digits>` and nothing else, so a different family that
  # merely starts with the same characters cannot match. Anchored, and the
  # base is escaped because it is data, not a pattern.
  pattern <- paste0("^", .irw_escape_regex(base), "(_[0-9]+)?$")
  candidates <- catalog[grepl(pattern, catalog$reference) & catalog$public, , drop = FALSE]
  if (nrow(candidates) == 0L) {
    return(list())
  }

  configured <- vapply(specs, function(s) .irw_dataset_key(s$dataset), character(1))
  candidates <- candidates[!candidates$reference %in% configured, , drop = FALSE]
  if (nrow(candidates) == 0L) {
    return(list())
  }

  # Oldest first, to match the order every spec list is declared in: the
  # unsuffixed base is shard 1, then ascending numeric suffix. Lexical order
  # would put _10 before _2.
  suffix <- sub(paste0("^", .irw_escape_regex(base)), "", candidates$reference)
  index <- ifelse(nzchar(suffix), suppressWarnings(as.integer(sub("^_", "", suffix))), 1L)
  candidates <- candidates[order(index, na.last = TRUE), , drop = FALSE]

  lapply(seq_len(nrow(candidates)), function(i) {
    list(
      user = .irw_catalog_organization,
      dataset = paste0(candidates$reference[[i]], ":", candidates$reference_id[[i]])
    )
  })
}

#' Escape a literal string for use inside a regular expression
#'
#' @keywords internal
#' @noRd
.irw_escape_regex <- function(x) {
  gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", x, perl = TRUE)
}

#' Tell the user once that their irw is behind the warehouse
#'
#' A discovered shard means the installed package predates it. That is worth
#' saying: the tables are reachable now, but the package's own filter
#' vocabularies and documentation do not know about whatever else shipped
#' alongside. Said once per shard per session.
#'
#' @keywords internal
#' @noRd
.irw_announce_discovered <- function(extra, noun) {
  if (length(extra) == 0L) {
    return(invisible(NULL))
  }
  keys <- vapply(extra, function(s) .irw_dataset_key(s$dataset), character(1))
  if (!exists("discovery_announced", envir = .irw_env)) {
    .irw_env$discovery_announced <- character(0)
  }
  new <- setdiff(keys, .irw_env$discovery_announced)
  if (length(new) == 0L) {
    return(invisible(NULL))
  }
  .irw_env$discovery_announced <- c(.irw_env$discovery_announced, new)
  message(
    "Found ", length(new), " IRW ", noun, "(s) newer than this version of irw: ",
    paste(new, collapse = ", "), ".\n",
    "They are included here. Upgrading irw also brings the documentation and ",
    "filters that shipped with them."
  )
  invisible(NULL)
}

#' The spec list to actually open for a family
#'
#' Configured specs, plus any live shard that continues the family. This is the
#' single seam discovery acts through: every caller that opens a family, and
#' every fingerprint that decides whether a cache is stale, goes through here,
#' so the configured list and the effective list cannot drift apart.
#'
#' Deliberately not cached. The catalog behind it is (one request per session),
#' but this function is a pure transformation of the configured specs, and
#' caching it would make a test that swaps \code{.irw_datasource_specs} see the
#' wrong list.
#'
#' @param source One of the names of \code{.irw_datasource_specs}, or
#'   \code{"text"} for the item text shards.
#' @return Spec list, oldest first.
#' @keywords internal
#' @noRd
.irw_effective_specs <- function(source) {
  specs <- if (identical(source, "text")) {
    .irw_itemtext_specs
  } else {
    .irw_datasource_specs[[source]]
  }
  if (is.null(specs) || !.irw_shard_discovery_enabled()) {
    return(specs)
  }
  extra <- .irw_extra_shard_specs(specs, .irw_dataset_catalog())
  if (length(extra) == 0L) {
    return(specs)
  }
  .irw_announce_discovered(extra, if (identical(source, "text")) "item text shard" else .irw_source_noun(source))
  c(specs, extra)
}
