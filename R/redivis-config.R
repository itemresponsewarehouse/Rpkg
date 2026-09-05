#' IRW Redivis datasource identifiers
#'
#' Single source of truth for Redivis user/dataset IDs used by the package.
#' To add another main IRW warehouse, append one list element to
#' \code{.irw_datasource_specs$core} (see \code{inst/developer/warehouses.md}).
#'
#' @keywords internal
#' @noRd
.irw_datasource_specs <- list(
  core = list(
    list(user = "datapages", dataset = "item_response_warehouse:as2e"),
    list(user = "datapages", dataset = "item_response_warehouse_2:epbx"),
    list(user = "datapages", dataset = "item_response_warehouse_3:5xaj"),
    list(user = "datapages", dataset = "item_response_warehouse_4:980f"),
    list(user = "datapages", dataset = "item_response_warehouse_5:3ykx"),
    list(user = "datapages", dataset = "item_response_warehouse_6:fpe6")
  ),
  sim = list(
    list(user = "datapages", dataset = "irw_simsyn:0btg")
  ),
  comp = list(
    list(user = "datapages", dataset = "irw_competitions:cmd7")
  ),
  nom = list(
    list(user = "datapages", dataset = "irw_nominal:614n")
  )
)

#' Datasources that have a tags table
#'
#' Tags are hand-annotated per source from a Google Sheet (see
#' \code{src/metadata/03_tags.R}). \code{comp} and \code{sim} deliberately have
#' none -- see \code{inst/developer/tags.md} before adding one.
#'
#' @keywords internal
#' @noRd
.irw_tag_sources <- c("core", "nom")

##Sources that have collections (issue #1633). Core only for now. Same reason
##.irw_tag_sources exists: filter.R and explore.R read this rather than testing
##source == "core", and a non-collection source must ERROR rather than return an
##empty tibble -- an empty tibble filters every table away and reads as "no
##matches" instead of "wrong question". See inst/developer/collections.md.
.irw_collection_sources <- c("core")

#' IRW auxiliary datasource identifiers
#'
#' Companions to \code{.irw_datasource_specs}, for the datasets that are not
#' table sources: the metadata/biblio/tags backbone and the item text dataset.
#' Kept as bare specs (not wrapped in a list) because, unlike the entries in
#' \code{.irw_datasource_specs}, they are never looked up by a \code{source}
#' argument -- see \code{.irw_sources} in \code{redivis-datasets.R}.
#'
#' @keywords internal
#' @noRd
.irw_meta_spec <- list(user = "datapages", dataset = "irw_meta:bdxt")

#' Item text datasource identifiers
#'
#' A *list* of specs, oldest to newest, exactly like
#' \code{.irw_datasource_specs$core}. Redivis caps a dataset at 1000 tables, so
#' item text shards the way response data does; the shards are searched
#' newest-first and the first match wins. To add one, append a list element here
#' -- see \code{inst/developer/warehouses.md}.
#'
#' @keywords internal
#' @noRd
.irw_itemtext_specs <- list(
  list(user = "datapages", dataset = "irw_text:07b6"),
  list(user = "datapages", dataset = "irw_text_2:ae47")
)

#' Open the IRW metadata dataset
#'
#' Thin wrapper so the many metadata fetchers share one definition of the
#' owner/dataset pair.
#'
#' @keywords internal
#' @noRd
.irw_open_meta_dataset <- function() {
  .irw_open_dataset(.irw_meta_spec)
}

#' Open a Redivis dataset from a spec list
#'
#' Honors any session version pin for the dataset (see \code{irw_set_version()}).
#' Every IRW dataset is opened through here, so pinning one dataset pins it for
#' fetches, table listings, and metadata alike.
#'
#' @param spec A list with \code{user} and \code{dataset} elements.
#' @keywords internal
#' @noRd
.irw_open_dataset <- function(spec) {
  key <- .irw_dataset_key(spec$dataset)
  version <- .irw_pinned_version(key)
  # `irw_use_version()` pins a dataset that did not exist yet to a sentinel.
  # Reading it has to fail: falling back to the current release would mix
  # today's data into a run that is meant to reproduce an old one. For a core
  # warehouse this is caught by `.irw_open_core_datasources()` and the shard is
  # dropped, which is exactly right -- it was not part of that IRW version.
  if (identical(version, .irw_absent_version)) {
    stop(
      key, " had no released version at the IRW version pinned by ",
      "`irw_use_version()`, so it cannot be read in this session. Use ",
      "`irw_reset_version()` to return to the current release.",
      call. = FALSE
    )
  }
  ds <- .irw_redivis_dataset(spec, version)
  ds$get()
  ds
}

#' Construct a Redivis dataset handle, optionally at a fixed version
#'
#' The single point where the Redivis client is asked for a dataset, so that
#' version pinning has exactly one place to take effect.
#'
#' @param spec A list with \code{user} and \code{dataset} elements.
#' @param version Optional version tag, e.g. \code{"v32.0"}.
#' @keywords internal
#' @noRd
.irw_redivis_dataset <- function(spec, version = NULL) {
  .irw_require_redivis()
  user <- redivis::redivis$user(spec$user)
  if (is.null(version)) {
    user$dataset(spec$dataset)
  } else {
    user$dataset(spec$dataset, version = version)
  }
}

#' Fingerprint of a list of dataset specs (for session cache invalidation)
#'
#' @param specs List of specs, each with \code{user} and \code{dataset}.
#' @keywords internal
#' @noRd
.irw_specs_fingerprint <- function(specs) {
  paste(vapply(specs, function(s) paste(s$user, s$dataset, sep = "/"), character(1)), collapse = "|")
}

#' Fingerprint of configured core warehouses (for session cache invalidation)
#'
#' @keywords internal
#' @noRd
.irw_core_warehouse_fingerprint <- function() {
  .irw_specs_fingerprint(.irw_datasource_specs$core)
}

#' Fingerprint of configured item text shards (for session cache invalidation)
#'
#' @keywords internal
#' @noRd
.irw_itemtext_fingerprint <- function() {
  .irw_specs_fingerprint(.irw_itemtext_specs)
}

#' Session cache key for a non-core datasource
#'
#' @param source One of \code{"sim"}, \code{"comp"}, \code{"nom"}.
#' @keywords internal
#' @noRd
.irw_single_datasource_cache_key <- function(source) {
  switch(source,
         sim = "sim_datasource",
         comp = "comp_datasource",
         nom = "nom_datasource",
         stop("Unknown single datasource: ", source, call. = FALSE))
}
