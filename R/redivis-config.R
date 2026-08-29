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

#' @keywords internal
#' @noRd
.irw_itemtext_spec <- list(user = "datapages", dataset = "irw_text:07b6")

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
#' @param spec A list with \code{user} and \code{dataset} elements.
#' @keywords internal
#' @noRd
.irw_open_dataset <- function(spec) {
  ds <- redivis::redivis$user(spec$user)$dataset(spec$dataset)
  ds$get()
  ds
}

#' Fingerprint of configured core warehouses (for session cache invalidation)
#'
#' @keywords internal
#' @noRd
.irw_core_warehouse_fingerprint <- function() {
  specs <- .irw_datasource_specs$core
  paste(vapply(specs, function(s) paste(s$user, s$dataset, sep = "/"), character(1)), collapse = "|")
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
