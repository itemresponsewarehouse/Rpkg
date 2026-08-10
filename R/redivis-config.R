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
    list(user = "datapages", dataset = "item_response_warehouse_4:980f")
  ),
  sim = list(
    list(user = "bdomingu", dataset = "irw_simsyn:0btg")
  ),
  comp = list(
    list(user = "bdomingu", dataset = "irw_competitions:cmd7")
  ),
  nom = list(
    list(user = "bdomingu", dataset = "irw_nominal:614n")
  )
)

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
