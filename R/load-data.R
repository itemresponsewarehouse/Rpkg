.onLoad <- function(libname, pkgname) {
  data("metadata", package=pkgname, envir=parent.env(environment()))
}

#' Retrieve Metadata
#'
#' Retrieves the `metadata` dataset dynamically loaded into the package namespace.
#'
#' @details
#' The `metadata` dataset contains precomputed information about tables in the database, 
#' such as `id_count`, `item_count`, `resp_count`, and others. This function ensures 
#' that the dataset is accessed in a controlled manner.
#'
#' @examples
#' metadata <- get_metadata()
#' head(metadata)
#'
#' @export
get_metadata <- function() {
  get("metadata", envir = asNamespace("irwpkg"))
}