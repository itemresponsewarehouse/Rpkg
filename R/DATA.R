#' Metadata for Available Tables
#'
#' This dataset contains metadata about tables from the database, 
#' automatically loaded into the package namespace when the package is attached.
#'
#' @details
#' The `metadata` dataset is loaded dynamically when the package is attached, 
#' and can be accessed internally using functions like `get_metadata()`. 
#' This ensures that it remains scoped to the package namespace.
#'
#' @examples
#' # Access metadata programmatically
#' metadata <- irwpkg::get_metadata()
#' head(metadata)
"metadata"

#' Metadata Failures
#'
#' This dataset tracks tables from the Redivis database that failed to process 
#' during metadata computation. It logs the issues encountered and whether 
#' partial processing was possible.
#'
#' @details This dataset is created during metadata computation and stored in 
#' the `data/` folder with the suffix `_failed.csv`. It serves as a log for 
#' debugging and reprocessing tables that could not be fully handled. 
#'
#' @keywords datasets
#' @name metadata_failed
#' @md
NULL