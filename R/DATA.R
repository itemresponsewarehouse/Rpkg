#' Metadata Summary
#'
#' This dataset contains metadata about tables from the Redivis database.
#' It provides precomputed information about table properties to be 
#' used by internal package functions for filtering and querying.
#'
#' @details This dataset is generated during package development and stored 
#' in the `data/` folder. It provides essential metadata for the package's 
#' internal operations, enabling faster processing and filtering of tables 
#' based on specific criteria.
#'
#' @keywords datasets
#' @name metadata
#' @md
NULL

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