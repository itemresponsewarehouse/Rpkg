#' Simulated Item Difficulty Pool from IRW Datasets
#'
#' A dataset of item difficulty estimates and associated standard errors compiled from multiple IRW datasets.
#' Used internally by `irw_simu_diff()` to generate realistic item parameters for simulation.
#'
#' @format A data frame with N rows and 4 variables:
#' \describe{
#'   \item{dataset}{Dataset identifier (character). Indicates the source dataset.}
#'   \item{item}{Item identifier (character). A label for each item.}
#'   \item{difficulty}{Estimated item difficulty (numeric).}
#'   \item{SE}{Standard error of the difficulty estimate (numeric).}
#' }
#'
#' @details
#' This dataset provides a pool of empirically estimated item difficulties and uncertainty
#' from real or simulated IRW datasets. It is used as a default source by functions such as `irw_simu_diff()`
#' to construct realistic simulation scenarios.
#'
#' @source Compiled from real or simulated IRW data by the package authors.
"diff_long"
