#' Item Difficulties Extracted from IRW datasets (updated Feb, 2025)
#'
#' A dataset containing item difficulty estimates and standard errors
#' from multiple datasets. This can be used as an example input for the `simu_diff()` function.
#'
#' @format A data frame with N rows and 3 variables:
#' \describe{
#'   \item{dataset}{Factor or character. The dataset identifier.}
#'   \item{item}{Item label.}
#'   \item{difficulty}{Numeric. Estimated item difficulty.}
#'   \item{SE}{Numeric. Standard error of the item difficulty estimate.}
#' }
#' @source Simulated or real data compiled by the package author.
#' @docType data
#' @keywords datasets
#' @name diff_long
"diff_long"
