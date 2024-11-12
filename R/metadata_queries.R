# Functions to manage, retrieve, and manipulate metadata,
# such as details about tables, columns, variables, and the dataset’s properties in Redivis.

#' List Available Datasets
#'
#' Retrieves a summary of available datasets in the datasource, including the dataset name,
#' number of rows, and variable count for each dataset.
#'
#' @return A data frame where each row corresponds to a dataset, with columns for the
#'         dataset name (`name`), number of rows (`numRows`), and variable count (`variableCount`).
#' @examples
#' \dontrun{
#'   datasets_summary <- list_available_datasets()
#'   print(datasets_summary)
#' }
#' @export
list_available_datasets <- function() {
  # Initialize the datasource if not already set
  ds <- initialize_datasource()

  # Retrieve the list of datasets from the datasource
  datasets <- ds$list_tables()  # Assuming 'list_tables()' returns datasets as well

  # Extract metadata to create a data frame
  datasets_info <- data.frame(
    name = sapply(datasets, function(dataset) dataset$name),
    numRows = sapply(datasets, function(dataset) dataset$properties$numRows),
    variableCount = sapply(datasets, function(dataset) dataset$properties$variableCount),
    stringsAsFactors = FALSE
  )

  return(datasets_info)
}



