#' Fetch Data from the IRW Database
#'
#' Retrieves one or more datasets from the Item Response Warehouse (IRW) database by their names.
#'
#' @param name A character vector specifying one or more dataset names to fetch.
#' @return If a single dataset is fetched, returns a data frame; if multiple datasets are fetched, returns a named list of data frames.
#' @examples
#' \dontrun{
#'   # Fetch a single dataset
#'   df <- fetch_data("dataset_name")
#'   print(df)
#'
#'   # Fetch multiple datasets
#'   datasets <- fetch_data(c("dataset1", "dataset2"))
#'   print(names(datasets))  # Displays "dataset1" and "dataset2"
#' }
#' @export
fetch_data <- function(name) {
  # Helper function to fetch and convert a single dataset
  fetch_single_data <- function(dataset_name) {
    # Attempt to fetch the table
    table <- tryCatch(
      fetch_table(dataset_name),
      error = function(e) {
        stop(paste("Unable to fetch the dataset", shQuote(dataset_name),
                   "from the IRW database. Please check the dataset name."))
      }
    )

    # Convert the table to a data frame
    df <- tryCatch(
      table$to_data_frame(),
      error = function(e) {
        stop(paste("Failed to convert the dataset", shQuote(dataset_name),
                   "to a data frame. Please ensure the dataset is in a compatible format."))
      }
    )

    return(df)
  }

  # Check if fetching a single or multiple datasets
  if (length(name) == 1) {
    # Return a single data frame
    return(fetch_single_data(name))
  } else {
    # Return a named list of data frames if multiple datasets
    dataset_list <- lapply(name, fetch_single_data)
    names(dataset_list) <- name
    return(dataset_list)
  }
}



