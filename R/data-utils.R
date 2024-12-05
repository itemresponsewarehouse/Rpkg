# Provide utility functions for data handling, including downloading


#' Download a Dataset
#'
#' This function downloads a specified dataset from the IRW datasource.
#'
#' @param table_name A character string specifying the name of the Redivis table
#'        from which data should be downloaded. This should be the exact name
#'        of the table as listed in the IRW Redivis database.
#' @param path A string specifying the file path where the data should be saved.
#' If `NULL`, the dataset will be saved in the current working directory with the table's name as the file name.
#' @param overwrite A logical indicating whether to overwrite an existing file at the specified path.
#' Default is `FALSE`.
#' @return A message confirming the file download location.
#' @export
download_data <- function(table_name, path = NULL, overwrite = FALSE) {
  table <- fetch_table(table_name)
  # Check if the table object has the download method
  if (!is.function(table$download)) {
    stop("The provided object does not support downloading. Ensure it is a valid Redivis data table.")
  }

  # Attempt to download the file
  table$download(path = path, overwrite = overwrite)

  # Notify user of the download location
  if (is.null(path)) {
    path <- paste0(getwd(), "/", table$name)
  }
  message("Dataset downloaded to: ", path)
}
