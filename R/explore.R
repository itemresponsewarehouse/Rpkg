#' Retrieve and Print Metadata for a Specific Table
#'
#' Fetches and displays detailed metadata for a specified table in the IRW database.
#' The metadata includes information such as the number of rows, size in bytes, timestamps,
#' access level, and related URLs.
#'
#' @param table_name A character string specifying the name of the table to retrieve metadata for.
#' @examples
#' \dontrun{
#'   irw_table_metadata("abortion")
#' }
#' @export
irw_table_metadata <- function(table_name) {
  # Fetch the table object
  table <- .fetch_redivis_table(table_name)
  
  # Retrieve metadata properties
  name <- table$properties$name
  created_at <- table$properties$createdAt / 1000  # Convert from milliseconds to seconds
  updated_at <- table$properties$updatedAt / 1000  # Convert from milliseconds to seconds
  num_rows <- table$properties$numRows
  data_size <- table$properties$numBytes / 1024  # Convert bytes to KB
  variable_count <- table$properties$variableCount
  table_url <- table$properties$url
  is_sample <- table$properties$isSample
  container_url <- table$properties$container$url
  doi <- table$properties$container$doi
  doi_url <- if (!is.null(doi)) paste0("https://doi.org/", doi) else "N/A"
  
  # Format dates
  formatted_created_at <- format(as.POSIXct(created_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  formatted_updated_at <- format(as.POSIXct(updated_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  
  # Print metadata information
  cat("\nTable Metadata for:", table_name, "\n")
  cat(strrep("-", 50), "\n")
  cat(sprintf("%-25s %s\n", "Name:", name))
  cat(sprintf("%-25s %s\n", "Created At:", formatted_created_at))
  cat(sprintf("%-25s %s\n", "Last Updated At:", formatted_updated_at))
  cat(sprintf("%-25s %d\n", "Number of Rows:", num_rows))
  cat(sprintf("%-25s %.2f KB\n", "Data Size (KB):", data_size))
  cat(sprintf("%-25s %d\n", "Variable Count:", variable_count))
  cat(sprintf("%-25s %s\n", "Is Sample:", ifelse(is_sample, "Yes", "No")))
  cat(sprintf("%-25s %s\n", "DOI:", doi_url))
  cat(sprintf("%-25s %s\n", "Table URL:", table_url))
  cat(sprintf("%-25s %s\n", "Container URL:", container_url))
  cat(strrep("-", 50), "\n\n")
  
  # No return value—only prints output
  invisible(NULL)
}



#' List Available Datasets
#'
#' Retrieves a summary of available datasets in the IRW database, including their name,
#' number of rows, and variable count, sorted alphabetically by dataset name.
#'
#' @return A data frame with the following columns:
#'   \item{name}{The name of the dataset, sorted alphabetically.}
#'   \item{numRows}{The number of rows in the dataset.}
#'   \item{variableCount}{The number of variables in the dataset.}
#' @examples
#' \dontrun{
#'   datasets <- irw_list_datasets()
#'   print(datasets)
#' }
#' @export
irw_list_datasets <- function() {
  # Initialize the datasource if not already set
  ds <- .initialize_datasource()
  
  # Retrieve the list of datasets from the datasource
  datasets <- ds$list_tables()  # Assuming 'list_tables()' returns datasets as well
  
  # Extract metadata to create a data frame
  datasets_info <- data.frame(
    name = sapply(datasets, function(dataset) dataset$name),
    numRows = sapply(datasets, function(dataset) dataset$properties$numRows),
    variableCount = sapply(datasets, function(dataset) dataset$properties$variableCount),
    stringsAsFactors = FALSE
  )
  
  # Sort datasets_info by the name column in alphabetical order
  datasets_info <- datasets_info[order(datasets_info$name), ]
  
  return(datasets_info)
}


#' Retrieve and Print IRW Database Metadata
#'
#' Fetches and displays comprehensive metadata for the IRW database, including the database version,
#' number of tables, total data size, and relevant URLs.
#'
#' @examples
#' \dontrun{
#'   irw_db_metadata()
#' }
#' @export
irw_db_metadata <- function() {
  # Initialize the datasource if it hasn't been initialized
  ds <- .initialize_datasource()
  
  # Retrieve metadata properties
  version <- ds$properties$version$tag
  table_count <- ds$properties$tableCount
  created_at <- ds$properties$createdAt / 1000  # Convert from milliseconds to seconds
  updated_at <- ds$properties$updatedAt / 1000  # Convert from milliseconds to seconds
  total_size <- ds$properties$totalNumBytes / (1024 ^ 3)  # Convert bytes to GB
  active_size <- ds$properties$totalActiveTabularBytes / (1024 ^ 3)  # Convert bytes to GB
  doi <- ds$properties$doi
  doi_url <- if (!is.null(doi)) paste0("https://doi.org/", doi) else "N/A"
  dataset_url <- ds$properties$url
  documentation_link <- if (!is.null(ds$properties$links[[1]]$url)) ds$properties$links[[1]]$url else "N/A"
  methodology_link <- if (!is.null(ds$properties$methodologyMarkdown)) ds$properties$methodologyMarkdown else "N/A"
  usage_link <- if (!is.null(ds$properties$usageMarkdown)) ds$properties$usageMarkdown else "N/A"
  
  # Format dates
  formatted_created_at <- format(as.POSIXct(created_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  formatted_updated_at <- format(as.POSIXct(updated_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  
  # Print metadata information
  cat("\nIRW Database Metadata\n")
  cat(strrep("-", 50), "\n")
  cat(sprintf("%-25s %s\n", "Version:", version))
  cat(sprintf("%-25s %d\n", "Table Count:", table_count))
  cat(sprintf("%-25s %s\n", "Created At:", formatted_created_at))
  cat(sprintf("%-25s %s\n", "Last Updated At:", formatted_updated_at))
  cat(sprintf("%-25s %.2f GB\n", "Total Data Size:", total_size))
  cat(sprintf("%-25s %.2f GB\n", "Active Data Size:", active_size))
  cat(sprintf("%-25s %s\n", "DOI:", doi_url))
  cat(sprintf("%-25s %s\n", "Dataset URL:", dataset_url))
  cat(sprintf("%-25s %s\n", "Documentation:", documentation_link))
  cat(sprintf("%-25s %s\n", "Methodology:", methodology_link))
  cat(sprintf("%-25s %s\n", "Usage Information:", usage_link))
  cat(strrep("-", 50), "\n")
  
  # No return value—only prints output
  invisible(NULL)
}
