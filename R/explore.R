#' Retrieve Metadata for a Specific Table
#'
#' Fetches and displays detailed metadata for a specified table in the IRW database.
#' The metadata includes information such as the number of rows, size in bytes, timestamps,
#' access level, and related URLs.
#'
#' @param table_name A character string specifying the name of the table to retrieve metadata for.
#' @param verbose Logical; whether to print the metadata to the console. Defaults to `TRUE`.
#' @return A list containing the table metadata for programmatic use, including:
#'   \item{name}{The name of the table.}
#'   \item{created_at}{The creation timestamp of the table (formatted as a string).}
#'   \item{updated_at}{The last update timestamp of the table (formatted as a string).}
#'   \item{num_rows}{The number of rows in the table.}
#'   \item{data_size_kb}{The size of the table in kilobytes.}
#'   \item{variable_count}{The number of variables in the table.}
#'   \item{is_sample}{Logical; indicates whether the table is a sample dataset.}
#'   \item{doi}{The DOI of the table, if available.}
#'   \item{table_url}{The URL to the table.}
#'   \item{container_url}{The URL to the container of the table.}
#' @examples
#' \dontrun{
#'   metadata <- irw_table_metadata("abortion")
#'   print(metadata)
#' }
#' @export
irw_table_metadata <- function(table_name, verbose = TRUE) {
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
  doi_url <- paste0("https://doi.org/", doi)
  
  # Format dates
  formatted_created_at <- format(as.POSIXct(created_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  formatted_updated_at <- format(as.POSIXct(updated_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  
  # Optionally print metadata information
  if (verbose) {
    cat("Table Metadata for:", table_name, "\n")
    cat("--------------------------------------------------\n")
    cat("Name:                     ", name, "\n")
    cat("Created At:               ", formatted_created_at, "\n")
    cat("Last Updated At:          ", formatted_updated_at, "\n")
    cat("Number of Rows:           ", num_rows, "\n")
    cat("Data Size (KB):           ", round(data_size, 2), "KB\n")
    cat("Variable Count:           ", variable_count, "\n")
    cat("Is Sample:                ", ifelse(is_sample, "Yes", "No"), "\n")
    cat("DOI:                      ", doi_url, "\n")
    cat("Table URL:                ", table_url, "\n")
    cat("Container URL:            ", container_url, "\n")
    cat("--------------------------------------------------\n")
  }
  
  # Compile metadata into a list for programmatic use
  metadata <- list(
    name = name,
    created_at = formatted_created_at,
    updated_at = formatted_updated_at,
    num_rows = num_rows,
    data_size_kb = round(data_size, 2),
    variable_count = variable_count,
    is_sample = is_sample,
    doi = doi,
    table_url = table_url,
    container_url = container_url
  )
  
  # Return the metadata list
  return(metadata)
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


#' Retrieve Database Metadata
#'
#' Fetches and displays comprehensive metadata for the IRW database, including the database version,
#' number of tables, total data size, and relevant URLs.
#'
#' @return A list containing the database metadata, including:
#'   \item{version}{The version of the database.}
#'   \item{table_count}{The total number of tables in the database.}
#'   \item{created_at}{The creation timestamp of the database (formatted as a string).}
#'   \item{updated_at}{The last update timestamp of the database (formatted as a string).}
#'   \item{total_size_gb}{The total size of the database in gigabytes.}
#'   \item{active_data_size_gb}{The size of active tabular data in gigabytes.}
#'   \item{doi}{The DOI of the database.}
#'   \item{dataset_url}{The URL to the database.}
#'   \item{documentation}{The link to the database documentation.}
#'   \item{methodology}{The link to the database methodology.}
#'   \item{usage_info}{The link to usage information.}
#' @examples
#' \dontrun{
#'   db_metadata <- irw_db_metadata()
#'   print(db_metadata)
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
  doi_url <- paste0("https://doi.org/", doi)
  dataset_url <- ds$properties$url
  documentation_link <- ds$properties$links[[1]]$url
  methodology_link <- ds$properties$methodologyMarkdown
  usage_link <- ds$properties$usageMarkdown
  
  # Format dates
  formatted_created_at <- format(as.POSIXct(created_at, origin = "1970-01-01", tz = "UTC"),
                                 "%Y-%m-%d %H:%M:%S")
  formatted_updated_at <- format(as.POSIXct(updated_at, origin = "1970-01-01", tz = "UTC"),
                                 "%Y-%m-%d %H:%M:%S")
  
  # Print metadata information
  cat("IRW Database Metadata:\n")
  cat("--------------------------------------------------\n")
  cat("Version:                  ", version, "\n")
  cat("Table Count:              ", table_count, "\n")
  cat("Created At:               ", formatted_created_at, "\n")
  cat("Last Updated At:          ", formatted_updated_at, "\n")
  cat("Total Data Size (GB):     ", round(total_size, 2), "GB\n")
  cat("Active Data Size (GB):    ", round(active_size, 2), "GB\n")
  cat("DOI:                      ", doi_url, "\n")
  cat("Dataset URL:              ", dataset_url, "\n")
  cat("Documentation:            ", documentation_link, "\n")
  cat("Methodology:              ", methodology_link)
  cat("Usage Information:        ", usage_link, "\n")
  cat("--------------------------------------------------\n")
  
  # Compile metadata into a list for programmatic use
  metadata <- list(
    version = version,
    table_count = table_count,
    created_at = formatted_created_at,
    updated_at = formatted_updated_at,
    total_size_gb = round(total_size, 2),
    active_data_size_gb = round(active_size, 2),
    doi = doi,
    dataset_url = dataset_url,
    documentation = documentation_link,
    methodology = methodology_link,
    usage_info = usage_link
  )
  
  # Return the metadata list
  return(metadata)
}
