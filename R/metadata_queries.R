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


#' Get Database Metadata
#'
#' Retrieves and prints comprehensive metadata about the IRW database, including version, number of tables,
#' data size, access information, and relevant URLs.
#'
#' @return A list containing the database metadata for programmatic use.
#' @export
get_database_metadata <- function() {
  # Initialize the datasource if it hasn't been initialized
  ds <- initialize_datasource()

  # Retrieve metadata properties
  version <- ds$properties$version$tag
  table_count <- ds$properties$tableCount
  created_at <- ds$properties$createdAt / 1000  # Convert from milliseconds to seconds
  updated_at <- ds$properties$updatedAt / 1000  # Convert from milliseconds to seconds
  total_size <- ds$properties$totalNumBytes / (1024^3)  # Convert bytes to GB
  active_size <- ds$properties$totalActiveTabularBytes / (1024^3)  # Convert bytes to GB
  doi <- ds$properties$doi
  doi_url <- paste0("https://doi.org/", doi)
  dataset_url <- ds$properties$url
  documentation_link <- ds$properties$links[[1]]$url
  methodology_link <- ds$properties$methodologyMarkdown
  usage_link <- ds$properties$usageMarkdown

  # Format dates
  formatted_created_at <- format(as.POSIXct(created_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  formatted_updated_at <- format(as.POSIXct(updated_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")

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



#' Get Metadata for a Specific Table
#'
#' Retrieves and prints detailed metadata about a specified table within the IRW database,
#' including the number of rows, size in bytes, creation and update timestamps, access level, and URLs.
#'
#' @param table_name A character string specifying the name of the table.
#' @return A list containing the table metadata for programmatic use.
#' @export
get_table_metadata <- function(table_name) {
  # Fetch the table object
  table <- fetch_table(table_name)

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

  # Print metadata information
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

