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