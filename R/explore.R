#' List Available Tables
#'
#' Retrieves a summary of available tables in the IRW database, including their name,
#' number of rows, and variable count, sorted alphabetically by table name.
#'
#' @return A data frame with the following columns:
#'   \item{name}{The name of the table, sorted alphabetically.}
#'   \item{numRows}{The number of rows in the table.}
#'   \item{variableCount}{The number of variables in the table.}
#' @examples
#' \dontrun{
#'   tables <- irw_list_tables()
#'   print(tables)
#' }
#' @export
irw_list_tables <- function() {
  # Initialize the datasource if not already set
  ds <- .initialize_datasource()
  
  # Retrieve the list of tables from the datasource
  tables <- ds$list_tables()  
  
  # Extract metadata to create a data frame
  tables_info <- data.frame(
    name = sapply(tables, function(table) table$name),
    numRows = sapply(tables, function(table) table$properties$numRows),
    variableCount = sapply(tables, function(table) table$properties$variableCount),
    stringsAsFactors = FALSE
  )
  
  # Sort tables_info by the name column in alphabetical order
  tables_info <- tables_info[order(tables_info$name), ]
  
  return(tables_info)
}


#' Retrieve and Print IRW Database or Table Information
#'
#' Fetches and displays information for the IRW database or a specified table.
#' If no table name is provided, it returns general database information.
#' If a table name is provided, it returns detailed information about that table.
#'
#' @param table_name Optional. A character string specifying the name of the table to retrieve information for.
#' @examples
#' \dontrun{
#'   irw_info()  # Prints database information
#'   irw_info("abortion")  # Prints table-specific information
#' }
#' @export
irw_info <- function(table_name = NULL) {
  if (is.null(table_name)) {
    # Fetch database information
    ds <- .initialize_datasource()
    version <- ds$properties$version$tag
    table_count <- ds$properties$tableCount
    created_at <- ds$properties$createdAt / 1000  # Convert from milliseconds to seconds
    updated_at <- ds$properties$updatedAt / 1000  # Convert from milliseconds to seconds
    total_size <- ds$properties$totalNumBytes / (1024 ^ 3)  # Convert bytes to GB
    dataset_url <- ds$properties$url
    documentation_link <- if (!is.null(ds$properties$links[[1]]$url)) ds$properties$links[[1]]$url else "N/A"
    methodology_link <- "Tables have been harmonized as per details given here: (https://datapages.github.io/irw/standard.html)."
    usage_link <- "Please find information about data licenses and citation info here: (https://datapages.github.io/irw/docs.html)."
    
    formatted_created_at <- format(as.POSIXct(created_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
    formatted_updated_at <- format(as.POSIXct(updated_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
    
    message(strrep("-", 50))
    message("IRW Database Information")
    message(strrep("-", 50))
    message(sprintf("%-25s %s", "Version:", version))
    message(sprintf("%-25s %d", "Table Count:", table_count))
    message(sprintf("%-25s %.2f GB", "Total Data Size:", total_size))
    message(sprintf("%-25s %s", "Created At:", formatted_created_at))
    message(sprintf("%-25s %s", "Last Updated At:", formatted_updated_at))
    message(strrep("-", 50))
    message(sprintf("%-25s %s", "Redivis URL:", dataset_url))
    message(sprintf("%-25s %s", "Data Website:", documentation_link))
    message(sprintf("%-25s %s", "Methodology:", methodology_link))
    message(sprintf("%-25s %s", "Usage Information:", usage_link))
    message(strrep("-", 50))
  } else {
    # Fetch table-specific information
    table <- .fetch_redivis_table(table_name)
    bib <- .fetch_biblio_table()
    thisbib <- bib[bib$table == table_name, ]
    
    name <- table$properties$name
    num_rows <- table$properties$numRows
    data_size <- table$properties$numBytes / 1024  # Convert bytes to KB
    variable_count <- table$properties$variableCount
    table_url <- table$properties$url
    doi <- thisbib$DOI__for_paper_
    url_data <- thisbib$URL__for_data_
    licence <- thisbib$Derived_License
    description <- thisbib$Description
    reference <- thisbib$Reference_x  
    
    message(strrep("-", 50))
    message("Table Information for: ", table_name)
    message(strrep("-", 50))
    message(sprintf("%-25s %s", "Description:", description))
    message(sprintf("%-25s %d", "Number of Rows:", num_rows))
    message(sprintf("%-25s %d", "Variable Count:", variable_count))
    message(sprintf("%-25s %.2f KB", "Data Size (KB):", data_size))
    message(strrep("-", 50))
    message(sprintf("%-25s %s", "Redivis URL:", table_url))
    message(sprintf("%-25s %s", "Data URL:", url_data))
    message(sprintf("%-25s %s", "DOI:", doi))
    message(sprintf("%-25s %s", "License:", licence))
    message(strrep("-", 50))
    message(sprintf("%s%s", "Reference:\n", reference))
    message(strrep("-", 50))
  }
  
  invisible(NULL)
}

