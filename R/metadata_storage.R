#' Generate Metadata Summary
#'
#' This function retrieves and saves metadata for all tables in the IRW datasource.
#' Metadata includes table name, row count, and column names for each table.
#'
#' @return A data frame with metadata for each table.
generate_metadata_summary <- function() {
  ds <- initialize_datasource()
  tables <- ds$list_tables()

  # Initialize metadata summary with fixed column structure
  metadata_summary <- data.frame(
    table_name = character(0),
    numRows = integer(0),
    columns = I(list())
  )

  for (table in tables) {
    table_metadata <- table$properties
    columns <- sapply(table$list_variables(), function(var) var$name)

    # Append the new row to metadata_summary, ensuring consistent structure
    metadata_summary <- rbind(metadata_summary, data.frame(
      table_name = table_metadata$name,
      numRows = table_metadata$numRows,
      columns = I(list(columns))
    ), stringsAsFactors = FALSE)
  }

  save(metadata_summary, file = "data/metadata_summary.RData")
  return(metadata_summary)
}

#' Metadata Summary for IRW Tables
#'
#' A dataset containing metadata for each table in the IRW datasource.
#' This dataset provides an overview of table characteristics, including the table name,
#' number of rows, and column names, enabling efficient access to structural information.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{table_name}{Name of the table (character)}
#'   \item{numRows}{Number of rows in the table (integer)}
#'   \item{columns}{List of column names within the table (list of character vectors)}
#' }
#' @usage data(metadata_summary)
#' @keywords datasets
#' @examples
#' \dontrun{
#'   # Load and view metadata summary
#'   data(metadata_summary)
#'   head(metadata_summary)
#' }
"metadata_summary"
