#' List Available Tables
#'
#' Retrieves a summary of available tables in the IRW database, including their name,
#' number of rows, and variable count, sorted alphabetically by table name.
#'
#' @param sim Logical. If TRUE, lists tables from the IRW simulation dataset (`irw_simsyn`).
#' @param comp Logical. If TRUE, lists tables from the IRW competition dataset (`irw_comp`).
#'   If both `sim` and `comp` are FALSE (default), lists tables from the main IRW database.
#'   Only one of `sim` or `comp` should be TRUE at a time.
#' @return A data frame with the following columns:
#'   \item{name}{The name of the table, sorted alphabetically.}
#'   \item{numRows}{The number of rows in the table.}
#'   \item{variableCount}{The number of variables in the table.}
#' @examples
#' \dontrun{
#' irw_list_tables()             # Main IRW database
#' irw_list_tables(sim = TRUE)   # Simulation dataset
#' irw_list_tables(comp = TRUE)  # Competition dataset
#' }
#' @export
irw_list_tables <- function(sim = FALSE, comp=FALSE) {
  if (sim && comp) {
    stop("Only one of 'sim' or 'comp' can be TRUE at a time.")
  }
  
  ds_list <- .initialize_datasource(sim = sim, comp=comp)
  
  tables_info_list <- lapply(ds_list, function(ds) {
    tables <- .retry_with_backoff(function() ds$list_tables())
    data.frame(
      name = vapply(tables, function(tbl) tbl$name, character(1)),
      numRows = vapply(tables, function(tbl) tbl$properties$numRows, numeric(1)),
      variableCount = vapply(tables, function(tbl) tbl$properties$variableCount, numeric(1)),
      stringsAsFactors = FALSE
    )
  })
  
  tables_info <- do.call(rbind, tables_info_list)
  tables_info <- tables_info[order(tables_info$name), ]
  rownames(tables_info) <- NULL
  
  return(tables_info)
}


#' Retrieve and Print IRW Database or Table Information
#'
#' Prints information about the IRW datasets or a specific table.
#'
#' If \code{table_name} is \code{NULL}, shows combined totals across all IRW datasets.
#' Use \code{details = TRUE} to also print a per‑dataset breakdown.
#' If a table name is provided, shows details for that table, automatically fetching
#' from the dataset where it resides.
#'
#' @param table_name Optional. Table name to describe; if \code{NULL}, prints database‑level info.
#' @param details Logical. When \code{TRUE} and \code{table_name} is \code{NULL}, also
#'   prints a breakdown by dataset. Defaults to \code{FALSE}.
#'
#' @return Invisibly returns \code{NULL}.
#' @examples
#' \dontrun{
#' irw_info()                 # Combined totals
#' irw_info(details = TRUE)   # Combined + per-dataset breakdown
#' irw_info("frac20")      # Specific table
#' }
#' @export
irw_info <- function(table_name = NULL, details = FALSE) {
  if (is.null(table_name)) {
    # --- Combined totals ---
    ds_list <- .initialize_datasource(sim = FALSE)
    
    total_table_count <- 0
    total_size_gb <- 0
    created_at_all <- c()
    updated_at_all <- c()
    
    for (ds in ds_list) {
      total_table_count <- total_table_count + ds$properties$tableCount
      total_size_gb <- total_size_gb + ds$properties$totalNumBytes / (1024^3)
      created_at_all <- c(created_at_all, ds$properties$createdAt / 1000)
      updated_at_all <- c(updated_at_all, ds$properties$updatedAt / 1000)
    }
    
    message(strrep("-", 50))
    message("IRW Database Information (Combined)")
    message(strrep("-", 50))
    message(sprintf("%-25s %d", "Total Table Count:", total_table_count))
    message(sprintf("%-25s %.2f GB", "Total Data Size:", total_size_gb))
    message(sprintf("%-25s %s", "Earliest Created At:",
                    format(as.POSIXct(min(created_at_all), origin = "1970-01-01", tz = "UTC"),
                           "%Y-%m-%d %H:%M:%S")))
    message(sprintf("%-25s %s", "Latest Updated At:",
                    format(as.POSIXct(max(updated_at_all), origin = "1970-01-01", tz = "UTC"),
                           "%Y-%m-%d %H:%M:%S")))
    message(strrep("-", 50))
    message(sprintf("%-25s %s", "Data Website:", "https://datapages.github.io/irw/"))
    message(sprintf("%-25s %s", "Methodology:",
                    "Tables harmonized as per https://datapages.github.io/irw/standard.html"))
    message(sprintf("%-25s %s", "Usage Information:",
                    "License & citation info: https://datapages.github.io/irw/docs.html"))
    message(strrep("-", 50))
    
    # --- Optional per-dataset details ---
    if (details) {
      for (i in seq_along(ds_list)) {
        ds <- ds_list[[i]]
        message(sprintf("Dataset %d:", i))
        message(sprintf("  %-22s %s", "Version:", ds$properties$version$tag))
        message(sprintf("  %-22s %d", "Table Count:", ds$properties$tableCount))
        message(sprintf("  %-22s %.2f GB", "Total Data Size:",
                        ds$properties$totalNumBytes / (1024^3)))
        message(sprintf("  %-22s %s", "Created At:",
                        format(as.POSIXct(ds$properties$createdAt / 1000,
                                          origin = "1970-01-01", tz = "UTC"),
                               "%Y-%m-%d %H:%M:%S")))
        message(sprintf("  %-22s %s", "Last Updated At:",
                        format(as.POSIXct(ds$properties$updatedAt / 1000,
                                          origin = "1970-01-01", tz = "UTC"),
                               "%Y-%m-%d %H:%M:%S")))
        message(sprintf("  %-22s %s", "Redivis URL:", ds$properties$url))
        message(strrep("-", 50))
      }
    }
  } else {
    # --- Table-specific info (unchanged) ---
    table <- .fetch_redivis_table(table_name)
    ds_version <- attr(table, "dataset_version")
    bib <- .fetch_biblio_table()
    thisbib <- bib[bib$table == table_name, ]
    
    tags <- .fetch_tags_table()
    construct <- tags$construct_name[tags$table == table_name]
    if (length(construct) == 0 || is.na(construct)) {
      construct <- "No construct specified"
    }
    
    name <- table$properties$name
    num_rows <- table$properties$numRows
    data_size <- table$properties$numBytes / 1024
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
    message(sprintf("%-25s %s", "Construct:", construct))
    message(sprintf("%-25s %d", "Number of Rows:", num_rows))
    message(sprintf("%-25s %d", "Variable Count:", variable_count))
    message(sprintf("%-25s %.2f KB", "Data Size (KB):", data_size))
    message(strrep("-", 50))
    if (!is.null(ds_version) && !is.na(ds_version) && nzchar(ds_version)) {
      message(sprintf("%-25s %s", "Dataset Version:", ds_version))
    }
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
