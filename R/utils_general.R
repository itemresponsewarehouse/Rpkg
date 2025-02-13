# General-purpose utility functions.

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
irw_download <- function(table_name,
                         path = NULL,
                         overwrite = FALSE) {
  table <- .fetch_redivis_table(table_name)
  # Check if the table object has the download method
  if (!is.function(table$download)) {
    stop(
      "The provided object does not support downloading. Ensure it is a valid Redivis data table."
    )
  }
  
  # Attempt to download the file
  table$download(path = path, overwrite = overwrite)
  
  # Notify user of the download location
  if (is.null(path)) {
    path <- paste0(getwd(), "/", table$name)
  }
  message("Dataset downloaded to: ", path)
}



#' Save BibTeX Entries for IRW Tables
#'
#' Saves BibTeX entries for one or more IRW tables to a specified output file.
#' Updates the BibTeX key to match the table name.
#'
#' @param table_names A character vector of table names for which BibTeX entries are generated.
#' @param output_file A character string specifying the file path to save BibTeX entries. Default is "refs.bib".
#' @return Invisibly returns BibTeX entries as a character vector.
#' @export
irw_save_bibtex <- function(table_names, output_file = "refs.bib") {
  # Initialize lists for valid BibTeX entries and invalid table names
  valid_entries <- character()
  invalid_tables <- character()
  
  # Process each table name
  for (table_name in table_names) {
    table_row <- data_index[data_index$table == table_name, ]
    
    # Handle missing tables
    if (nrow(table_row) == 0) {
      invalid_tables <- c(invalid_tables, table_name)
      next
    }
    
    # Fetch BibTeX from DOI
    doi <- table_row$DOI
    bibtex <- if (!is.na(doi) && nzchar(doi)) {
      response <- tryCatch(
        httr::GET(
          glue::glue("https://doi.org/{doi}"),
          httr::add_headers(Accept = "application/x-bibtex")
        ),
        error = function(e)
          NULL
      )
      if (!is.null(response) &&
          httr::status_code(response) == 200) {
        httr::content(response, as = "text", encoding = "UTF-8")
      } else {
        NULL
      }
    } else {
      NULL
    }
    
    # Fallback to manual BibTeX
    if (is.null(bibtex)) {
      manual_bibtex <- bibtex_manual[bibtex_manual$table == table_name, "BibTex", drop = FALSE]
      if (nrow(manual_bibtex) > 0) {
        bibtex <- manual_bibtex$BibTex
      } else {
        invalid_tables <- c(invalid_tables, table_name)
        next
      }
    }
    
    # Update the BibTeX key while preserving the entry type
    if (!is.null(bibtex) && grepl("^@", bibtex)) {
      bibtex <- sub("@(\\w+)\\{[^,]+,",
                    paste0("@\\1{", table_name, ","),
                    bibtex)
    }
    
    # Add valid BibTeX to the list
    valid_entries <- c(valid_entries, bibtex)
  }
  
  # Save valid entries to file
  if (length(valid_entries) > 0) {
    writeLines(unique(valid_entries), con = output_file)
    message("BibTeX entries saved to: ", output_file)
  } else {
    message("No valid BibTeX entries found. Output file was not created.")
  }
  
  # Message for invalid tables
  if (length(invalid_tables) > 0) {
    message(
      "NOTE: These tables were not saved due to invalid names or missing BibTeX info: ",
      paste(invalid_tables, collapse = ", ")
    )
  }
  
  invisible(valid_entries)
}
