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
  .check_redivis()
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
  .check_redivis()
  valid_entries <- character()
  missing_tables <- character()
  missing_doi_tables <- character()
  
  # Process each table name
  for (table_name in table_names) {
    # Check if the table exists in IRW by attempting to fetch it
    tryCatch({
      # If the table exists in IRW, this will succeed
      irw_table <- .fetch_redivis_table(table_name)
    }, error = function(e) {
      # If an error occurs, the table is missing from IRW
      missing_tables <<- c(missing_tables, table_name)
      return(NULL)  # Skip processing for this table
    })
    
    # If the table is missing from IRW, skip further processing and just report it
    if (table_name %in% missing_tables) {
      next
    }
    
    # Now that the table exists in IRW, fetch the biblio table
    biblio <- .fetch_biblio_table()
    
    # Check if the table is in biblio
    if (!table_name %in% biblio$table) {
      missing_doi_tables <- c(missing_doi_tables, table_name)
      next
    }
    
    # Retrieve DOI from "DOI__for_paper_"
    doi <- biblio[biblio$table == table_name, "DOI__for_paper_", drop = TRUE]
    
    # Attempt to fetch BibTeX from the DOI
    bibtex <- NULL
    if (!is.null(doi) && nzchar(doi)) {
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
        bibtex <- httr::content(response, as = "text", encoding = "UTF-8")
      }
    }
    
    # If DOI-based retrieval fails, use the manual BibTeX entry in biblio
    if (is.null(bibtex)) {
      manual_bibtex <- biblio[biblio$table == table_name, "BibTex", drop = TRUE]
      if (length(manual_bibtex) > 0 && nzchar(manual_bibtex)) {
        bibtex <- manual_bibtex
      } else {
        missing_doi_tables <- c(missing_doi_tables, table_name)
        next
      }
    }
    
    # Update the BibTeX key while preserving the entry type
    if (!is.null(bibtex) && grepl("^@", bibtex)) {
      bibtex <- sub("@(\\w+)\\{[^,]+,",
                    paste0("@\\1{", table_name, ","),
                    bibtex)
      valid_entries <- c(valid_entries, bibtex)
    }
  }
  
  # Save valid entries to file
  if (length(valid_entries) > 0) {
    writeLines(unique(valid_entries), con = output_file)
    message("BibTeX entries saved to: ", output_file)
  }
  
  # Messages for missing tables and missing DOIs
  if (length(missing_tables) > 0) {
    message(
      "NOTE: These tables were not processed because they do not exist in the IRW database. Please check the names:\n",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  if (length(missing_doi_tables) > 0) {
    message(
      "NOTE: These tables exist in the IRW database but were missing valid DOI information, and no manual BibTeX entry was found:\n",
      paste(missing_doi_tables, collapse = ", ")
    )
  }
  
  invisible(valid_entries)
}