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
  # Initialize lists
  valid_entries <- character()
  missing_tables <- character()
  missing_doi_tables <- character()
  
  # Fetch the full biblio table once
  biblio <- .fetch_biblio_table()
  
  # Process each table name
  for (table_name in table_names) {
    # Try to confirm the table exists in IRW
    exists <- tryCatch({
      .fetch_redivis_table(table_name)
      TRUE
    }, error = function(e) {
      FALSE
    })
    
    if (!exists) {
      missing_tables <- c(missing_tables, table_name)
      next
    }
    
    # Get the corresponding row from biblio
    biblio_entry <- biblio[biblio$table == table_name, ]
    if (nrow(biblio_entry) == 0) {
      missing_doi_tables <- c(missing_doi_tables, table_name)
      next
    }
    
    # --- Step 1: Try manual BibTeX ---
    bibtex <- trimws(biblio_entry$BibTex)
    
    if (is.null(bibtex) || is.na(bibtex) || !nzchar(bibtex) || !grepl("@", bibtex)) {
      # --- Step 2: Try DOI-based BibTeX ---
      bibtex <- NULL  # Reset
      doi <- biblio_entry$DOI__for_paper_
      
      if (!is.na(doi) && nzchar(doi)) {
        response <- tryCatch(
          httr::GET(
            glue::glue("https://doi.org/{doi}"),
            httr::add_headers(Accept = "application/x-bibtex")
          ),
          error = function(e) NULL
        )
        if (!is.null(response) && httr::status_code(response) == 200) {
          fetched_bibtex <- trimws(httr::content(response, as = "text", encoding = "UTF-8"))
          if (nzchar(fetched_bibtex) && grepl("@", fetched_bibtex)) {
            bibtex <- fetched_bibtex
          }
        }
      }
    }
    
    # --- Step 3: If all else fails ---
    if (is.null(bibtex) || is.na(bibtex) || !nzchar(bibtex) || !grepl("@", bibtex)) {
      missing_doi_tables <- c(missing_doi_tables, table_name)
      next
    }
    
    # --- Step 4: Replace BibTeX key with table name ---
    bibtex <- sub(
      "@(\\w+)\\{[^,]+,",
      paste0("@\\1{", table_name, ","),
      bibtex
    )
    valid_entries <- c(valid_entries, bibtex)
  }
  
  # Write BibTeX entries to file
  if (length(valid_entries) > 0) {
    writeLines(paste0(unique(valid_entries), "\n"), con = output_file)
    message("BibTeX entries saved to: ", output_file)
  }
  
  # Final summary
  message(length(table_names), " table(s) requested; ",
          length(valid_entries), " BibTeX entry(ies) saved.")
  
  # Show messages for skipped tables
  if (length(missing_tables) > 0) {
    message(
      "NOTE: These tables were not processed because they do not exist in the IRW database:\n",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  if (length(missing_doi_tables) > 0) {
    message(
      "NOTE: These tables exist in the IRW database but were missing valid BibTeX entries (manual or via DOI):\n",
      paste(missing_doi_tables, collapse = ", ")
    )
  }

  
  invisible(valid_entries)
}
