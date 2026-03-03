# General-purpose utility functions.

#' Download a Dataset
#'
#' Downloads a specified dataset table from the IRW datasource.
#'
#' @param table_name A character string specifying the name of the Redivis table
#'        to download.
#' @param path A string specifying the file path where the data should be saved.
#'        If `NULL`, the dataset will be saved in the current working directory
#'        with the table's name as the file name.
#' @param overwrite Logical. Whether to overwrite an existing file.
#' @param sim Logical. If TRUE, download from the simulation dataset.
#' @param comp Logical. If TRUE, download from the competition dataset.
#' @param nom Logical. If TRUE, download from the nominal dataset.
#'
#' @return A message confirming the file download location.
#' @export
irw_download <- function(table_name,
                         path = NULL,
                         overwrite = FALSE,
                         sim = FALSE,
                         comp = FALSE,
                         nom=FALSE) {
  if (!is.character(table_name) || length(table_name) != 1) {
    stop("'table_name' must be a single character string.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("'overwrite' must be a single TRUE or FALSE value.")
  }
  if (!is.logical(sim) || length(sim) != 1) {
    stop("'sim' must be a single TRUE or FALSE value.")
  }
  if (!is.logical(comp) || length(comp) != 1) {
    stop("'comp' must be a single TRUE or FALSE value.")
  }
  if (!is.logical(nom) || length(nom) != 1) {
    stop("'nom' must be a single TRUE or FALSE value.")
  }
  
  n_sources <- sum(c(isTRUE(sim), isTRUE(comp), isTRUE(nom)))
  if (n_sources > 1L) {
    stop("Cannot set more than one of sim = TRUE, comp = TRUE, nom = TRUE.")
  }
  
  table <- tryCatch(
    .fetch_redivis_table(table_name, sim = isTRUE(sim), comp = isTRUE(comp), nom = isTRUE(nom)),
    error = function(e) {
      msg <- paste0(
        "Table '", table_name, "' not found in the selected dataset."
      )
      if (!isTRUE(sim) && !isTRUE(comp) && !isTRUE(nom)) {
        msg <- paste0(
          msg,
          "\nHint: If this is a simulation, competition, or nominal table, try sim = TRUE, comp = TRUE, or nom = TRUE."
        )
      }
      stop(msg, call. = FALSE)
    }
  )
  
  if (!is.function(table$download)) {
    stop(
      "The provided object does not support downloading. ",
      "Ensure it is a valid Redivis data table."
    )
  }
  
  table$download(path = path, overwrite = overwrite)
  
  if (is.null(path)) {
    path <- file.path(getwd(), table$name)
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
#' @param comp Logical. If TRUE, uses competition bibliography and validates tables against the competition dataset.
#' @param sim Logical. If TRUE, uses simulation bibliography and validates tables against the simulation dataset.
#' @param nom Logical. If TRUE, uses nominal bibliography and validates tables against the nominal dataset.
#'   Only one of comp/sim/nom may be TRUE.
#' @return Invisibly returns BibTeX entries as a character vector.
#' @export
irw_save_bibtex <- function(table_names,
                            output_file = "refs.bib",
                            comp = FALSE,
                            sim = FALSE,
                            nom = FALSE) {
  if (!is.character(table_names) || length(table_names) < 1) {
    stop("'table_names' must be a non-empty character vector.")
  }
  if (!is.character(output_file) || length(output_file) != 1) {
    stop("'output_file' must be a single character string.")
  }
  if (!is.logical(comp) || length(comp) != 1) stop("'comp' must be a single TRUE or FALSE value.")
  if (!is.logical(sim)  || length(sim)  != 1) stop("'sim' must be a single TRUE or FALSE value.")
  if (!is.logical(nom)  || length(nom)  != 1) stop("'nom' must be a single TRUE or FALSE value.")
  n_sources <- sum(c(isTRUE(comp), isTRUE(sim), isTRUE(nom)))
  if (n_sources > 1L) stop("Cannot set more than one of comp = TRUE, sim = TRUE, nom = TRUE.")
  
  # Initialize lists
  valid_entries <- character()
  missing_tables <- character()
  missing_bib_tables <- character()
  missing_doi_tables <- character()
  
  # Fetch the full biblio table once (by source)
  biblio <- if (isTRUE(comp)) {
    .fetch_comps_biblio_table()
  } else if (isTRUE(sim)) {
    .fetch_simsyn_biblio_table()
  } else if (isTRUE(nom)) {
    .fetch_nominal_biblio_table()
  } else {
    .fetch_biblio_table()
  }
  
  table_exists <- function(tbl_name) {
    tryCatch({
      .fetch_redivis_table(
        tbl_name,
        comp = isTRUE(comp),
        sim  = isTRUE(sim),
        nom  = isTRUE(nom)
      )
      TRUE
    }, error = function(e) FALSE)
  }
  
  table_names <- unique(table_names)
  
  for (table_name in table_names) {
    if (!table_exists(table_name)) {
      missing_tables <- c(missing_tables, table_name)
      next
    }
    
    biblio_entry <- biblio[biblio$table == table_name, , drop = FALSE]
    if (nrow(biblio_entry) == 0) {
      missing_bib_tables <- c(missing_bib_tables, table_name)
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
      "NOTE: These tables were not processed because they do not exist in the selected IRW dataset:\n",
      paste(missing_tables, collapse = ", "),
      if (!isTRUE(comp))
        "\nHint: If these are competition tables, try calling with comp = TRUE."
    )
  }
  
  
  if (length(missing_bib_tables) > 0) {
    message(
      "NOTE: These tables exist in the IRW database but have no row in the bibliography table:\n",
      paste(missing_bib_tables, collapse = ", ")
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
