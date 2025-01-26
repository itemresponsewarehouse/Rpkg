#' Generate Citation and BibTeX for a Table
#'
#' Generates and displays both a citation and BibTeX entry for a table.
#'
#' @param table_name A character string specifying the name of the table.
#' @return Invisibly returns the formatted string (so it can be captured programmatically if needed).
#' @export
irw_citation <- function(table_name) {
  # Look up the table in the metadata
  table_row <- data_index[data_index$Filename == table_name, ]
  
  if (nrow(table_row) == 0) {
    warning("The table '", table_name, "' does not exist in the metadata.")
    result <- paste0("No citation or BibTeX entry available for the table '", table_name, "'.")
    cat(result, "\n")
    return(invisible(result))
  }
  
  # Extract DOI and Reference
  doi <- table_row$DOI
  reference <- table_row$Reference
  
  # Prepare citation
  if (!is.null(doi) && nzchar(doi) && !is.na(doi)) {
    citation <- paste0("https://doi.org/", doi)
  } else if (!is.null(reference) && nzchar(reference)) {
    citation <- reference
  } else {
    citation <- "No citation available for this table."
  }
  
  # Prepare BibTeX
  bibtex <- NULL
  if (!is.null(doi) && nzchar(doi) && !is.na(doi)) {
    response <- tryCatch({
      httr::GET(
        glue::glue("https://doi.org/{doi}"),
        httr::add_headers(Accept = "application/x-bibtex")
      )
    }, error = function(e) NULL)
    
    if (!is.null(response) && httr::status_code(response) == 200) {
      bibtex <- httr::content(response, as = "text", encoding = "UTF-8")
    }
  }
  
  # Fallback to bibtex_manual if DOI BibTeX is not available
  if (is.null(bibtex)) {
    bibtex_row <- bibtex_manual[bibtex_manual$Filename == table_name, ]
    if (nrow(bibtex_row) > 0) {
      bibtex <- bibtex_row$BibTex
    }
  }
  
  if (is.null(bibtex)) {
    bibtex <- "No BibTeX entry available for this table."
  }
  
  # Combine and format the output
  result <- paste0(
    "Citation for '", table_name, "':\n",
    citation, "\n\n",
    "BibTeX entry for '", table_name, "':\n",
    bibtex
  )
  
  # Display the result using cat()
  cat(result, "\n")
  
  # Return the result invisibly for programmatic use
  return(invisible(result))
}