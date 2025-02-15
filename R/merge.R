#' Merge Tables Sharing the Same DOI or BibTex
#'
#' Identifies and merges tables that share the same DOI or, if DOI is missing, the same BibTex entry.
#' If tables do not have the same structure, only those with identical structures will be merged,
#' and a message will be printed listing tables that could not be merged.
#'
#' @param table_name A character string specifying the name of the table to find merge candidates for.
#' @return A merged data frame containing all tables with the same DOI or BibTex, or NULL if no merge candidates are found.
#' @export
irw_merge <- function(table_name) {
  # Fetch bibliography table
  bib <- .fetch_biblio_table()
  
  # Generate DOI and BibTex mapping, ignoring "NA" DOI values
  generate_doi_bibtex_mapping <- function(biblio_table) {
    valid_doi_entries <- biblio_table$DOI__for_paper_ != "NA" & !is.na(biblio_table$DOI__for_paper_)
    doi_map <- split(biblio_table$table[valid_doi_entries], biblio_table$DOI__for_paper_[valid_doi_entries])
    bibtex_map <- split(biblio_table$table, biblio_table$BibTex)
    
    doi_map <- doi_map[lengths(doi_map) > 1]  # Keep only DOIs with multiple tables
    bibtex_map <- bibtex_map[lengths(bibtex_map) > 1]  # Keep only BibTex with multiple tables
    
    return(list(doi_map = doi_map, bibtex_map = bibtex_map))
  }
  
  # Find merge candidates
  maps <- generate_doi_bibtex_mapping(bib)
  find_merge_candidates <- function(table_name, maps) {
    for (doi in names(maps$doi_map)) {
      if (table_name %in% maps$doi_map[[doi]]) {
        return(maps$doi_map[[doi]])
      }
    }
    for (bibtex in names(maps$bibtex_map)) {
      if (table_name %in% maps$bibtex_map[[bibtex]]) {
        return(maps$bibtex_map[[bibtex]])
      }
    }
    return(NULL)  # No merge candidates
  }
  
  merge_candidates <- find_merge_candidates(table_name, maps)
  
  if (is.null(merge_candidates)) {
    message("No mergeable tables found for ", table_name)
    return(NULL)
  }
  
  # Print merge message with table count before fetching data
  message(sprintf("Found %d tables to merge:", length(merge_candidates)))
  message(paste("  ", merge_candidates, collapse = "\n"))
  
  # Ask user for confirmation before proceeding
  response <- readline(prompt = "Do you want to proceed with merging these tables? (yes/no): ")
  if (tolower(response) != "yes") {
    message("Merge operation canceled.")
    return(NULL)
  }
  
  # Fetch tables and check structures
  fetch_and_check <- function(tbl_name) {
    data <- irw_fetch(tbl_name)  # Fetch table
    structure <- colnames(data)  # Extract column names as structure
    message(sprintf("Processing table: %s (Rows: %d, Columns: %d)", tbl_name, nrow(data), ncol(data)))
    return(list(name = tbl_name, data = data, structure = structure))
  }
  
  table_list <- lapply(merge_candidates, fetch_and_check)
  
  # Identify groups of tables with the same structure
  structure_map <- split(table_list, sapply(table_list, function(x) paste(sort(x$structure), collapse = ",")))
  
  # Merge tables that share the same structure
  merged_tables <- list()
  failed_tables <- c()
  
  for (key in names(structure_map)) {
    subset <- structure_map[[key]]
    if (length(subset) > 1) {
      merged_tables[[key]] <- dplyr::bind_rows(lapply(subset, function(x) x$data))
    } else {
      failed_tables <- c(failed_tables, subset[[1]]$name)
    }
  }
  
  if (length(failed_tables) > 0) {
    message("The following tables could not be merged due to structural differences:")
    for (tbl in failed_tables) {
      message(sprintf("  - %s", tbl))
    }
  }
  
  # Print final processing message
  if (length(merged_tables) == 1) {
    merged_table <- merged_tables[[1]]
    message(sprintf("\nProcessing done. Merged table dimension: Rows: %d, Columns: %d", nrow(merged_table), ncol(merged_table)))
    return(merged_table)  # Return single merged table directly
  } else {
    message("Processing done. Returning multiple merged tables.")
    return(merged_tables)  # Return list of merged tables
  }
}
