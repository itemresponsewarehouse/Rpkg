#' Merge Tables Sharing the Same DOI or BibTex
#'
#' Identifies and merges tables that share the same DOI or, if DOI is missing, the same BibTex entry.
#' If tables do not have the same structure, only those with identical structures will be merged.
#'
#' @param table_name A character string specifying the name of the table to find merge candidates for.
#' @param add_source_column A boolean value indicating whether to add the `source_table` column (default is FALSE).
#' @return A merged data frame containing all tables with the same DOI or BibTex, or NULL if no merge candidates are found.
#' @export
irw_merge <- function(table_name, add_source_column = FALSE) {
  # Fetch bibliography table
  bib <- .fetch_biblio_table()
  print(dim(bib))
  
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
  
  # Fetch tables and merge incrementally
  merged_table <- NULL
  all_tables <- list()  # Keep track of the tables to perform the checks after fetching
  
  for (tbl_name in merge_candidates) {
    # Fetch the table (data and structure)
    data <- irw_fetch(tbl_name)  # Fetch table
    
    # Add the table to the list of fetched tables
    all_tables[[tbl_name]] <- list(data = data, structure = colnames(data))
    
    
    message(sprintf("Fetching table: %s (Rows: %d, Columns: %d)", tbl_name, nrow(data), ncol(data)))
    
    # Attempt to rbind with the existing merged table
    tryCatch({
      if (is.null(merged_table)) {
        merged_table <- data  # Initialize the merged table
      } else {
        merged_table <- rbind(merged_table, data)  # Try to merge incrementally
      }
    }, error = function(e) {
      # If rbind fails, stop the function and show error message
      stop(sprintf("Merging aborted due to structural mismatch: %s", e$message))  # Stop the function
    })
  }
  
  # Now, check IDs and items after fetching all tables
  check_ids_and_items <- function() {
    id_columns <- lapply(all_tables, function(x) x$data$id)  # Check 'id' column consistency
    item_columns <- lapply(all_tables, function(x) x$data$item)  # Check 'item' columns
    
    # Check if IDs are just a sequence of numbers (1, 2, 3, ...)
    id_consistency <- all(sapply(id_columns, function(x) {
      all(x == seq_along(x))  # Check if the IDs are a sequence (1, 2, 3, ...)
    }))
    
    if (id_consistency) {
      message("\nNOTE: IDs are sequential (1...n). You may need to manually verify the IDs, as there could be multiple studies with different subjects, where IDs are the same in both studies (e.g., 1, 2, 3,...). Proceed with caution.")
    } else {
      # Check if IDs are the same across all datasets
      id_consistency <- all(sapply(id_columns, function(x) length(intersect(x, id_columns[[1]])) > 0))
      if (!id_consistency) {
        message("\nNOTE: IDs do not match across tables. Proceed with caution.")
      }
    }
    
    # Check if item columns are unique across datasets
    item_consistency <- TRUE
    if (length(item_columns) > 1) {
      for (i in 1:(length(item_columns) - 1)) {
        for (j in (i + 1):length(item_columns)) {
          # Check for intersection between items in different datasets
          if (length(intersect(item_columns[[i]], item_columns[[j]])) > 0) {
            item_consistency <- FALSE
            break
          }
        }
        if (!item_consistency) break
      }
    }
    
    if (!item_consistency) {
      message("\nNOTE: There are items that overlap across tables. Proceed with caution.")
    }
    
    # Return whether merging should continue (based on item consistency and id consistency)
    return(id_consistency && item_consistency)
  }
  
  # Check IDs and items, and prompt user if needed
  proceed <- check_ids_and_items()
  
  if (!proceed) {
    proceed_input <- readline(prompt = "Do you still want to proceed with merging? (yes/no): ")
    
    # Handle user input
    while (tolower(proceed_input) != "yes" && tolower(proceed_input) != "no") {
      proceed_input <- readline(prompt = "Please answer 'yes' or 'no': ")
    }
    
    if (tolower(proceed_input) == "yes") {
      add_source_column <- TRUE  # Set add_source_column to TRUE if user confirms
    } else {
      message("Merge operation canceled.")
      return(NULL)
    }
  }
  
  if (add_source_column) {
    merged_table$source_table <- rep(names(all_tables), sapply(all_tables, function(x) nrow(x$data)))
  }
  # Print final processing message
  message(sprintf("\nProcessing done. Merged table dimension: Rows: %d, Columns: %d", nrow(merged_table), ncol(merged_table)))
  if (add_source_column) {
    message("The merged table includes a 'source_table' column indicating the source of each row.")
  }
  return(merged_table)
}