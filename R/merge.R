##### Helper functions for irw_merge() #####

#' Flag bibliography values that identify a real citation
#'
#' Biblio columns store missing citations as the literal string "NA" (and
#' occasionally as an empty string), so grouping on the raw column would treat
#' "no citation" as a shared citation.
#'
#' @param x A character vector from the biblio table.
#' @return A logical vector, TRUE where the value is a usable citation key.
#' @noRd
.is_present_biblio_value <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & nzchar(x) & x != "NA"
}

#' Stack data frames on the union of their columns
#'
#' Columns a member lacks are added as NA. If rbind() cannot combine them
#' because a column's class differs across members, those columns are coerced
#' to character and the stack is retried.
#'
#' @param tables A named list of data frames.
#' @return One data frame with row names reset.
#' @noRd
.stack_union <- function(tables) {
  cols <- unique(unlist(lapply(tables, names), use.names = FALSE))
  pad <- function(d) {
    for (cn in setdiff(cols, names(d))) d[[cn]] <- NA
    d[, cols, drop = FALSE]
  }
  padded <- lapply(tables, pad)
  out <- tryCatch(do.call(rbind, unname(padded)), error = function(e) NULL)
  if (is.null(out)) {
    # Columns whose class differs across members (ignoring all-NA padding)
    cls <- function(cn) unique(unlist(lapply(tables, function(d)
      if (cn %in% names(d)) paste(class(d[[cn]]), collapse = "/"))))
    clash <- cols[vapply(cols, function(cn) length(cls(cn)) > 1, logical(1))]
    padded <- lapply(padded, function(d) {
      for (cn in clash) d[[cn]] <- as.character(d[[cn]])
      d
    })
    out <- do.call(rbind, unname(padded))
  }
  rownames(out) <- NULL
  out
}

#' Ask a yes/no question at the console
#'
#' In a non-interactive session `readline()` returns "" immediately, so this
#' helper never loops there: it reports the assumed answer and returns it.
#' Interactively it re-asks until the input is valid, giving up after a bounded
#' number of attempts rather than looping forever.
#'
#' @param prompt Character. The question to display.
#' @param default Logical. The answer assumed when no input can be read.
#' @param max_attempts Integer. Maximum number of invalid inputs tolerated.
#' @return Logical, TRUE for "yes" and FALSE for "no".
#' @noRd
.prompt_yes_no <- function(prompt, default = TRUE, max_attempts = 5L) {
  if (!interactive()) {
    message(prompt, if (default) "yes" else "no",
            " (assumed; session is not interactive)")
    return(default)
  }

  for (i in seq_len(max_attempts)) {
    answer <- tolower(trimws(readline(prompt = prompt)))
    if (answer %in% c("yes", "y")) return(TRUE)
    if (answer %in% c("no", "n")) return(FALSE)
    message("Invalid input. Please enter 'yes' or 'no'.")
  }

  message("No valid input received; assuming '", if (default) "yes" else "no", "'.")
  default
}

#' Generate DOI and BibTex mappings from a bibliography table
#'
#' This function creates mappings of table names grouped by their DOI and BibTex keys.
#' It ignores entries with missing, empty, or "NA" DOI/BibTex values, so that tables
#' lacking a bibliography entry are never grouped with one another.
#'
#' @param biblio_table A data frame containing columns `DOI__for_paper_`, `BibTex`, and `table`.
#' @return A list containing two named lists:
#'   - `doi_map`: A list mapping DOI values to associated table names.
#'   - `bibtex_map`: A list mapping BibTex keys to associated table names.
#'   - `data_doi_map`: A list mapping data DOIs to table names, built only from
#'     rows with no paper DOI and consulted last.
#' @noRd
generate_doi_bibtex_mapping <- function(biblio_table) {
  valid_doi_entries <- .is_present_biblio_value(biblio_table$DOI__for_paper_)
  valid_bibtex_entries <- .is_present_biblio_value(biblio_table$BibTex)

  doi_map <- split(biblio_table$table[valid_doi_entries], biblio_table$DOI__for_paper_[valid_doi_entries])
  bibtex_map <- split(biblio_table$table[valid_bibtex_entries], biblio_table$BibTex[valid_bibtex_entries])

  doi_map <- doi_map[lengths(doi_map) > 1] # Keep only DOIs with multiple tables
  bibtex_map <- bibtex_map[lengths(bibtex_map) > 1] # Keep only BibTex with multiple tables

  # Last resort: the data (deposit) DOI, for tables whose paper DOI is blank
  # (a deposit DOI is cleared from DOI__for_paper_ for citation; irw#3572).
  data_doi_map <- list()
  if ("DOI__for_data_" %in% names(biblio_table)) {
    valid_data_doi <- .is_present_biblio_value(biblio_table$DOI__for_data_) & !valid_doi_entries
    data_doi_map <- split(biblio_table$table[valid_data_doi], biblio_table$DOI__for_data_[valid_data_doi])
    data_doi_map <- data_doi_map[lengths(data_doi_map) > 1]
  }

  return(list(doi_map = doi_map, bibtex_map = bibtex_map, data_doi_map = data_doi_map))
}

#' Find tables that can be merged based on DOI or BibTex
#'
#' Given a table name and precomputed DOI/BibTex mappings, this function retrieves
#' all tables that share the same DOI or BibTex key. It ensures that the input `table_name`
#' is the first element in the returned list.
#'
#' @param table_name A character string specifying the table name to find merge candidates for.
#' @param maps A list containing DOI and BibTex mappings (as generated by `generate_doi_bibtex_mapping`).
#' @return A character vector of table names, ensuring the input `table_name` appears first.
#' @noRd
find_merge_candidates <- function(table_name, maps) {
  candidates <- NULL

  for (doi in names(maps$doi_map)) {
    if (table_name %in% maps$doi_map[[doi]]) {
      candidates <- maps$doi_map[[doi]]
      break
    }
  }

  if (is.null(candidates)) {
    for (bibtex in names(maps$bibtex_map)) {
      if (table_name %in% maps$bibtex_map[[bibtex]]) {
        candidates <- maps$bibtex_map[[bibtex]]
        break
      }
    }
  }

  if (is.null(candidates)) {
    for (data_doi in names(maps$data_doi_map)) {
      if (table_name %in% maps$data_doi_map[[data_doi]]) {
        candidates <- maps$data_doi_map[[data_doi]]
        break
      }
    }
  }

  # Ensure input table_name is first in the list
  if (!is.null(candidates)) {
    candidates <- c(table_name, setdiff(candidates, table_name))
  }

  return(candidates) # Returns ordered list with input table_name first
}

#' Get Number of Respondents from Metadata
#'
#' This function retrieves the number of respondents for each table from metadata.csv.
#' It calls irw_metadata() to get the metadata and extracts the relevant information.
#'
#' @return A named vector where names are table names and values are the number of respondents.
#' @noRd
get_n_respondents <- function() {
  metadata <- irw_metadata()
  n_respondents <- metadata$n_participants
  names(n_respondents) <- metadata$table
  return(n_respondents)
}

#' Check Number of Respondents Consistency
#'
#' This function checks if the number of respondents is consistent across all tables to be merged.
#' It retrieves the number of respondents from metadata.csv and displays the counts before asking
#' the user if they want to proceed.
#'
#' @param table_names A character vector of table names to be checked.
#' @return Logical value:
#'   - `TRUE` if the user confirms to proceed.
#'   - `FALSE` if the user chooses not to proceed.
#' @noRd
check_n_respondents <- function(table_names) {
  n_respondents <- get_n_respondents()
  n_values <- n_respondents[table_names]
  unique_n <- unique(n_values)

  # Display N respondents for each table
  message("\033[1m\n=== Found ", length(table_names), " Tables to Merge ===\033[0m")
  for (tbl in table_names) {
    message(sprintf("%s (N = %d)", tbl, n_respondents[tbl]))
  }

  # Check if all N respondents are equal
  if (length(unique_n) == 1) {
    message("\nAll tables have the same number of respondents (N = ", unique_n, ").")
  } else {
    # Display bold-faced warning if N respondents are not consistent
    message("\033[1m\n=== WARNING ===\033[0m")
    message("\033[1mThe number of respondents (N) is not consistent across tables.\033[0m")
    message("\033[1mMerging tables with inconsistent N respondents may require human judgment.\033[0m")
  }

  # Ask user if they want to proceed
  proceed <- .prompt_yes_no("Do you want to proceed with merging these tables? (yes/no): ")

  if (!proceed) {
    message("Merge operation canceled.")
    return(FALSE)
  }

  return(TRUE)
}

#' Check consistency of IDs and items across multiple tables
#'
#' This function verifies that:
#' 1. All tables share the same set of IDs.
#' 2. The IDs (if shared) form a strict sequence (e.g., 1,2,3,...).
#' 3. There is no overlap of item columns across tables.
#'
#' If any of these conditions are violated, a warning message is displayed.
#'
#' @param all_tables A named list of data frames, where each data frame represents a table.
#' @return Logical value:
#'   - `TRUE` if all conditions pass without warning.
#'   - `FALSE` if any warning condition is triggered.
#' @noRd
check_ids_and_items <- function(all_tables) {
  id_columns <- lapply(all_tables, function(x) sort(unique(na.omit(x$id)))) # Extract unique IDs, removing NA
  item_columns <- lapply(all_tables, function(x) unique(na.omit(x$item))) # Extract unique items

  messages <- c() # Collect messages

  # Step 1: Check if all tables share the same set of IDs
  all_ids <- Reduce(union, id_columns) # Get all unique IDs across tables
  shared_ids <- Reduce(intersect, id_columns) # Get IDs that are common across all tables
  id_match <- length(all_ids) == length(shared_ids) # If all IDs are shared, this should be TRUE

  if (!id_match) {
    messages <- c(messages, "IDs do not match across tables.")
  }

  # Step 2: If there are any IDs shared, further check if they form a strict sequence (1,2,3,...)
  id_sequential <- FALSE
  any_id_overlap <- length(shared_ids) > 0 # TRUE if there's any overlap
  if (any_id_overlap) {
    expected_seq <- seq(min(shared_ids), max(shared_ids)) # Expected sequence from min to max ID
    id_sequential <- length(shared_ids) == length(expected_seq) &&
      all(shared_ids == expected_seq)

    if (id_sequential) {
      messages <- c(messages, "IDs are sequential (1...n). You may need to manually verify the IDs, as there could be multiple studies with different subjects, where IDs are the same in both studies.")
    }
  }

  # Step 3: Check if item columns have overlapping items across tables
  # (only meaningful with at least two tables to compare)
  item_overlap <- FALSE
  if (length(item_columns) > 1) {
    item_overlap <- any(vapply(seq_len(length(item_columns) - 1), function(i) {
      any(vapply(seq.int(i + 1, length(item_columns)), function(j) {
        length(intersect(item_columns[[i]], item_columns[[j]])) > 0
      }, logical(1)))
    }, logical(1)))
  }

  if (item_overlap) {
    messages <- c(messages, "There are items that overlap across tables.")
  }

  # Print all messages at the end
  if (length(messages) > 0) {
    message("\033[1m\n=== NOTE ===\033[0m") 
    message("\033[1m- ", paste(messages, collapse = "\n- "), "\033[0m")
  }
  # Return TRUE only if none of the warning messages were triggered
  return(length(messages) == 0)
}


#' Merge Tables Sharing the Same DOI or BibTex
#'
#' Identifies and merges tables that share the same DOI or, if DOI is missing, the same BibTex entry.
#' If tables do not have the same structure, only those with identical structures will be merged.
#' This function performs several consistency checks before merging, 
#' and prompts the user for confirmation if inconsistencies are found.
#'
#' @param table_name A character string specifying the name of the table to find merge candidates for.
#' @param add_source_column A boolean value indicating whether to add the `source_table` column 
#' to the merged table (default is TRUE). This column keeps track of the original source of each row.
#' 
#' @details 
#' `irw_merge()` performs the following checks before proceeding with the merge:
#' 
#' 1. **Number of Respondents Check:**
#'    - The function checks if the number of respondents is consistent across all tables.
#'    - If the number of respondents is consistent, the merge proceeds.
#'    - If the number of respondents is inconsistent across tables, a **WARNING** message is displayed:
#'      ```
#'      === WARNING ===
#'      The number of respondents (N) is not consistent across tables.
#'      Merging tables with inconsistent N respondents may require human judgment.
#'      ```
#' 
#' 2. **ID Consistency Check:**
#'    - Checks if all tables share the same set of IDs.
#'    - Checks if the IDs form a strict sequence (e.g., 1, 2, 3, ...).
#'    - If IDs are not consistent across tables, a **NOTE** message is displayed:
#'      ```
#'      === NOTE: ===
#'      - IDs do not match across tables.
#'      - IDs are sequential (1...n). You may need to manually verify the IDs, 
#'        as there could be multiple studies with different subjects, where IDs 
#'        are the same in both studies.
#'      ```
#'
#' 3. **Item Column Overlap Check:**
#'    - Verifies that there is no overlap of item columns across tables.
#'    - If overlapping items are found, a **NOTE** message is displayed:
#'      ```
#'      === NOTE: ===
#'      - There are items that overlap across tables.
#'      ```
#'
#' Tables whose bibliography entry is missing (recorded as `"NA"` in the biblio
#' table) are never treated as merge candidates for one another.
#'
#' In a non-interactive session the confirmation prompts cannot be answered, so
#' the merge proceeds by default; the checks above are still reported as messages.
#'
#' @return A merged data frame containing all tables with the same DOI or BibTex, or NULL if no merge candidates are found.
#' @export
irw_merge <- function(table_name, add_source_column = TRUE) {
  # Fetch bibliography table
  bib <- .fetch_biblio_table()

  # Find merge candidates
  maps <- generate_doi_bibtex_mapping(bib)
  merge_candidates <- find_merge_candidates(table_name, maps)

  if (is.null(merge_candidates)) {
    message("No mergeable tables found for ", table_name)
    return(NULL)
  }

  # Check N respondents before fetching any data
  proceed <- check_n_respondents(merge_candidates)
  if (!proceed) {
    return(NULL) # User chose not to proceed
  }

  # Fetch every member first, then stack them on the union of their columns.
  # Members routinely carry different cov_* columns; an incremental rbind()
  # made the first table fetched the reference and silently dropped every
  # member whose columns differed (Rpkg#224).
  all_tables <- list() # Tables fetched successfully
  skipped_tables <- list() # Tables that could not be fetched

  message("\033[1m\n=== Fetching Tables ===\033[0m")
  for (tbl_name in merge_candidates) {
    data <- irw_fetch(tbl_name)

    # irw_fetch() returns NULL when the table is listed in the bibliography but
    # is not present in any IRW datasource; record it and move on.
    if (is.null(data) || nrow(data) == 0) {
      skipped_tables[[tbl_name]] <- "table could not be fetched"
      message(sprintf("  - Skipping table '%s': no data returned.", tbl_name))
      next
    }

    message(sprintf("- Fetching table: %s (Rows: %d, Columns: %d)...", tbl_name, nrow(data), ncol(data)))

    # Add source column before merging if requested
    if (add_source_column) data$source_table <- tbl_name
    all_tables[[tbl_name]] <- data
  }

  # Nothing to merge unless at least two members were fetched
  if (length(all_tables) < 2) {
    message("\nMerging failed for all tables. No data merged.")
    return(NULL)
  }

  merged_table <- .stack_union(all_tables)
  partial_cols <- setdiff(
    names(merged_table),
    Reduce(intersect, lapply(all_tables, names))
  )

  # Check IDs and items, and prompt user if needed
  proceed <- check_ids_and_items(all_tables)

  if (!proceed) {
    if (!.prompt_yes_no("Do you still want to proceed with merging? (yes/no): ")) {
      message("Merge operation canceled.")
      return(NULL)
    }
  }


  # Print final processing message
  message("\033[1m\n=== Processing Summary ===\033[0m")
  message(sprintf("- Merged table dimension: (Rows: %d, Columns: %d).", nrow(merged_table), ncol(merged_table)))

  if (add_source_column) {
    message("- The merged table includes a 'source_table' column indicating the source of each row.")
  }

  if (length(partial_cols) > 0) {
    message("- Columns not present in every table (NA where absent): ",
            paste(partial_cols, collapse = ", "), ".")
  }

  # Print summary of skipped tables (if any). A warning, not only a message:
  # a partial merge must not look like a complete one to a non-interactive caller.
  if (length(skipped_tables) > 0) {
    message("\033[1m\n=== Skipped Tables ===\033[0m")
    message("The following tables were skipped:")
    for (tbl in names(skipped_tables)) {
      message(sprintf("- %s (%s)", tbl, skipped_tables[[tbl]]))
    }
    warning(sprintf(
      "irw_merge(): %d of %d tables were not merged: %s",
      length(skipped_tables), length(merge_candidates),
      paste(names(skipped_tables), collapse = ", ")
    ), call. = FALSE)
  }

  return(merged_table)
}
