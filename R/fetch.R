#' Fetch Dataset(s) from the Item Response Warehouse
#'
#' Retrieves one or more datasets from IRW and returns them as tibbles.
#' If the dataset includes a character `resp` column, the function attempts to
#' coerce it into numeric. Strings like `"NA"`, `""`, and `NA` are treated as missing values.
#' A warning is issued only if other non-numeric values are encountered.
#'
#' @param name Character vector of one or more dataset names (IRW table IDs).
#' @param sim Logical, optional. If TRUE, fetches from the IRW simulation dataset. Defaults to FALSE.
#' @param dedup Logical, optional. If TRUE, deduplicates responses based on timing variables. Defaults to FALSE.
#'   - If a 'date' column is present, no deduplication is performed.
#'   - If only a 'wave' column is present, one random response is retained per (id, item) pair within each wave.
#'   - If neither 'date' nor 'wave' is present, one random response is retained per (id, item) pair.
#'
#' @return If a single name is provided, returns a tibble. If multiple, returns a named list
#'         of tibbles (or error messages, if retrieval failed).
#'
#' @examples
#' \dontrun{
#' irw_fetch("environment_ltm")
#' irw_fetch("gilbert_meta_3", sim = TRUE)
#' irw_fetch("pks_probability", dedup = TRUE)
#' }
#' @export
irw_fetch <- function(name, sim = FALSE, dedup = FALSE) {
  # Helper to fetch one dataset and recode 'resp' if needed
  fetch_single_data <- function(table_id, sim = FALSE) {
    tryCatch(
      {
        table_obj <- suppressMessages(.fetch_redivis_table(table_id, sim))
        df <- table_obj$to_tibble()

        # Recode 'resp' from character to numeric if needed
        if ("resp" %in% names(df) && is.character(df$resp)) {
          suppressWarnings({
            new_resp <- as.numeric(df$resp)
          })

          non_numeric_values <- is.na(new_resp) & !(tolower(trimws(df$resp)) %in% c("na", "", NA))

          if (any(non_numeric_values)) {
            warning(sprintf(
              "In dataset '%s': 'resp' column contained non-numeric values that could not be coerced. Some NAs were introduced.",
              table_id
            ))
          }

          df$resp <- new_resp
        }

        # Deduplication logic
        if (dedup) {
          n_before <- nrow(df)
          
          if ("date" %in% names(df)) {
            message(sprintf(
              "Deduplication skipped for dataset '%s': 'date' column detected (timestamped responses).", table_id
            ))
            
          } else {
            # Determine grouping keys
            if ("wave" %in% names(df)) {
              grouping_keys <- list(df$id, df$item, df$wave)
              success_msg <- sprintf(
                "Deduplicated dataset '%s': one response randomly retained per (id, item, wave) group.", table_id
              )
            } else {
              grouping_keys <- list(df$id, df$item)
              success_msg <- sprintf(
                "Deduplicated dataset '%s': one response randomly retained per (id, item) pair.", table_id
              )
            }
            
            # Perform deduplication
            split_df <- split(df, grouping_keys, drop = TRUE)
            df <- do.call(rbind, lapply(split_df, function(g) g[sample(nrow(g), 1), , drop = FALSE]))
            rownames(df) <- NULL
            
            # Compare before/after
            n_after <- nrow(df)
            if (n_after < n_before) {
              message(success_msg)
            } else {
              message(sprintf(
                "Deduplication not needed for dataset '%s': no duplicate responses found.", table_id
              ))
            }
          }
        }

        return(df)
      },
      error = function(e) {
        error_message <- paste(
          "Error fetching dataset",
          shQuote(table_id),
          ":",
          e$message
        )
        message(error_message)
        return(invisible(NULL))
      }
    )
  }

  # Decide single vs multiple
  if (length(name) == 1 && is.character(name)) {
    return(fetch_single_data(name, sim))
  } else {
    dataset_list <- lapply(name, function(nm) fetch_single_data(nm, sim = sim))
    names(dataset_list) <- name
    return(dataset_list)
  }
}

#' Retrieve IRW Metadata Table
#'
#' Fetches the metadata table from Redivis and returns it as a tibble.
#' Link to Redivis table: https://redivis.com/datasets/bdxt-4fqe5tyf4/tables/h5gs-04agty3j1
#' Automatically checks for updates and refreshes only when needed.
#'
#' @return A tibble containing metadata information.
#' @export
irw_metadata <- function() {
  return(.fetch_metadata_table())
}

#' Retrieve IRW Tags Table
#'
#' Fetches the tags table from Redivis and returns it as a tibble.
#' Link to Redivis table: https://redivis.com/datasets/bdxt-4fqe5tyf4/tables/7nkh-6m7wq01yv

#' Automatically checks for updates and refreshes only when needed.
#'
#' @return A tibble containing tags information.
#' @export
irw_tags <- function() {
  return(.fetch_tags_table())
}


#' Retrieve item text metadata for an IRW table
#'
#' Returns item-level text metadata for a given IRW table, if available.
#' If no item text table is available for the specified table, the function returns \code{NULL} 
#' and prints a message.
#'
#' @param table_name Character. The name of the IRW table to look up (e.g., \code{"gilbert_meta_49"}).
#'
#' @return A tibble containing item text metadata, or \code{NULL} if unavailable.
#'
#' @examples
#' \dontrun{
#'   irw_itemtext("gilbert_meta_49")
#' }
#'
#' @export
irw_itemtext <- function(table_name) {
  return(.fetch_itemtext_table(table_name))
}

