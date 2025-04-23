#' Fetch Dataset(s) from the Item Response Warehouse
#'
#' Retrieves one or more datasets from IRW and returns them as tibbles.
#' If the dataset includes a character `resp` column, the function attempts to
#' coerce it into numeric. Strings like `"NA"`, `""`, and `NA` are treated as missing values.
#' A warning is issued only if other non-numeric values are encountered.
#'
#' @param name Character vector of one or more dataset names (IRW table IDs).
#'
#' @return If a single name is provided, returns a tibble. If multiple, returns a named list
#'         of tibbles (or error messages, if retrieval failed).
#'
#' @examples
#' \dontrun{
#' irw_fetch("alcoholresearch_sumscore")
#' }
#' @export
irw_fetch <- function(name) {
  # Helper to fetch one dataset and recode 'resp' if needed
  fetch_single_data <- function(table_id) {
    tryCatch(
      {
        table_obj <- suppressMessages(.fetch_redivis_table(table_id))
        df <- table_obj$to_tibble()
        
        # Recode 'resp' from character to numeric if needed
        if ("resp" %in% names(df) && is.character(df$resp)) {
          suppressWarnings({
            new_resp <- as.numeric(df$resp)
          })
          
          # Identify truly problematic entries (not NA, "", or "NA")
          non_numeric_values <- is.na(new_resp) & !(tolower(trimws(df$resp)) %in% c("na", "", NA))
          
          if (any(non_numeric_values)) {
            warning(sprintf(
              "In dataset '%s': 'resp' column contained non-numeric values that could not be coerced. Some NAs were introduced.",
              table_id
            ))
          }
          
          df$resp <- new_resp
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
        return(error_message)
      }
    )
  }
  
  # Decide single vs multiple
  if (length(name) == 1 && is.character(name)) {
    return(fetch_single_data(name))
  } else {
    dataset_list <- lapply(name, fetch_single_data)
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


