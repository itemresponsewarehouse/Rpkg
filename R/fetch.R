#' Fetch Data from the IRW Database
#'
#' Retrieves one or more datasets from the Item Response Warehouse (IRW) database by their names.
#' The datasets are fetched as data frames (tibbles) and can be accessed programmatically for analysis.
#'
#' @param name A character vector specifying one or more dataset names to fetch.
#' @return If a single dataset is fetched, returns a tibble. If multiple datasets are fetched,
#'         returns a named list of tibbles or error messages for datasets that failed to load.
#' @examples
#' \dontrun{
#' # Fetch a single dataset
#' df <- irw_fetch("example_dataset")
#' print(df)
#'
#' # Fetch multiple datasets
#' datasets <- irw_fetch(c("dataset1", "dataset2"))
#' print(names(datasets)) # Outputs: "dataset1" and "dataset2"
#' }
#' @export
irw_fetch <- function(name) {
  # Helper function to fetch a single dataset
  fetch_single_data <- function(table) {
    tryCatch(
      {
        table <- suppressMessages(.fetch_redivis_table(table))
        table$to_tibble()
      },
      error = function(e) {
        error_message <- paste(
          "Error fetching dataset",
          shQuote(table),
          ":",
          e$message
        )
        message(error_message) # print error immediately
        return(error_message) # save error message in list output
      }
    )
  }

  # Check if fetching a single or multiple datasets
  if (length(name) == 1) {
    return(fetch_single_data(name)) # Return a single tibble or error message
  } else {
    dataset_list <- lapply(name, fetch_single_data)
    names(dataset_list) <- name
    return(dataset_list) # Return a named list of tibbles/errors
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

#' View Available Tag Values
#'
#' Returns all unique values for a given tag metadata column used in `irw_filter()`.
#' Useful for discovering valid filter options when working with tag-based metadata
#' (e.g., `age_range`, `construct_type`, `sample`).
#'
#' @param column Character string specifying the tag column name (e.g., "age_range", "sample").
#' @return A character vector of unique values found in the specified tag column.
#' @examples
#' \dontrun{
#' irw_tag_options("age_range")
#' irw_tag_options("construct_type")
#' }
#' @export
irw_tag_options <- function(column) {
  tags <- .fetch_tags_table()
  
  if (!column %in% colnames(tags)) {
    stop(sprintf("'%s' is not a valid column in the tags table. Use names(irw_tags()) to see available options.", column))
  }
  
  unique_vals <- unique(tags[[column]])
  sort(unique_vals[!is.na(unique_vals)])
}

