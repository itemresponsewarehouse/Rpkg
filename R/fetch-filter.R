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
#'   # Fetch a single dataset
#'   df <- irw_fetch("example_dataset")
#'   print(df)
#'
#'   # Fetch multiple datasets
#'   datasets <- irw_fetch(c("dataset1", "dataset2"))
#'   print(names(datasets))  # Outputs: "dataset1" and "dataset2"
#' }
#' @export
irw_fetch <- function(name) {
  # Helper function to fetch a single dataset
  fetch_single_data <- function(table) {
    tryCatch({
      table <- suppressMessages(.fetch_redivis_table(table))
      table$to_tibble()
    }, error = function(e) {
      error_message <- paste("Error fetching dataset",
                             shQuote(table),
                             ":",
                             e$message)
      message(error_message)  # print error immediately
      return(error_message)  # save error message in list output
    })
  }
  
  # Check if fetching a single or multiple datasets
  if (length(name) == 1) {
    return(fetch_single_data(name))  # Return a single tibble or error message
  } else {
    dataset_list <- lapply(name, fetch_single_data)
    names(dataset_list) <- name
    return(dataset_list)  # Return a named list of tibbles/errors
  }
}


#' Retrieve IRW Metadata Table
#'
#' Fetches the metadata table from Redivis and returns it as a tibble.
#' Automatically checks for updates and refreshes only when needed.
#'
#' @return A tibble containing metadata information.
#' @export
irw_metadata <- function() {
  return(.fetch_metadata_table())
}


#' Filter Available Datasets in IRW
#'
#' Identifies datasets in the Item Response Warehouse (IRW) based on user-defined criteria.
#' This function filters datasets using **precomputed metadata**, which contains summary statistics
#' for each dataset (e.g., number of responses, number of participants, density scores, etc.).
#'
#' ## Exploring Metadata
#' To understand available dataset properties before filtering, run `summary(irw_metadata())`.
#' This provides an overview of key characteristics such as response counts, participant numbers, and density.
#' Users can then apply `irw_filter()` to select datasets matching their criteria.
#'
#' @param n_responses A numeric vector of length 2 specifying the range for total responses.
#' @param n_categories A numeric vector of length 2 specifying the range for unique response categories.
#' @param n_participants A numeric vector of length 2 specifying the range for the number of participants.
#' @param n_items A numeric vector of length 2 specifying the range for the number of items.
#' @param responses_per_participant A numeric vector of length 2 specifying the range for average responses per participant.
#' @param responses_per_item A numeric vector of length 2 specifying the range for average responses per item.
#' @param density A numeric vector of length 2 specifying the range for data density.
#'                Defaults to `c(0.5, 1)`. To disable this filter, set `density = NULL`.
#' @param var A character vector specifying one or more variables.
#'            - If **exact variable names** are provided, only datasets containing **all specified variables** will be returned.
#'            - If a variable name **contains an underscore** (e.g., `"cov_"`, `"Qmatrix_"`), the function will match all datasets that 
#'              contain **at least one variable** that starts with that prefix.
#' @return A sorted character vector of dataset names matching **all specified criteria** or an empty result if no matches are found.
#'
#' @examples
#' \dontrun{
#'   # Example 1: Filter datasets with at least 1,000 responses and contain "rt"
#'   irw_filter(n_responses = c(1000, Inf), var = "rt")
#'
#'   # Example 2: Disable density filtering and return datasets with "wave"
#'   irw_filter(var = "wave", density = NULL)
#'
#'   # Example 3: Find datasets with at least 500 participants and response density 0.3-0.8
#'   irw_filter(n_participants = c(500, Inf), density = c(0.3, 0.8))
#'
#'   # Example 4: Retrieve datasets that contain **all** of "treat", "rt", and any "cov_*" variables
#'   irw_filter(var = c("treat", "rt", "cov_"))
#'
#'   # Example 5: Retrieve datasets that contain any variable starting with "Qmatrix_"
#'   irw_filter(var = c("Qmatrix_"))
#' }
#' @export
irw_filter <- function(n_responses = NULL,
                       n_categories = NULL,
                       n_participants = NULL,
                       n_items = NULL,
                       responses_per_participant = NULL,
                       responses_per_item = NULL,
                       density = c(0.5, 1),
                       var = NULL) {
  
  metadata <- irw_metadata()  # Load latest metadata
  
  # Store initial dataset count
  initial_count <- nrow(metadata)
  
  # Convert "variables" column from "|"-separated string to list
  metadata$variables_list <- strsplit(metadata$variables, "\\| ")
  
  # Apply variable filtering
  if (!is.null(var)) {
    metadata <- metadata[sapply(metadata$variables_list, function(vars) {
      all(sapply(var, function(v) {
        if (grepl("_", v)) {
          # If variable contains "_", treat it as a prefix (match any variable that starts with it)
          any(grepl(paste0("^", v), vars))
        } else {
          # Exact match
          v %in% vars
        }
      }))
    }), ]
    
    # If no datasets remain after filtering, return empty result
    if (nrow(metadata) == 0) return(character(0))
  }
  
  # Store dataset count before applying density filtering
  count_before_density <- nrow(metadata)
  
  # Numeric filters
  filters <- list(
    n_responses = n_responses,
    n_categories = n_categories,
    n_participants = n_participants,
    n_items = n_items,
    responses_per_participant = responses_per_participant,
    responses_per_item = responses_per_item,
    density = density
  )
  
  # Remove NULL filters (unused parameters)
  filters <- filters[!sapply(filters, is.null)]
  
  # Check if the user explicitly set density
  user_specified_density <- !missing(density)
  
  # Apply numeric range filters dynamically
  for (filter_name in names(filters)) {
    filter_value <- filters[[filter_name]]
    
    if (is.numeric(filter_value) && length(filter_value) == 2) {
      metadata <- metadata[metadata[[filter_name]] >= filter_value[1] &
                             metadata[[filter_name]] <= filter_value[2], ]
    } else {
      metadata <- metadata[metadata[[filter_name]] %in% filter_value, ]
    }
  }
  
  # Check if the default density filter was applied **and** actually removed some datasets
  num_removed_by_density <- count_before_density - nrow(metadata)
  if (!user_specified_density &&
      identical(density, c(0.5, 1)) && num_removed_by_density > 0) {
    message(
      sprintf(
        "Note: The default density filter (0.5 to 1) was applied and removed %d dataset(s). To disable it, set density = NULL.",
        num_removed_by_density
      )
    )
  }
  
  # Sort final dataset names in alphabetical order before returning
  return(sort(metadata$table))
}
