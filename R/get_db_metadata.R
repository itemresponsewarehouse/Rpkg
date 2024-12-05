#' Show Overall Statistics for Metadata
#'
#' Computes and displays overall statistics for the datasets stored in the internal metadata file.
#'
#' @details
#' The function dynamically loads the `metadata.csv` file from the package's `data/` folder
#' and computes:
#' - Range (min, max) and mean values for `id_count`, `item_count`, `resp_count`, and `sparsity`.
#' - Number of tables with `rater`, `response time (rt)`, and `date` information.
#'
#' The function prints the statistics to the console without returning any value.
#'
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#'   show_overall_statistics()
#' }
#' 
#' @export
show_overall_statistics <- function() {
  metadata_file <- "data/metadata.csv"
  
  # Check if the file exists
  if (!file.exists(metadata_file)) {
    stop("No metadata.csv file found in the data/ directory.")
  }
  
  # Load the metadata file
  metadata <- utils::read.csv(metadata_file, stringsAsFactors = FALSE)
  
  # Helper function to compute range and mean
  compute_stats <- function(x) {
    list(
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE)
    )
  }
  
  # Compute stats for each key numeric column
  stats <- list(
    id_count = compute_stats(metadata$id_count),
    item_count = compute_stats(metadata$item_count),
    resp_count = compute_stats(metadata$resp_count),
    sparsity = compute_stats(metadata$sparsity)
  )
  
  # Additional counts
  num_tables <- nrow(metadata)
  num_rater <- sum(metadata$has_rater, na.rm = TRUE)
  num_rt <- sum(metadata$has_rt, na.rm = TRUE)
  num_date <- sum(metadata$has_date, na.rm = TRUE)
  
  # Print Summary to Console
  cat("Summary of Metadata\n")
  cat("--------------------\n")
  cat(sprintf("Number of Tables: %d\n", num_tables))
  cat(sprintf("Tables with Raters (rater): %d\n", num_rater))
  cat(sprintf("Tables with Response Time (rt): %d\n", num_rt))
  cat(sprintf("Tables with Date (date): %d\n", num_date))
  cat("\n")
  
  cat(sprintf("ID Count:\n  Min: %d, Max: %d, Mean: %.2f\n",
              stats$id_count$min, stats$id_count$max, stats$id_count$mean))
  cat(sprintf("Item Count:\n  Min: %d, Max: %d, Mean: %.2f\n",
              stats$item_count$min, stats$item_count$max, stats$item_count$mean))
  cat(sprintf("Response Count:\n  Min: %d, Max: %d, Mean: %.2f\n",
              stats$resp_count$min, stats$resp_count$max, stats$resp_count$mean))
  cat(sprintf("Sparsity:\n  Min: %.3f, Max: %.3f, Mean: %.3f\n",
              stats$sparsity$min, stats$sparsity$max, stats$sparsity$mean))
}


#' Visualize Metadata Distributions
#'
#' Visualizes the distributions of key numeric attributes in the metadata file as raw count histograms.
#'
#' @param ranges A named list where each attribute has a numeric vector of two values specifying 
#'        the lower and upper range for the histogram (e.g., `list(id_count = c(0, 10000))`).
#'        Attributes not included in `ranges` will use their full data range.
#'
#'        The attributes that can be adjusted are:
#'        - `"id_count"`: Number of unique IDs per dataset (e.g., `ranges = list(id_count = c(0, 50000))`).
#'        - `"item_count"`: Number of unique items per dataset (e.g., `ranges = list(item_count = c(0, 50))`).
#'        - `"resp_count"`: Total number of responses (e.g., `ranges = list(resp_count = c(0, 100000))`).
#'        - `"sparsity"`: Sparsity measure, ranging between 0 and 1 (e.g., `ranges = list(sparsity = c(0.1, 0.9))`).
#'
#'        Users can specify ranges for one or more attributes, and attributes not included in `ranges` 
#'        will automatically use their full range.
#'
#' @importFrom utils read.csv
#' @importFrom graphics par hist mtext axis
#' @examples
#' \dontrun{
#' # Example 1: Visualize with default ranges for all attributes
#' visualize_metadata_distributions()
#'
#' # Example 2: Visualize with custom ranges for some attributes
#' visualize_metadata_distributions(
#'   ranges = list(id_count = c(0, 1000), resp_count = c(0, 100000))
#' )
#'
#' # Example 3: Visualize with custom ranges for all attributes
#' visualize_metadata_distributions(
#'   ranges = list(
#'     id_count = c(0, 1000),
#'     item_count = c(0, 300),
#'     resp_count = c(0, 100000),
#'     sparsity = c(0, 3)
#'   )
#' )
#' }
#'
#' @export
visualize_metadata_distributions <- function(ranges = list()) {
  metadata_file <- "data/metadata.csv"
  
  # Check if the file exists
  if (!file.exists(metadata_file)) {
    stop("No metadata.csv file found in the data/ directory.")
  }
  
  # Load the metadata file
  metadata <- utils::read.csv(metadata_file, stringsAsFactors = FALSE)
  
  # Attributes to visualize
  attributes <- c("id_count", "item_count", "resp_count", "sparsity")
  
  # User-friendly labels for attributes
  attribute_labels <- list(
    id_count = "Number of Unique IDs",
    item_count = "Number of Unique Items",
    resp_count = "Total Number of Responses",
    sparsity = "Sparsity Measure"
  )
  
  # Helper function to limit values to the user-defined range
  limit_range <- function(x, range) {
    if (!is.null(range)) {
      x[x >= range[1] & x <= range[2]]
    } else {
      x
    }
  }
  
  # Helper function to format axis labels
  format_axis <- function(x) {
    if (x >= 1e6) {
      return(sprintf("%.1fM", x / 1e6))
    } else if (x >= 1e3) {
      return(sprintf("%.1fk", x / 1e3))
    } else {
      return(as.character(x))
    }
  }
  
  # Set up plotting area
  par(mfrow = c(2, 2), mar = c(4, 5, 4, 1))  # Adjust layout for 4 attributes
  
  # Plot each attribute
  for (attribute in attributes) {
    if (!attribute %in% names(metadata)) {
      warning(sprintf("Attribute '%s' not found in metadata.", attribute))
      next
    }
    
    # Limit data to user-defined range or use full range
    range <- ranges[[attribute]]
    data <- limit_range(metadata[[attribute]], range)
    
    # Get user-friendly labels
    title <- attribute_labels[[attribute]] %||% attribute
    subtitle <- attribute
    
    # Raw histogram
    hist_obj <- hist(data, breaks = 30, col = "lightgray", border = "black", 
                     main = title, 
                     xlab = "",  
                     ylab = "", 
                     xaxt = "n", yaxt = "n")
    
    # Subtitle with smaller font size
    mtext(subtitle, side = 3, line = 0.5, cex = 0.8)
    
    # Improved x-axis labels
    axis(1, at = pretty(hist_obj$breaks), labels = sapply(pretty(hist_obj$breaks), format_axis), las = 1)
    # Improved y-axis labels
    axis(2, at = pretty(hist_obj$counts), labels = sapply(pretty(hist_obj$counts), format_axis), las = 1)
  }
  
  # Reset plotting layout
  par(mfrow = c(1, 1))
}


#' List Available Datasets
#'
#' Retrieves a summary of available datasets in the datasource, including the dataset name,
#' number of rows, and variable count for each dataset.
#'
#' @return A data frame where each row corresponds to a dataset, with columns for the
#'         dataset name (`name`), number of rows (`numRows`), and variable count (`variableCount`).
#' @examples
#' \dontrun{
#'   datasets_summary <- list_available_datasets()
#'   print(datasets_summary)
#' }
#' @export
list_available_datasets <- function() {
  # Initialize the datasource if not already set
  ds <- initialize_datasource()
  
  # Retrieve the list of datasets from the datasource
  datasets <- ds$list_tables()  # Assuming 'list_tables()' returns datasets as well
  
  # Extract metadata to create a data frame
  datasets_info <- data.frame(
    name = sapply(datasets, function(dataset) dataset$name),
    numRows = sapply(datasets, function(dataset) dataset$properties$numRows),
    variableCount = sapply(datasets, function(dataset) dataset$properties$variableCount),
    stringsAsFactors = FALSE
  )
  
  return(datasets_info)
}



#' Get Database Metadata
#'
#' Retrieves and prints comprehensive metadata about the IRW database, including version, number of tables,
#' data size, access information, and relevant URLs.
#'
#' @return A list containing the database metadata for programmatic use.
#' @export
get_database_metadata <- function() {
  # Initialize the datasource if it hasn't been initialized
  ds <- initialize_datasource()
  
  # Retrieve metadata properties
  version <- ds$properties$version$tag
  table_count <- ds$properties$tableCount
  created_at <- ds$properties$createdAt / 1000  # Convert from milliseconds to seconds
  updated_at <- ds$properties$updatedAt / 1000  # Convert from milliseconds to seconds
  total_size <- ds$properties$totalNumBytes / (1024 ^ 3)  # Convert bytes to GB
  active_size <- ds$properties$totalActiveTabularBytes / (1024 ^ 3)  # Convert bytes to GB
  doi <- ds$properties$doi
  doi_url <- paste0("https://doi.org/", doi)
  dataset_url <- ds$properties$url
  documentation_link <- ds$properties$links[[1]]$url
  methodology_link <- ds$properties$methodologyMarkdown
  usage_link <- ds$properties$usageMarkdown
  
  # Format dates
  formatted_created_at <- format(as.POSIXct(created_at, origin = "1970-01-01", tz = "UTC"),
                                 "%Y-%m-%d %H:%M:%S")
  formatted_updated_at <- format(as.POSIXct(updated_at, origin = "1970-01-01", tz = "UTC"),
                                 "%Y-%m-%d %H:%M:%S")
  
  # Print metadata information
  cat("IRW Database Metadata:\n")
  cat("--------------------------------------------------\n")
  cat("Version:                  ", version, "\n")
  cat("Table Count:              ", table_count, "\n")
  cat("Created At:               ", formatted_created_at, "\n")
  cat("Last Updated At:          ", formatted_updated_at, "\n")
  cat("Total Data Size (GB):     ", round(total_size, 2), "GB\n")
  cat("Active Data Size (GB):    ", round(active_size, 2), "GB\n")
  cat("DOI:                      ", doi_url, "\n")
  cat("Dataset URL:              ", dataset_url, "\n")
  cat("Documentation:            ", documentation_link, "\n")
  cat("Methodology:              ", methodology_link)
  cat("Usage Information:        ", usage_link, "\n")
  cat("--------------------------------------------------\n")
  
  # Compile metadata into a list for programmatic use
  metadata <- list(
    version = version,
    table_count = table_count,
    created_at = formatted_created_at,
    updated_at = formatted_updated_at,
    total_size_gb = round(total_size, 2),
    active_data_size_gb = round(active_size, 2),
    doi = doi,
    dataset_url = dataset_url,
    documentation = documentation_link,
    methodology = methodology_link,
    usage_info = usage_link
  )
  
  # Return the metadata list
  return(metadata)
}
