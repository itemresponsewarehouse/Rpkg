#' Retrieve Metadata for a Specific Table
#'
#' Fetches and displays detailed metadata for a specified table in the IRW database.
#' The metadata includes information such as the number of rows, size in bytes, timestamps,
#' access level, and related URLs.
#'
#' @param table_name A character string specifying the name of the table to retrieve metadata for.
#' @param verbose Logical; whether to print the metadata to the console. Defaults to `TRUE`.
#' @return A list containing the table metadata for programmatic use, including:
#'   \item{name}{The name of the table.}
#'   \item{created_at}{The creation timestamp of the table (formatted as a string).}
#'   \item{updated_at}{The last update timestamp of the table (formatted as a string).}
#'   \item{num_rows}{The number of rows in the table.}
#'   \item{data_size_kb}{The size of the table in kilobytes.}
#'   \item{variable_count}{The number of variables in the table.}
#'   \item{is_sample}{Logical; indicates whether the table is a sample dataset.}
#'   \item{doi}{The DOI of the table, if available.}
#'   \item{table_url}{The URL to the table.}
#'   \item{container_url}{The URL to the container of the table.}
#' @examples
#' \dontrun{
#'   metadata <- irw_table_metadata("abortion")
#'   print(metadata)
#' }
#' @export
irw_table_metadata <- function(table_name, verbose = TRUE) {
  # Fetch the table object
  table <- .fetch_redivis_table(table_name)
  
  # Retrieve metadata properties
  name <- table$properties$name
  created_at <- table$properties$createdAt / 1000  # Convert from milliseconds to seconds
  updated_at <- table$properties$updatedAt / 1000  # Convert from milliseconds to seconds
  num_rows <- table$properties$numRows
  data_size <- table$properties$numBytes / 1024  # Convert bytes to KB
  variable_count <- table$properties$variableCount
  table_url <- table$properties$url
  is_sample <- table$properties$isSample
  container_url <- table$properties$container$url
  doi <- table$properties$container$doi
  doi_url <- paste0("https://doi.org/", doi)
  
  # Format dates
  formatted_created_at <- format(as.POSIXct(created_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  formatted_updated_at <- format(as.POSIXct(updated_at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  
  # Optionally print metadata information
  if (verbose) {
    cat("Table Metadata for:", table_name, "\n")
    cat("--------------------------------------------------\n")
    cat("Name:                     ", name, "\n")
    cat("Created At:               ", formatted_created_at, "\n")
    cat("Last Updated At:          ", formatted_updated_at, "\n")
    cat("Number of Rows:           ", num_rows, "\n")
    cat("Data Size (KB):           ", round(data_size, 2), "KB\n")
    cat("Variable Count:           ", variable_count, "\n")
    cat("Is Sample:                ", ifelse(is_sample, "Yes", "No"), "\n")
    cat("DOI:                      ", doi_url, "\n")
    cat("Table URL:                ", table_url, "\n")
    cat("Container URL:            ", container_url, "\n")
    cat("--------------------------------------------------\n")
  }
  
  # Compile metadata into a list for programmatic use
  metadata <- list(
    name = name,
    created_at = formatted_created_at,
    updated_at = formatted_updated_at,
    num_rows = num_rows,
    data_size_kb = round(data_size, 2),
    variable_count = variable_count,
    is_sample = is_sample,
    doi = doi,
    table_url = table_url,
    container_url = container_url
  )
  
  # Return the metadata list
  return(metadata)
}

#' Display Overall Metadata Statistics
#'
#' Computes and displays overall statistics for datasets stored in the IRW database.
#' The statistics include the range (minimum, maximum) and mean values for key attributes,
#' as well as counts of tables with specific characteristics such as raters or response times.
#'
#' @details
#' This function uses an internal metadata structure and dynamically computes statistics
#' for key attributes, such as:
#' - `id_count`: Number of unique IDs per dataset.
#' - `item_count`: Number of unique items per dataset.
#' - `resp_count`: Total number of responses in each dataset.
#' - `density`: Density measure for each dataset (between 0 and 1).
#'
#' @return Prints the statistics to the console. Does not return a value.
#' @examples
#' \dontrun{
#'   irw_overall_stats()
#' }
#' @export
irw_overall_stats <- function() {
  
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
    density = compute_stats(metadata$density)
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
  
  cat(
    sprintf(
      "ID Count:\n  Min: %d, Max: %d, Mean: %.2f\n",
      stats$id_count$min,
      stats$id_count$max,
      stats$id_count$mean
    )
  )
  cat(
    sprintf(
      "Item Count:\n  Min: %d, Max: %d, Mean: %.2f\n",
      stats$item_count$min,
      stats$item_count$max,
      stats$item_count$mean
    )
  )
  cat(
    sprintf(
      "Response Count:\n  Min: %d, Max: %d, Mean: %.2f\n",
      stats$resp_count$min,
      stats$resp_count$max,
      stats$resp_count$mean
    )
  )
  cat(
    sprintf(
      "Density:\n  Min: %.3f, Max: %.3f, Mean: %.3f\n",
      stats$density$min,
      stats$density$max,
      stats$density$mean
    )
  )
}


#' Visualize Metadata Distributions
#'
#' Generates histograms for the distributions of key attributes in the IRW metadata.
#'
#' @param ranges A named list specifying the lower and upper range for each attribute to visualize.
#'        If not provided, the full range of data will be used for each attribute. Valid attributes include:
#'        - `"id_count"`: Number of unique IDs per dataset.
#'        - `"item_count"`: Number of unique items per dataset.
#'        - `"resp_count"`: Total number of responses per dataset.
#'        - `"density"`: Density measure (between 0 and 1).
#' @return No return value. Outputs histograms to the console.
#' @examples
#' # Example with default ranges:
#' irw_visualize()
#'
#' # Example with custom ranges:
#' irw_visualize(ranges = list(id_count = c(0, 1000), density = c(0.1, 0.9)))
#' @importFrom graphics axis hist mtext par
#' @export
irw_visualize <- function(ranges = list()) {
  
  # Attributes to visualize
  attributes <- c("id_count", "item_count", "resp_count", "density")
  
  # User-friendly labels for attributes
  attribute_labels <- list(
    id_count = "Number of Unique IDs",
    item_count = "Number of Unique Items",
    resp_count = "Total Number of Responses",
    density = "Density Measure"
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
    hist_obj <- hist(
      data,
      breaks = 30,
      col = "lightgray",
      border = "black",
      main = title,
      xlab = "",
      ylab = "",
      xaxt = "n",
      yaxt = "n"
    )
    
    # Subtitle with smaller font size
    mtext(subtitle,
          side = 3,
          line = 0.5,
          cex = 0.8)
    
    # Improved x-axis labels
    axis(
      1,
      at = pretty(hist_obj$breaks),
      labels = sapply(pretty(hist_obj$breaks), format_axis),
      las = 1
    )
    # Improved y-axis labels
    axis(
      2,
      at = pretty(hist_obj$counts),
      labels = sapply(pretty(hist_obj$counts), format_axis),
      las = 1
    )
  }
  
  # Reset plotting layout
  par(mfrow = c(1, 1))
}


#' List Available Datasets
#'
#' Retrieves a summary of available datasets in the IRW database, including their name,
#' number of rows, and variable count, sorted alphabetically by dataset name.
#'
#' @return A data frame with the following columns:
#'   \item{name}{The name of the dataset, sorted alphabetically.}
#'   \item{numRows}{The number of rows in the dataset.}
#'   \item{variableCount}{The number of variables in the dataset.}
#' @examples
#' \dontrun{
#'   datasets <- irw_list_datasets()
#'   print(datasets)
#' }
#' @export
irw_list_datasets <- function() {
  # Initialize the datasource if not already set
  ds <- .initialize_datasource()
  
  # Retrieve the list of datasets from the datasource
  datasets <- ds$list_tables()  # Assuming 'list_tables()' returns datasets as well
  
  # Extract metadata to create a data frame
  datasets_info <- data.frame(
    name = sapply(datasets, function(dataset) dataset$name),
    numRows = sapply(datasets, function(dataset) dataset$properties$numRows),
    variableCount = sapply(datasets, function(dataset) dataset$properties$variableCount),
    stringsAsFactors = FALSE
  )
  
  # Sort datasets_info by the name column in alphabetical order
  datasets_info <- datasets_info[order(datasets_info$name), ]
  
  return(datasets_info)
}



#' Retrieve Database Metadata
#'
#' Fetches and displays comprehensive metadata for the IRW database, including the database version,
#' number of tables, total data size, and relevant URLs.
#'
#' @return A list containing the database metadata, including:
#'   \item{version}{The version of the database.}
#'   \item{table_count}{The total number of tables in the database.}
#'   \item{created_at}{The creation timestamp of the database (formatted as a string).}
#'   \item{updated_at}{The last update timestamp of the database (formatted as a string).}
#'   \item{total_size_gb}{The total size of the database in gigabytes.}
#'   \item{active_data_size_gb}{The size of active tabular data in gigabytes.}
#'   \item{doi}{The DOI of the database.}
#'   \item{dataset_url}{The URL to the database.}
#'   \item{documentation}{The link to the database documentation.}
#'   \item{methodology}{The link to the database methodology.}
#'   \item{usage_info}{The link to usage information.}
#' @examples
#' \dontrun{
#'   db_metadata <- irw_db_metadata()
#'   print(db_metadata)
#' }
#' @export
irw_db_metadata <- function() {
  # Initialize the datasource if it hasn't been initialized
  ds <- .initialize_datasource()
  
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
