# Compute Metadata for Redivis Database
# This function processes tables from the Redivis database, computes summary statistics for each table,
# and saves the results incrementally to a CSV file. Logs any salvaging attempts or failed tables.

# Set base directory for processed data
output_base_dir <- file.path("data-raw", "internal_data", "processed_data")

# Ensure the directory exists
if (!dir.exists(output_base_dir)) {
  dir.create(output_base_dir, recursive = TRUE)
}

# Main function
compute_metadata <- function(output_file = file.path(output_base_dir, "metadata.csv"),
                             testing = FALSE) {
  # Adjust output file for testing mode
  if (testing) {
    output_file <- file.path(output_base_dir, "testing.csv")
  }
  
  # Initialize the datasource and fetch the list of tables
  datasource <- .initialize_datasource()
  table_list <- datasource$list_tables()
  
  # Limit to a subset for testing
  if (testing) {
    table_list <- head(table_list, 5)
  }
  
  total_tables <- length(table_list)
  start_time <- Sys.time()  # Track the start time
  
  # Log failures or salvaging attempts incrementally to a separate CSV
  log_failure <- function(table_name, reason, status) {
    failed_file <- sub("\\.csv$", "_failed.csv", output_file)
    failed_entry <- data.frame(
      table_name = table_name,
      reason = reason,
      status = status,
      stringsAsFactors = FALSE
    )
    
    if (file.exists(failed_file)) {
      existing_failures <- read.csv(failed_file, stringsAsFactors = FALSE)
      write.csv(rbind(existing_failures, failed_entry), file = failed_file, row.names = FALSE)
    } else {
      write.csv(failed_entry, file = failed_file, row.names = FALSE)
    }
  }
  
  # Process a single table and compute metadata
  process_table <- function(table) {
    table_name <- table$properties$name
    message(sprintf("Processing table: %s", table_name))
    flush.console()
    
    # Skip processing if the table is already in the output file
    if (file.exists(output_file)) {
      existing_metadata <- read.csv(output_file, stringsAsFactors = FALSE)
      if (table_name %in% existing_metadata$table_name) {
        message(sprintf("Skipping already processed table: %s", table_name))
        return(NULL)
      }
    }
    
    # Fetch table data with error handling
    table_data <- tryCatch({
      table$get()$to_data_frame()
    }, error = function(e) {
      log_failure(table_name, paste("Data fetch error:", e$message), "Skipped")
      return(NULL)
    })
    if (is.null(table_data)) return(NULL)
    
    # Validate required columns
    required_columns <- c("id", "item", "resp")
    missing_columns <- setdiff(required_columns, names(table_data))
    if (length(missing_columns) > 0) {
      log_failure(table_name, paste("Missing columns:", paste(missing_columns, collapse = ", ")), "Skipped")
      return(NULL)
    }
    
    # Process `resp` column
    table_data$resp <- suppressWarnings(as.numeric(table_data$resp))
    if (any(is.na(table_data$resp))) {
      salvage_attempt <- sprintf("Coerced 'resp' to numeric. %d values became NA.", sum(is.na(table_data$resp)))
      log_failure(table_name, salvage_attempt, "Partially Processed")
      table_data <- table_data[!is.na(table_data$resp), ]
    }
    
    if (nrow(table_data) == 0) {
      log_failure(table_name, "All 'resp' values invalid after coercion.", "Skipped")
      return(NULL)
    }
    
    # Compute metadata
    id_count <- length(unique(table_data$id))
    item_count <- length(unique(table_data$item))
    resp_count <- nrow(table_data)
    resp_unique <- length(unique(table_data$resp))
    sparsity <- (sqrt(resp_count) / id_count) * (sqrt(resp_count) / item_count)
    resp_per_id <- mean(as.numeric(table(table_data$id)))
    resp_per_item <- mean(as.numeric(table(table_data$item)))
    
    response_data <- table_data[, c("item", "resp")]
    response_groups <- split(response_data$resp, response_data$item)
    normalized_responses <- lapply(response_groups, function(x) x / max(x, na.rm = TRUE))
    mean_normalized_resp <- mean(unlist(normalized_responses), na.rm = TRUE)
    
    has_rt <- "rt" %in% names(table_data)
    has_date <- "date" %in% names(table_data)
    has_rater <- "rater" %in% names(table_data)
    
    # Clean up memory
    rm(table_data, response_data, response_groups, normalized_responses)
    gc()
    
    # Return metadata
    list(
      table_name = table_name,
      id_count = id_count,
      item_count = item_count,
      resp_count = resp_count,
      resp_unique = resp_unique,
      sparsity = sparsity,
      resp_per_id = resp_per_id,
      resp_per_item = resp_per_item,
      mean_normalized_resp = mean_normalized_resp,
      has_rt = as.numeric(has_rt),
      has_date = as.numeric(has_date),
      has_rater = as.numeric(has_rater)
    )
  }
  
  # Process tables
  for (i in seq_along(table_list)) {
    table <- table_list[[i]]
    metadata <- tryCatch({
      process_table(table)
    }, error = function(e) {
      log_failure(table$properties$name, paste("Unexpected error:", e$message), "Skipped")
      return(NULL)
    })
    
    if (!is.null(metadata)) {
      metadata_df <- as.data.frame(metadata, stringsAsFactors = FALSE)
      if (file.exists(output_file)) {
        existing_data <- read.csv(output_file, stringsAsFactors = FALSE)
        write.csv(rbind(existing_data, metadata_df), file = output_file, row.names = FALSE)
      } else {
        write.csv(metadata_df, file = output_file, row.names = FALSE)
      }
    }
    
    elapsed_time <- difftime(Sys.time(), start_time, units = "secs")
    avg_time_per_table <- as.numeric(elapsed_time) / i
    remaining_time <- avg_time_per_table * (total_tables - i)
    message(sprintf(
      "Progress: %d/%d tables processed. Estimated time remaining: %.1f seconds.",
      i, total_tables, remaining_time
    ))
  }
  
  return(read.csv(output_file, stringsAsFactors = FALSE))
}

# Run the function
results <- compute_metadata()


