# Compute Metadata for Redivis Database
# This function processes tables from the Redivis database, computes summary statistics for each table,
# and saves the results incrementally to a CSV file. Logs any salvaging attempts or failed tables.

compute_metadata <- function(output_file = "data/metadata.csv",
                             testing = FALSE) {
  # Initialize the datasource and fetch the list of tables
  datasource <- initialize_datasource()
  table_list <- datasource$list_tables()
  
  # Limit to a subset for testing
  if (testing) {
    table_list <- head(table_list, 5)
    output_file <- "data/testing.csv"  # Use a separate output file for testing
  }
  total_tables <- length(table_list)
  
  # Track the start time for progress estimation
  start_time <- Sys.time()
  
  # Log failures or salvaging attempts incrementally to a separate CSV
  log_failure <- function(table_name, reason, status) {
    failed_file <- sub("\\.csv$", "_failed.csv", output_file)
    failed_entry <- data.frame(
      table_name = table_name,
      reason = reason,
      status = status,  # Indicate whether partially processed or skipped
      stringsAsFactors = FALSE
    )
    
    if (file.exists(failed_file)) {
      # Append to existing failures
      existing_failures <- read.csv(failed_file, stringsAsFactors = FALSE)
      write.csv(rbind(existing_failures, failed_entry), file = failed_file, row.names = FALSE)
    } else {
      # Create a new failures file
      write.csv(failed_entry, file = failed_file, row.names = FALSE)
    }
  }
  
  # Process a single table and compute metadata
  process_table <- function(table) {
    table_name <- table$properties$name
    message(sprintf("Processing table: %s", table_name))
    flush.console()  # Ensure immediate display of messages
    
    # Skip processing if the table is already in the output file
    if (file.exists(output_file)) {
      existing_metadata <- read.csv(output_file, stringsAsFactors = FALSE)
      if (table_name %in% existing_metadata$table_name) {
        message(sprintf("Skipping already processed table: %s", table_name))
        return(NULL)
      }
    }
    
    # Attempt to fetch table data
    table_data <- tryCatch({
      table$get()$to_data_frame()
    }, error = function(e) {
      log_failure(table_name, paste("Data fetch error:", e$message), "Skipped")
      return(NULL)
    })
    if (is.null(table_data)) return(NULL)
    
    # Ensure the table contains required columns
    required_columns <- c("id", "item", "resp")
    missing_columns <- setdiff(required_columns, names(table_data))
    if (length(missing_columns) > 0) {
      log_failure(table_name, paste("Missing columns:", paste(missing_columns, collapse = ", ")), "Skipped")
      return(NULL)
    }
    
    # Coerce `resp` to numeric, logging salvaging actions
    table_data$resp <- suppressWarnings(as.numeric(table_data$resp))
    if (any(is.na(table_data$resp))) {
      salvage_attempt <- sprintf("Coerced 'resp' to numeric. %d values became NA.", sum(is.na(table_data$resp)))
      log_failure(table_name, salvage_attempt, "Partially Processed")
      # Remove rows with invalid `resp` values
      table_data <- table_data[!is.na(table_data$resp), ]
    }
    
    # Skip tables with no valid `resp` values left
    if (nrow(table_data) == 0) {
      log_failure(table_name, "All 'resp' values invalid after coercion.", "Skipped")
      return(NULL)
    }
    
    # Calculate summary statistics
    id_count <- length(unique(table_data$id))  # Number of unique IDs
    item_count <- length(unique(table_data$item))  # Number of unique items
    resp_count <- nrow(table_data)  # Total number of responses
    resp_unique <- length(unique(table_data$resp))  # Unique response values
    sparsity <- (sqrt(resp_count) / id_count) * (sqrt(resp_count) / item_count)  # Sparsity measure
    resp_per_id <- mean(as.numeric(table(table_data$id)))  # Avg responses per ID
    resp_per_item <- mean(as.numeric(table(table_data$item)))  # Avg responses per item
    
    # Calculate mean normalized response
    response_data <- table_data[, c("item", "resp")]
    response_groups <- split(response_data$resp, response_data$item)
    normalized_responses <- lapply(response_groups, function(x) x / max(x, na.rm = TRUE))
    mean_normalized_resp <- mean(unlist(normalized_responses), na.rm = TRUE)
    
    # Check for optional columns
    has_rt <- "rt" %in% names(table_data)
    has_date <- "date" %in% names(table_data)
    has_rater <- "rater" %in% names(table_data)
    
    # Free memory
    rm(table_data, response_data, response_groups, normalized_responses)
    gc()
    
    # Return the computed metadata as a named list
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
  
  # Process each table sequentially
  for (i in seq_along(table_list)) {
    table <- table_list[[i]]
    
    # Attempt to process the table and handle unexpected errors gracefully
    metadata <- tryCatch({
      process_table(table)
    }, error = function(e) {
      log_failure(table$properties$name, paste("Unexpected error:", e$message), "Skipped")
      return(NULL)
    })
    
    # Write metadata to the output CSV incrementally
    if (!is.null(metadata)) {
      metadata_df <- as.data.frame(metadata, stringsAsFactors = FALSE)
      if (file.exists(output_file)) {
        existing_data <- read.csv(output_file, stringsAsFactors = FALSE)
        write.csv(rbind(existing_data, metadata_df), file = output_file, row.names = FALSE)
      } else {
        write.csv(metadata_df, file = output_file, row.names = FALSE)
      }
    }
    
    # Update progress
    elapsed_time <- difftime(Sys.time(), start_time, units = "secs")
    avg_time_per_table <- as.numeric(elapsed_time) / i
    remaining_time <- avg_time_per_table * (total_tables - i)
    message(sprintf(
      "Progress: %d/%d tables processed. Estimated time remaining: %.1f seconds.",
      i, total_tables, remaining_time
    ))
  }
  
  # Return the metadata directly from the output file
  return(read.csv(output_file, stringsAsFactors = FALSE))
}

# Run the function
results <- compute_metadata(testing = FALSE)
