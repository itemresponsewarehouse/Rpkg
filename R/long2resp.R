#' Convert IRW Long-Format Data to Wide-Format Response Matrix
#'
#' Converts an IRW-compliant dataset from long format to wide format while handling
#' optional metadata elements, filtering by wave, and ensuring consistency.
#'
#' This function applies a default sparsity filter with a threshold of `0.1` for `id` density
#' to avoid excessive sparsity in the response matrix.
#' Users can disable filtering by setting `id_density_threshold = NULL`.
#'
#' Additionally, the function detects and resolves duplicate id-item responses.
#' By default, it keeps the **most common response (mode)**, but users can specify
#' an alternative method (`mean`, `median`, or `first` response).
#'
#' @param df A data frame containing IRW-compliant item response data in long format.
#' @param wave (Optional) A numeric value specifying which wave to filter.
#'        If the dataset does not have a "wave" column, this input is ignored.
#'        Defaults to the most frequent wave if `NULL`.
#' @param id_density_threshold A numeric value between `0.0` and `1.0` specifying the 
#'        minimum response density required for an `id` to be included. 
#'        Default is `0.1`. Set to `NULL` to disable filtering.
#' @param agg_method A string specifying how to handle multiple `id`-`item` pairs.
#'        Options: `"mean"` (default), `"mode"`, `"median"`, `"first"`.
#' @return A data frame in wide format where rows represent `id` values and columns represent `item_*` responses.
#'
#' @importFrom stats aggregate median na.omit reshape
#' @export
irw_long2resp <- function(df, wave = NULL, id_density_threshold = 0.1, agg_method = "mean") {
  
  # Ensure required columns exist
  required_cols <- c("id", "item", "resp")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required IRW columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Stop execution if 'date' or 'rater' columns exist
  if (any(c("date", "rater") %in% names(df))) {
    stop("This function does not yet support data with 'date' or 'rater' columns.")
  }
  
  # Drop non-essential columns except id, item, resp, and wave (if exists)
  essential_cols <- c("id", "item", "resp", "wave")
  df <- df[, intersect(names(df), essential_cols), drop = FALSE]
  
  # Store messages to print at the end
  messages <- c()
  
  # Handle wave filtering
  if ("wave" %in% names(df)) {
    if (is.null(wave)) {
      # Find the most frequent wave
      wave_counts <- table(df$wave)
      wave <- as.numeric(names(wave_counts)[which.max(wave_counts)])
      messages <- c(messages, paste0("Defaulting to the most frequent wave: ", wave))
    }
    
    if (wave %in% df$wave) {
      df <- df[df$wave == wave, , drop = FALSE]
      messages <- c(messages, paste0("Filtering applied: Keeping only responses from wave ", wave))
    } else {
      messages <- c(messages, paste0("Wave ", wave, " not found in data. No filtering applied."))
    }
  }
  
  # Ensure item names have "item_" prefix
  df$item <- ifelse(grepl("^item_", df$item), df$item, paste0("item_", df$item))
  
  # Convert response values to numeric
  df$resp <- suppressWarnings(as.numeric(df$resp))
  
  # Warn if non-numeric responses exist
  if (any(is.na(df$resp))) {
    messages <- c(messages, "Some responses could not be converted to numeric. These have been set to NA.")
  }
  
  # Compute total id count before filtering
  total_ids <- length(unique(df$id))
  
  filtering_occurred <- FALSE  # Track if filtering was applied
  
  # Apply filtering for sparse ids
  if (!is.null(id_density_threshold)) {
    # Compute response density per id
    id_resp_counts <- aggregate(!is.na(df$resp) ~ id, data = df, FUN = sum)
    colnames(id_resp_counts)[2] <- "response_count"
    
    total_items <- length(unique(df$item))
    
    # Compute density per id
    id_resp_counts$density <- id_resp_counts$response_count / total_items
    
    # Filter ids based on density threshold
    ids_to_keep <- id_resp_counts$id[id_resp_counts$density >= id_density_threshold]
    filtered_ids <- setdiff(unique(df$id), ids_to_keep)
    
    if (length(filtered_ids) > 0) {
      percent_removed <- round((length(filtered_ids) / total_ids) * 100, 2)
      messages <- c(messages, paste0(length(filtered_ids), " ids removed (", 
                                     length(filtered_ids), " out of ", total_ids, ", ",
                                     percent_removed, "%) due to response density below threshold (", 
                                     id_density_threshold, ")."))
      filtering_occurred <- TRUE
    }
    
    df <- df[df$id %in% ids_to_keep, , drop = FALSE]
  }
  
  # Provide message on how to disable filtering if it occurred
  if (filtering_occurred) {
    messages <- c(messages, "To disable filtering, set `id_density_threshold = NULL`.")
  }
  
  # Count duplicate id-item responses properly
  dup_summary <- aggregate(resp ~ id + item, data = df, length)
  total_unique_pairs <- nrow(dup_summary)
  
  # Find only pairs with duplicates (more than 1 response)
  affected_pairs <- dup_summary[dup_summary$resp > 1, ]
  num_affected_pairs <- nrow(affected_pairs)
  num_duplicate_responses <- sum(affected_pairs$resp) - num_affected_pairs  # Extra responses
  avg_duplicates_per_pair <- ifelse(num_affected_pairs > 0, 
                                    round(num_duplicate_responses / num_affected_pairs, 2), 
                                    0)
  prop_dup_pairs <- round((num_affected_pairs / total_unique_pairs) * 100, 2)
  
  if (num_affected_pairs > 0) {
    messages <- c(messages, paste0(
      "Found ", num_duplicate_responses, " responses across ", 
      num_affected_pairs, " unique id-item pairs (", prop_dup_pairs, "% of total). ",
      "Average responses per pair: ", avg_duplicates_per_pair, ". ",
      "Aggregating responses based on '", agg_method, "' method."
    ))
  }
  
  # Deduplication function based on user input
  if (agg_method == "mode") {
    mode_fn <- function(x) {
      x <- na.omit(x)
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]  # Mode function
    }
    df <- aggregate(resp ~ id + item, data = df, FUN = mode_fn)
  } else if (agg_method == "mean") {
    df <- aggregate(resp ~ id + item, data = df, FUN = mean, na.rm = TRUE)
  } else if (agg_method == "median") {
    df <- aggregate(resp ~ id + item, data = df, FUN = median, na.rm = TRUE)
  } else if (agg_method == "first") {
    df <- df[!duplicated(df[, c("id", "item")]), ]
  } else {
    stop("Invalid `agg_method`. Choose from 'mode', 'mean', 'median', or 'first'.")
  }
  
  # Convert to wide format
  wide_df <- reshape(df, idvar = "id", timevar = "item", direction = "wide")
  
  # Remove "resp." prefix from column names
  colnames(wide_df) <- sub("^resp\\.", "", colnames(wide_df))
  
  # Print messages at the end
  if (length(messages) > 0) {
    message(paste(messages, collapse = "\n"))
  }
  
  return(wide_df)
}