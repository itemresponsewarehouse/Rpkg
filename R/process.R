#' Convert IRW Long-Format Data to Wide-Format Response Matrix
#'
#' Converts an IRW-compliant dataset from long format to wide format while handling
#' optional metadata elements, filtering by wave, and ensuring consistency.
#'
#' The function automatically drops the `date` column if present, as it is not required
#' for response matrix conversion.
#'
#' @param df A data frame containing IRW-compliant item response data in long format.
#' @param wave A numeric value specifying which wave to filter (default: `1`).
#'        If the dataset does not have a "wave" column, this input is ignored.
#' @param values_fn A function to resolve duplicate `id`-`item` responses (default: `mean`).
#' @param density_threshold A numeric value between `0.5` and `1.0` specifying the 
#'        minimum response density required for a respondent (`id`) to be included. Default is `NULL` (no filtering).
#' @return A data frame in wide format where rows represent `id` values and columns represent `item_*` responses.
#'
#' @importFrom stats aggregate reshape
#' @export
irw_long2resp <- function(df, wave = 1, values_fn = mean, density_threshold = NULL) {
  
  # Ensure required columns exist
  required_cols <- c("id", "item", "resp")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required IRW columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Drop 'date' column if present (silently, no message)
  if ("date" %in% names(df)) {
    df <- df[ , !(names(df) %in% "date")]
  }
  
  # Handle wave filtering if wave input is specified
  if (!is.null(wave)) {
    if ("wave" %in% names(df)) {
      if (!wave %in% unique(df$wave)) {
        message("Wave ", wave, " not found in data. No filtering applied.")
      } else {
        message("Filtering for wave ", wave, ".")
        df <- df[df$wave == wave, , drop = FALSE]
      }
    } else {
      message("Dataset does not contain a 'wave' column. Ignoring wave input (", wave, ").")
    }
  }
  
  # Ensure item names have "item_" prefix
  df$item <- ifelse(grepl("^item_", df$item), df$item, paste0("item_", df$item))
  
  # Convert response values to numeric
  df$resp <- suppressWarnings(as.numeric(df$resp))  # Convert invalid values to NA
  
  # Warn if non-numeric responses exist
  if (any(is.na(df$resp))) {
    message("Some responses could not be converted to numeric. These have been set to NA.")
  }
  
  # Compute and apply response density filter for respondents
  if (!is.null(density_threshold)) {
    density_threshold <- min(max(density_threshold, 0.5), 1)  # Ensure within [0.5,1]
    
    # Calculate response density per respondent (id)
    id_resp_counts <- aggregate(!is.na(df$resp) ~ id, data = df, FUN = sum)
    colnames(id_resp_counts)[2] <- "response_count"
    
    total_items <- length(unique(df$item))
    
    # Compute density per respondent
    id_resp_counts$density <- id_resp_counts$response_count / total_items
    
    # Filter respondents who meet the density threshold
    ids_to_keep <- id_resp_counts$id[id_resp_counts$density >= density_threshold]
    
    df <- df[df$id %in% ids_to_keep, , drop = FALSE]
  }
  
  # Resolve duplicate responses per `id`-`item`
  df_agg <- aggregate(resp ~ id + item, data = df, FUN = values_fn, na.rm = TRUE)
  
  # Convert to wide format using reshape
  wide_df <- reshape(df_agg, idvar = "id", timevar = "item", direction = "wide")
  
  # Rename columns to remove "resp." prefix while keeping "item_*"
  names(wide_df) <- gsub("^resp\\.", "", names(wide_df))
  
  # Ensure response values are numeric
  response_cols <- setdiff(names(wide_df), "id")
  wide_df[response_cols] <- lapply(wide_df[response_cols], as.numeric)
  
  return(wide_df)
}