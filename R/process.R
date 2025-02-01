#' Convert IRW Long-Format Data to Wide-Format Response Matrix
#'
#' Converts an IRW-compliant dataset from long format to wide format while handling
#' optional metadata elements, filtering by wave, and ensuring consistency.
#'
#' @param df A data frame containing IRW-compliant item response data in long format.
#' @param wave_col (Optional) A character string specifying the column name for wave number.
#'        If `NULL`, no filtering by wave is performed.
#' @param wave A numeric value specifying which wave to filter (default: `1`).
#'        Only applicable if `wave_col` is provided.
#' @param values_fn A function to resolve duplicate `id`-`item` responses (default: `mean`).
#' @return A data frame in wide format where rows represent `id` values and columns represent `item` responses.
#'
#' @importFrom stats aggregate reshape
#' @export
irw_long2resp <- function(df, wave_col = NULL, wave = 1, values_fn = mean) {
  
  # Ensure required columns exist
  required_cols <- c("id", "item", "resp")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required IRW columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Drop 'date' column if present
  if ("date" %in% names(df)) {
    message("Dropping 'date' column to maintain IRW standards.")
    df <- df[ , !(names(df) %in% "date")]
  }
  
  # If wave_col is provided, filter to selected wave
  if (!is.null(wave_col)) {
    if (!wave_col %in% names(df)) stop("Wave column not found in data.")
    df <- df[df[[wave_col]] == wave, , drop = FALSE]
  }
  
  # Ensure item names are character strings and properly prefixed
  df$item <- paste0("item_", as.character(df$item))
  
  # Convert resp to numeric (force conversion)
  df$resp <- suppressWarnings(as.numeric(df$resp))  # Converts invalid entries to NA
  
  # Warn if non-numeric responses exist
  if (any(is.na(df$resp))) {
    warning("Some responses could not be converted to numeric. These will be set to NA.")
  }
  
  # Resolve duplicate responses per `id`-`item` combination
  df_agg <- aggregate(resp ~ id + item, data = df, FUN = values_fn, na.rm = TRUE)
  
  # Convert to wide format using base R reshape
  wide_df <- reshape(df_agg, idvar = "id", timevar = "item", direction = "wide")
  
  # Rename columns to remove "resp." prefix
  names(wide_df) <- sub("resp\\.", "", names(wide_df))
  
  # Ensure response values are numeric
  response_cols <- setdiff(names(wide_df), "id")
  wide_df[response_cols] <- lapply(wide_df[response_cols], as.numeric)
  
  return(wide_df)
}