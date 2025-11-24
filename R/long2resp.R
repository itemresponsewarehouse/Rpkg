.irw_check_resp <- function(df, min_count = 5L, min_prop = 0.01) {
  single_category_items <- character(0)
  sparse_category_items <- list()
  
  # Only use non-missing responses for checks
  df_nonmiss <- df[!is.na(df$resp), , drop = FALSE]
  
  if (nrow(df_nonmiss) == 0L) {
    return(list(
      single_category_items = single_category_items,
      sparse_category_items = sparse_category_items
    ))
  }
  
  # Group responses by item
  resp_by_item <- split(df_nonmiss$resp, df_nonmiss$item)
  
  # 1. Items with only one observed response category
  unique_counts <- vapply(
    resp_by_item,
    function(x) length(unique(x)),
    integer(1L)
  )
  single_category_items <- names(unique_counts[unique_counts <= 1L])
  
  # 2. Polytomous items with very sparse categories
  #    Rule: category is flagged if it has < min_count responses OR < min_prop of responses.
  for (item_name in names(resp_by_item)) {
    x <- resp_by_item[[item_name]]
    # Only care about polytomous items (3+ categories)
    if (length(unique(x)) < 3L) next
    
    tab  <- table(x)
    prop <- prop.table(tab)
    
    rare <- (tab < min_count) | (prop < min_prop)
    if (any(rare)) {
      sparse_category_items[[item_name]] <- data.frame(
        resp  = as.numeric(names(tab)[rare]),
        count = as.integer(tab[rare]),
        prop  = as.numeric(prop[rare]),
        row.names = NULL
      )
    }
  }
  
  list(
    single_category_items = single_category_items,
    sparse_category_items = sparse_category_items
  )
}

#' Check response patterns in IRW data
#'
#' @description
#' Summarize basic response diagnostics in IRW data, including items with a
#' single observed response category and sparse categories in polytomous items.
#'
#' @details
#' When \code{x} is long-format (with columns \code{id}, \code{item}, \code{resp}),
#' diagnostics are recomputed using \code{min_count} and \code{min_prop}. When
#' \code{x} is the wide-format result of \code{irw_long2resp()}, any attached
#' diagnostics from \code{check_resp = TRUE} are returned directly.
#'
#' A response category is treated as "sparse" if it has fewer than
#' \code{min_count} observed responses OR a within-item proportion smaller than
#' \code{min_prop}.
#'
#' @param x A data frame in long format with columns \code{id}, \code{item},
#'   \code{resp}, or the result of \code{irw_long2resp()}.
#' @param min_count Minimum count for a category before it is considered
#'   sparse (only used when \code{x} is long-format). Default is 5.
#' @param min_prop Minimum within-item proportion for a category before it
#'   is considered sparse (only used when \code{x} is long-format).
#'   Default is 0.01.
#'
#' @return A list with two elements:
#'   \code{single_category_items} (character vector of item IDs) and
#'   \code{sparse_category_items} (named list of data frames with columns
#'   \code{resp}, \code{count}, \code{prop}).
#'
#' @export
irw_check_resp <- function(x, min_count = 5L, min_prop = 0.01) {
  # Case 1: x is long-format (id, item, resp)
  if (all(c("id", "item", "resp") %in% names(x))) {
    df <- x
    df$item <- ifelse(grepl("^item_", df$item), df$item, paste0("item_", df$item))
    df$resp <- suppressWarnings(as.numeric(df$resp))
    return(.irw_check_resp(df, min_count = min_count, min_prop = min_prop))
  }
  
  # Case 2: x is wide (output of irw_long2resp)
  diag_attr <- attr(x, "resp_checks", exact = TRUE)
  if (!is.null(diag_attr)) return(diag_attr)
  
  stop(
    "`irw_check_resp()` expects either:\n",
    "  - long-format data with id/item/resp, or\n",
    "  - an object from irw_long2resp(check_resp = TRUE)."
  )
}

#' Convert IRW Long-Format Data to Wide-Format Response Matrix
#'
#' Converts an IRW-compliant dataset from long format to wide format while handling
#' optional metadata elements, filtering by wave, and ensuring consistency.
#'
#' This function applies a default sparsity filter with a threshold of \code{0.1} for \code{id} density
#' to avoid excessive sparsity in the response matrix.
#' Users can disable filtering by setting \code{id_density_threshold = NULL}.
#'
#' If \code{check_resp = TRUE}, the function will run a basic response diagnostic using
#' default thresholds (\code{min_count = 5}, \code{min_prop = 0.01}), attach the results as
#' an attribute \code{"resp_checks"} on the returned object, and add a short NOTE with
#' counts of flagged items. A response category is considered sparse if it has fewer than
#' \code{min_count} responses OR a within-item proportion smaller than \code{min_prop}.
#' For more control, use \code{irw_check_resp()} directly.
#'
#' @param df A data frame containing IRW-compliant item response data in long format.
#' @param wave (Optional) A numeric value specifying which wave to filter.
#'        If the dataset does not have a \code{wave} column, this input is ignored.
#'        Defaults to the most frequent wave if \code{NULL}.
#' @param id_density_threshold A numeric value between \code{0.0} and \code{1.0} specifying the
#'        minimum response density required for an \code{id} to be included.
#'        Default is \code{0.1}. Set to \code{NULL} to disable filtering.
#' @param agg_method A string specifying how to handle multiple \code{id}-\code{item} pairs.
#'        Options: \code{"mean"} (default), \code{"mode"}, \code{"median"}, \code{"first"}.
#' @param check_resp Logical; if \code{TRUE}, perform basic response diagnostics on items
#'        using default thresholds and attach results as an attribute. Default is \code{FALSE}.
#'
#' @return A data frame in wide format where rows represent \code{id} values and columns
#'         represent \code{item_*} responses.
#'         If \code{check_resp = TRUE}, an attribute \code{"resp_checks"} is attached:
#'         \itemize{
#'           \item \code{single_category_items}: character vector of item IDs with only one observed category.
#'           \item \code{sparse_category_items}: named list; each element is a data.frame
#'                 with sparse categories for that item (columns: \code{resp}, \code{count}, \code{prop}).
#'         }
#'
#' @importFrom stats aggregate median na.omit reshape
#' @export
irw_long2resp <- function(df,
                          wave = NULL,
                          id_density_threshold = 0.1,
                          agg_method = "mean",
                          check_resp = FALSE) {
  
  # Ensure required columns exist
  required_cols <- c("id", "item", "resp")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0L) {
    stop("Missing required IRW columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Stop execution if 'date' exists
  if ("date" %in% names(df)) {
    stop("This function does not yet support data with 'date'.")
  }
  
  # Store messages to print at the end
  messages <- character(0)
  
  # Check for "rater" column and count unique raters
  if ("rater" %in% names(df)) {
    num_raters <- length(unique(df$rater))
    messages <- c(
      messages,
      paste0("NOTE: This dataset contains 'rater' information with ",
             num_raters, " unique raters.\n")
    )
  }
  
  # Drop non-essential columns except id, item, resp, and wave (if exists)
  essential_cols <- c("id", "item", "resp", "wave")
  df <- df[, intersect(names(df), essential_cols), drop = FALSE]
  
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
    messages <- c(
      messages,
      "Some responses were either missing or could not be converted to numeric; these have been recorded as NA."
    )
  }
  
  # ---- Duplicate handling ----
  # Count duplicate id-item responses
  dup_summary <- stats::aggregate(
    x  = seq_len(nrow(df)),                # just row indices
    by = list(id = df$id, item = df$item),
    FUN = length
  )
  names(dup_summary)[3] <- "n"
  
  total_unique_pairs <- nrow(dup_summary)
  affected_pairs <- dup_summary[dup_summary$n > 1L, , drop = FALSE]
  num_affected_pairs <- nrow(affected_pairs)
  
  num_duplicate_responses <- if (num_affected_pairs > 0L) {
    sum(affected_pairs$n) - num_affected_pairs
  } else 0L
  
  avg_duplicates_per_pair <- if (num_affected_pairs > 0L) {
    round(num_duplicate_responses / num_affected_pairs, 2)
  } else 0
  
  prop_dup_pairs <- if (total_unique_pairs > 0L) {
    round((num_affected_pairs / total_unique_pairs) * 100, 2)
  } else 0
  
  if (num_affected_pairs > 0L) {
    messages <- c(
      messages,
      paste0(
        "Found ", num_duplicate_responses, " responses across ",
        num_affected_pairs, " unique id-item pairs (", prop_dup_pairs, "% of total). ",
        "\nAverage responses per pair: ", avg_duplicates_per_pair, ". ",
        "\nAggregating responses based on agg_method='", agg_method, "'."
      )
    )
    
    # Aggregation based on user input (only if duplicates exist)
    if (agg_method == "mode") {
      mode_fn <- function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0L) return(NA_real_)
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }
      agg <- stats::aggregate(
        x  = df$resp,
        by = list(id = df$id, item = df$item),
        FUN = mode_fn
      )
      
    } else if (agg_method == "mean") {
      mean_fn <- function(x) {
        if (all(is.na(x))) return(NA_real_)
        mean(x, na.rm = TRUE)
      }
      agg <- stats::aggregate(
        x  = df$resp,
        by = list(id = df$id, item = df$item),
        FUN = mean_fn
      )
      
    } else if (agg_method == "median") {
      median_fn <- function(x) {
        if (all(is.na(x))) return(NA_real_)
        stats::median(x, na.rm = TRUE)
      }
      agg <- stats::aggregate(
        x  = df$resp,
        by = list(id = df$id, item = df$item),
        FUN = median_fn
      )
      
    } else if (agg_method == "first") {
      df <- df[!duplicated(df[, c("id", "item")]), , drop = FALSE]
      agg <- df[, c("id", "item", "resp")]
    } else {
      stop("Invalid `agg_method`. Choose from 'mode', 'mean', 'median', or 'first'.")
    }
    
    if (agg_method %in% c("mode", "mean", "median")) {
      names(agg) <- c("id", "item", "resp")
      df <- agg
    }
  }
  # If no duplicates, df stays as-is
  
  # ---- Optional response checks (on the deduped/numeric df) ----
  resp_checks <- NULL
  if (check_resp) {
    resp_checks <- .irw_check_resp(df, min_count = 5L, min_prop = 0.01)
    
    total_items_chk <- length(unique(df$item))
    n_single <- length(resp_checks$single_category_items)
    n_sparse <- length(resp_checks$sparse_category_items)
    
    if (n_single + n_sparse > 0L) {
      messages <- c(
        messages,
        paste0(
          "NOTE (check_resp): potential response issues detected.\n",
          "  - Single-category items: ", n_single, " out of ", total_items_chk, "\n",
          "  - Items with sparse categories: ", n_sparse, " out of ", total_items_chk, "\n",
          "For a summary or custom thresholds, call irw_check_resp() on your data."
        )
      )
    } else {
      messages <- c(
        messages,
        paste0(
          "NOTE (check_resp): no response issues detected with default thresholds.\n",
          "You can still run irw_check_resp() for a diagnostic list."
        )
      )
    }
  }
  
  # ---- Density filtering ----
  ids_all <- sort(unique(df$id))
  total_ids <- length(ids_all)
  filtering_occurred <- FALSE
  
  if (!is.null(id_density_threshold)) {
    id_resp_counts <- stats::aggregate(
      x  = !is.na(df$resp),
      by = list(id = df$id),
      FUN = sum
    )
    names(id_resp_counts) <- c("id", "response_count")
    
    total_items <- length(unique(df$item))
    id_resp_counts$density <- id_resp_counts$response_count / total_items
    
    ids_to_keep <- id_resp_counts$id[id_resp_counts$density >= id_density_threshold]
    filtered_ids <- setdiff(ids_all, ids_to_keep)
    
    if (length(filtered_ids) > 0L) {
      percent_removed <- round((length(filtered_ids) / total_ids) * 100, 2)
      messages <- c(
        messages,
        paste0(
          length(filtered_ids), " ids removed (",
          length(filtered_ids), " out of ", total_ids, ", ",
          percent_removed, "%) due to response density below threshold (",
          id_density_threshold, ")."
        )
      )
      filtering_occurred <- TRUE
    }
    
    df <- df[df$id %in% ids_to_keep, , drop = FALSE]
  }
  
  if (filtering_occurred) {
    messages <- c(messages, "To disable filtering, set `id_density_threshold = NULL`.")
  }
  
  # ---- Convert to wide format (force id/item/resp shape) ----
  df_wide <- df[, c("id", "item", "resp"), drop = FALSE]
  
  wide_df <- stats::reshape(
    df_wide,
    idvar   = "id",
    timevar = "item",
    v.names = "resp",
    direction = "wide"
  )
  
  # Remove "resp." prefix from column names
  colnames(wide_df) <- sub("^resp\\.", "", colnames(wide_df))
  
  # Attach response check info, if any
  if (check_resp) {
    attr(wide_df, "resp_checks") <- resp_checks
  }
  
  # Print messages at the end
  if (length(messages) > 0L) {
    message(paste(messages, collapse = "\n"))
  }
  
  wide_df
}
