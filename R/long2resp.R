.irw_check_resp <- function(df, min_count = 5L, min_prop = 0.01, resp_col = "resp") {
  if (!resp_col %in% names(df)) {
    stop("Column specified by `resp_col` not found in `df`: ", resp_col)
  }

  single_category_items <- character(0)
  sparse_category_items <- list()
  
  # Only use non-missing responses for checks
  df_nonmiss <- df[!is.na(df[[resp_col]]), , drop = FALSE]
  
  if (nrow(df_nonmiss) == 0L) {
    return(list(
      single_category_items = single_category_items,
      sparse_category_items = sparse_category_items
    ))
  }
  
  # Group responses by item
  resp_by_item <- split(df_nonmiss[[resp_col]], df_nonmiss$item)
  
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
    
    tab <- table(x)
    prop <- prop.table(tab)
    
    rare <- (tab < min_count) | (prop < min_prop)
    if (any(rare)) {
      resp_vals <- names(tab)[rare]
      if (all(!is.na(suppressWarnings(as.numeric(resp_vals))))) {
        resp_vals <- as.numeric(resp_vals)
      }
      sparse_category_items[[item_name]] <- data.frame(
        resp = resp_vals,
        count = as.integer(tab[rare]),
        prop = as.numeric(prop[rare]),
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
#' @param resp_col Character string giving the response column to inspect when
#'   \code{x} is long-format. Defaults to \code{"resp"}.
#'
#' @return A list with two elements:
#'   \code{single_category_items} (character vector of item IDs) and
#'   \code{sparse_category_items} (named list of data frames with columns
#'   \code{resp}, \code{count}, \code{prop}).
#'
#' @export
irw_check_resp <- function(x, min_count = 5L, min_prop = 0.01, resp_col = "resp") {
  # Case 1: x is long-format (id, item, <resp_col>)
  if (all(c("id", "item") %in% names(x))) {
    if (!resp_col %in% names(x)) {
      stop("Column specified by `resp_col` not found in `x`: ", resp_col)
    }
    df <- x
    df$item <- ifelse(grepl("^item_", df$item), df$item, paste0("item_", df$item))
    return(.irw_check_resp(
      df,
      min_count = min_count,
      min_prop = min_prop,
      resp_col = resp_col
    ))
  }
  
  # Case 2: x is wide (output of irw_long2resp)
  diag_attr <- attr(x, "resp_checks", exact = TRUE)
  if (!is.null(diag_attr)) return(diag_attr)
  
  stop(
    "`irw_check_resp()` expects either:\n",
    "  - long-format data with id/item plus the column named in `resp_col`, or\n",
    "  - an object from irw_long2resp(check_resp = TRUE)."
  )
}

#' Convert IRW Long-Format Data to Wide-Format Response Matrix
#'
#' Converts an IRW-compliant dataset from long format to wide format
#' while handling optional metadata elements and filtering by wave.
#'
#' This function applies a default sparsity filter with a threshold
#' of \code{0.1} for \code{id} density to avoid excessive sparsity.
#' Users can disable filtering by setting \code{id_density_threshold = NULL}.
#'
#' If \code{check_resp = TRUE}, the function will run a basic diagnostic
#' using default thresholds (\code{min_count = 5}, \code{min_prop = 0.01}),
#' attach the results as an attribute \code{"resp_checks"} on returned object,
#' and add a short NOTE with counts of flagged items.
#' A response category is considered sparse if it has fewer than
#' \code{min_count} responses OR a within-item proportion smaller than
#' \code{min_prop}. For more control, use \code{irw_check_resp()} directly.
#'
#' @param df A data frame containing IRW-compliant item response data in long
#' format.
#' @param wave (Optional) A numeric value specifying which wave to filter.
#'        If the dataset does not have a \code{wave} column, this input is
#'        ignored.
#'        Defaults to the most frequent wave if \code{NULL}.
#' @param id_density_threshold A numeric value between \code{0.0} and
#' \code{1.0} specifying the minimum response density required for an \code{id}
#'  to be included. Default is \code{0.1}. Set to \code{NULL} to disable
#'   filtering. Density is the proportion of items with a non-missing value
#'   in the column given by \code{resp_col}.
#' @param agg_method A string specifying how to handle multiple
#'  \code{id}-\code{item} pairs. Options: \code{"mean"}, \code{"mode"},
#'  \code{"median"}, \code{"first"}. Default is \code{"mean"} for numeric
#'  response columns and \code{"first"} for non-numeric (e.g. text).
#' @param check_resp Logical; if \code{TRUE}, perform basic response diagnostics
#'    on items using default thresholds and attach results as an attribute.
#'    Default is \code{FALSE}.
#' @param resp_col Character string giving the column in \code{df} to use as
#'   the response value when building the wide matrix. Defaults to \code{"resp"}.
#'   For nominal data stored in a text column, use e.g. \code{resp_col = "text"}.
#'
#' @return A data frame in wide format where rows represent \code{id} values
#' and columns represent \code{item_*} responses. The column used for
#' response values is recorded in the \code{"resp_col"} attribute.
#'         If \code{check_resp = TRUE}, an attribute \code{"resp_checks"}
#'          is attached:
#'         \itemize{
#'           \item \code{single_category_items}: character vector of item IDs
#'            with only one observed category.
#'           \item \code{sparse_category_items}: named list; each element is
#'            a data.frame with sparse categories for that item
#'            (columns: \code{resp}, \code{count}, \code{prop}).
#'         }
#'
#' @importFrom stats aggregate median na.omit reshape
#' @export
irw_long2resp <- function(df,
                          wave = NULL,
                          id_density_threshold = 0.1,
                          agg_method = NULL,
                          check_resp = FALSE,
                          resp_col = "resp") {

  required_cols <- c("id", "item")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0L) {
    stop("Missing required IRW columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!resp_col %in% names(df)) {
    stop("Column specified by `resp_col` not found in `df`: ", resp_col)
  }
  if ("date" %in% names(df)) {
    stop("This function does not yet support data with 'date'.")
  }

  messages <- character(0)
  resp_raw <- df[[resp_col]]
  is_numeric_resp <- is.numeric(resp_raw) || is.integer(resp_raw)

  agg_method_defaulted <- is.null(agg_method)
  if (is.null(agg_method)) {
    agg_method <- if (is_numeric_resp) "mean" else "first"
  }
  valid_agg <- c("mean", "mode", "median", "first")
  if (!agg_method %in% valid_agg) {
    stop("Invalid `agg_method`. Choose from 'mean', 'mode', 'median', or 'first'.")
  }
  if (!is_numeric_resp && agg_method %in% c("mean", "median")) {
    stop(
      "agg_method = '", agg_method, "' requires a numeric response column, ",
      "but `resp_col = \"", resp_col, "\"` is non-numeric. Use 'first' or 'mode' instead."
    )
  }

  if ("rater" %in% names(df)) {
    num_raters <- length(unique(df$rater))
    messages <- c(
      messages,
      paste0("NOTE: This dataset contains 'rater' information with ",
             num_raters, " unique raters.\n")
    )
  }

  essential_cols <- c("id", "item", "wave")
  df <- df[, intersect(names(df), essential_cols), drop = FALSE]
  df$resp <- resp_raw

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

  if (agg_method %in% c("mean", "median")) {
    original_resp <- df$resp
    df$resp <- suppressWarnings(as.numeric(df$resp))
    if (any(is.na(df$resp) & !is.na(original_resp))) {
      messages <- c(
        messages,
        paste0(
          "Some responses could not be converted to numeric for agg_method='",
          agg_method, "' and were recorded as NA."
        )
      )
    }
  }

  # ---- Duplicate handling ----
  # Count duplicate id-item responses
  dup_summary <- stats::aggregate(
    x = seq_len(nrow(df)),                # just row indices
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

  if (num_affected_pairs > 0L) {
    default_note <- if (agg_method_defaulted) {
      paste0(" (agg_method defaulted to '", agg_method, "' for ", if (is_numeric_resp) "numeric" else "non-numeric", " response)")
    } else {
      ""
    }
    messages <- c(
      messages,
      paste0(
        "Dropped ", num_duplicate_responses, " duplicate response(s) from ", num_affected_pairs,
        " id-item pair(s) (out of ", total_unique_pairs, " total pairs). ",
        "Aggregating using agg_method='", agg_method, "'", default_note, "."
      )
    )
    
    # Aggregation based on user input (only if duplicates exist)
    if (agg_method == "mode") {
      mode_fn <- function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0L) return(NA)
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }
      agg <- stats::aggregate(
        x = df$resp,
        by = list(id = df$id, item = df$item),
        FUN = mode_fn
      )
      
    } else if (agg_method == "mean") {
      mean_fn <- function(x) {
        if (all(is.na(x))) return(NA_real_)
        mean(x, na.rm = TRUE)
      }
      agg <- stats::aggregate(
        x = df$resp,
        by = list(id = df$id, item = df$item),
        FUN = mean_fn
      )
      
    } else if (agg_method == "median") {
      median_fn <- function(x) {
        if (all(is.na(x))) return(NA_real_)
        stats::median(x, na.rm = TRUE)
      }
      agg <- stats::aggregate(
        x = df$resp,
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
      x = !is.na(df$resp),
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
    as.data.frame(df_wide),
    idvar = "id",
    timevar = "item",
    v.names = "resp",
    direction = "wide"
  )
  
  # Remove "resp." prefix from column names
  colnames(wide_df) <- sub("^resp\\.", "", colnames(wide_df))

  attr(wide_df, "resp_col") <- resp_col
  if (check_resp) {
    attr(wide_df, "resp_checks") <- resp_checks
  }
  
  # Print messages at the end
  if (length(messages) > 0L) {
    message(paste(messages, collapse = "\n"))
  }
  
  wide_df
}

#' Convert Wide-Format Response Matrix to IRW Long Format
#'
#' Inverse of \code{irw_long2resp()}: converts a wide response data frame/matrix
#' into long format with columns \code{id}, \code{item}, \code{resp}.
#'
#' @param x A wide response object (data.frame or matrix). If produced by
#'   \code{irw_long2resp()}, it will typically include an \code{id} column.
#' @param id Logical. If TRUE (default), expects an \code{id} column in \code{x}.
#'   If FALSE, creates \code{id = 1:nrow(x)} and treats all columns as item columns.
#'
#' @return A data.frame with columns \code{id}, \code{item}, \code{resp}.
#' @export
irw_resp2long <- function(x, id = TRUE) {
  if (!(is.data.frame(x) || is.matrix(x))) {
    stop("`x` must be a data.frame or matrix.")
  }
  
  x_df <- as.data.frame(x, stringsAsFactors = FALSE)
  has_id_col <- "id" %in% names(x_df)
  
  if (isTRUE(id)) {
    if (!has_id_col) {
      stop(
        "No `id` column found in `x`.\n",
        "This function defaults to `id = TRUE` (expects the output of `irw_long2resp()`).\n",
        "If your wide matrix does not include an id column, call `irw_resp2long(x, id = FALSE)`."
      )
    }
    id_vec <- x_df$id
    item_cols <- setdiff(names(x_df), "id")
  } else {
    if (has_id_col) {
      message("`id = FALSE` ignored because `id` column already exists; using existing ids.")
      id_vec <- x_df$id
      item_cols <- setdiff(names(x_df), "id")
    } else {
      id_vec <- seq_len(nrow(x_df))
      item_cols <- names(x_df)
    }
  }
  
  if (length(item_cols) == 0L) {
    stop("No item columns found to convert.")
  }
  
  out <- do.call(
    rbind,
    lapply(item_cols, function(it) {
      data.frame(
        id   = id_vec,
        item = it,
        resp = x_df[[it]],
        stringsAsFactors = FALSE
      )
    })
  )
  
  rownames(out) <- NULL
  out
}

