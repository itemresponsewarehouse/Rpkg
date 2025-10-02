#' Fetch a Single IRW Table
#'
#' Internal helper that fetches a single dataset from IRW, simulation, or competition datasets,
#' converts it to a tibble, and applies response recoding and optional deduplication.
#'
#' @param table_id Character. Name of the table.
#' @param sim Logical. If TRUE, fetch from the IRW simulation dataset.
#' @param dedup Logical. If TRUE, apply deduplication logic to responses.
#' @param comp Logical. If TRUE, fetch from the IRW competition dataset.
#'
#' @return A tibble representing the dataset.
#' @keywords internal
fetch_single_data <- function(table_id, sim = FALSE, dedup = FALSE, comp = FALSE) {
  tryCatch(
    {
      table_obj <- suppressMessages(.fetch_redivis_table(table_id, sim = sim, comp = comp))
      df <- .retry_with_backoff(function() table_obj$to_tibble())
      
      # Recode 'resp' from character to numeric if needed
      if ("resp" %in% names(df) && is.character(df$resp)) {
        suppressWarnings({
          new_resp <- as.numeric(df$resp)
        })
        non_numeric_values <- is.na(new_resp) & !(tolower(trimws(df$resp)) %in% c("na", "", NA))
        if (any(non_numeric_values)) {
          warning(sprintf(
            "In dataset '%s': 'resp' column contained non-numeric values that could not be coerced. Some NAs were introduced.",
            table_id
          ))
        }
        df$resp <- new_resp
      }
      
      # Deduplication logic
      if (dedup) {
        n_before <- nrow(df)
        
        if ("date" %in% names(df)) {
          message(sprintf(
            "Deduplication skipped for dataset '%s': 'date' column detected (timestamped responses).", table_id
          ))
        } else {
          if ("wave" %in% names(df)) {
            grouping_keys <- list(df$id, df$item, df$wave)
            success_msg <- sprintf(
              "Deduplicated dataset '%s': one response randomly retained per (id, item, wave) group.", table_id
            )
          } else {
            grouping_keys <- list(df$id, df$item)
            success_msg <- sprintf(
              "Deduplicated dataset '%s': one response randomly retained per (id, item) pair.", table_id
            )
          }
          
          split_df <- split(df, grouping_keys, drop = TRUE)
          df <- do.call(rbind, lapply(split_df, function(g) g[sample(nrow(g), 1), , drop = FALSE]))
          rownames(df) <- NULL
          
          n_after <- nrow(df)
          if (n_after < n_before) {
            message(success_msg)
          } else {
            message(sprintf(
              "Deduplication not needed for dataset '%s': no duplicate responses found.", table_id
            ))
          }
        }
      }
      
      return(df)
    },
    error = function(e) {
      message(paste("Error fetching dataset", shQuote(table_id), ":", e$message))
      return(invisible(NULL))
    }
  )
}

#' Fetch Table(s) from the Item Response Warehouse
#'
#' Retrieves one or more tables from IRW and returns them as tibbles.
#' If the table includes a character `resp` column, the function attempts to
#' coerce it into numeric. Strings like `"NA"`, `""`, and `NA` are treated as missing values.
#' A warning is issued only if other non-numeric values are encountered.
#'
#' @param name Character vector of one or more table names (IRW table IDs).
#' @param sim Logical, optional. If TRUE, fetches from the IRW simulation dataset (`irw_simsyn`). Defaults to FALSE.
#' @param dedup Logical, optional. If TRUE, deduplicates responses based on timing variables. Defaults to FALSE.
#'   - If a 'date' column is present, no deduplication is performed.
#'   - If only a 'wave' column is present, one random response is retained per (id, item) group within each wave.
#'   - If neither 'date' nor 'wave' is present, one random response is retained per (id, item) pair.
#' @param comp Logical, optional. If TRUE, fetches from the IRW competition dataset (`irw_competitions`). Defaults to FALSE.
#' @param resp Logical, optional. If TRUE, returns response matrix via `irw_long2resp()`. Defaults to FALSE.
#'
#' @return If a single name is provided, returns a tibble. If multiple, returns a named list
#'         of tibbles (or error messages, if retrieval failed).
#'
#' @examples
#' \dontrun{
#' # Main IRW data
#' irw_fetch("environment_ltm")
#'
#' # Deduplicated
#' irw_fetch("pks_probability", dedup = TRUE)
#'
#' # Simulation data
#' irw_fetch("gilbert_meta_3", sim = TRUE)
#'
#' # Competition data
#' irw_fetch("collegefb_2021and2022", comp = TRUE)
#' }
#'
#' @export
irw_fetch <- function(name, sim = FALSE, dedup = FALSE, comp = FALSE, resp = FALSE) {
  if (missing(name)) {
    stop(
      "Please provide the IRW table name(s) to fetch.\n",
      "Tip: Use `irw_list_tables()` to see available tables.",
      call. = FALSE
    )
  }
  
  if (sim && comp) {
    stop("Cannot set both 'sim = TRUE' and 'comp = TRUE'. Please choose one source.")
  }
  
  process_one <- function(nm) {
    dat <- fetch_single_data(nm, sim = sim, dedup = dedup, comp = comp)
    if (resp) {
      dat <- tryCatch(
        irw_long2resp(dat),
        error = function(e) {
          warning(sprintf("Could not convert '%s' to response matrix: %s", nm, e$message))
          return(dat)
        }
      )
    }
    return(dat)
  }
  
  if (length(name) == 1 && is.character(name)) {
    return(process_one(name))
  } else {
    dataset_list <- lapply(name, process_one)
    names(dataset_list) <- name
    return(dataset_list)
  }
}

#' Retrieve IRW Metadata Table
#'
#' Fetches the metadata table from Redivis and returns it as a tibble.
#' Link to Redivis table: https://redivis.com/datasets/bdxt-4fqe5tyf4/tables/h5gs-04agty3j1
#' Automatically checks for updates and refreshes only when needed.
#'
#' @return A tibble containing metadata information.
#' @export
irw_metadata <- function() {
  return(.fetch_metadata_table())
}

#' Retrieve IRW Tags Table
#'
#' Fetches the tags table from Redivis and returns it as a tibble.
#' Link to Redivis table: https://redivis.com/datasets/bdxt-4fqe5tyf4/tables/7nkh-6m7wq01yv

#' Automatically checks for updates and refreshes only when needed.
#'
#' @return A tibble containing tags information.
#' @export
irw_tags <- function() {
  return(.fetch_tags_table())
}



