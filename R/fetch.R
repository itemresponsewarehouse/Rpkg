#' Fetch a Single IRW Table
#'
#' Internal helper that fetches a single dataset from IRW, simulation, or competition datasets,
#' converts it to a tibble, and applies response recoding and optional deduplication.
#'
#' @param table_id Character. Name of the table.
#' @param source Character. One of \code{"core"}, \code{"nom"}, \code{"sim"}, \code{"comp"}.
#' @param dedup Logical. If TRUE, apply deduplication logic to responses.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#'
#' @return A tibble representing the dataset.
#' @keywords internal
fetch_single_data <- function(table_id, source = "core", dedup = FALSE, sim = FALSE, comp = FALSE, nom = FALSE) {
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)
  tryCatch(
    {
      table_obj <- suppressMessages(.fetch_redivis_table(table_id, source = source))
      df <- .retry_with_backoff(function() table_obj$to_tibble())
      
      # Recode 'resp' from character to numeric if needed (skip for nominal source)
      if (source != "nom" && "resp" %in% names(df) && is.character(df$resp)) {
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
#' @param source Character. Data source: \code{"core"} (default), \code{"nom"}, \code{"sim"}, or \code{"comp"}.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param dedup Logical, optional. If TRUE, deduplicates responses based on timing variables. Defaults to FALSE.
#'   If a \code{date} column is present, no deduplication is performed.
#'   If only a \code{wave} column is present, one random response is retained per (id, item) group within each wave.
#'   If neither \code{date} nor \code{wave} is present, one random response is retained per (id, item) pair.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#' @param resp Logical, optional. If TRUE, returns response matrix via \code{irw_long2resp()}. Defaults to FALSE.
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
#' irw_fetch("gilbert_meta_3", source = "sim")
#'
#' # Competition data
#' irw_fetch("collegefb_2021and2022", source = "comp")
#' }
#'
#' @export
irw_fetch <- function(name, source = "core", dedup = FALSE, sim = FALSE, comp = FALSE, nom = FALSE, resp = FALSE) {
  if (missing(name)) {
    stop(
      "Please provide the IRW table name(s) to fetch.\n",
      "Tip: Use `irw_list_tables()` to see available tables.",
      call. = FALSE
    )
  }

  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)

  process_one <- function(nm) {
    dat <- fetch_single_data(nm, source = source, dedup = dedup)
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
#' Automatically checks for updates and refreshes only when needed.
#'
#' @param source Character. Data source: \code{"core"} (default), \code{"nom"}, \code{"sim"}, or \code{"comp"}.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#'
#' @return A tibble containing metadata information.
#' @export
irw_metadata <- function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)

  if (source == "comp") return(.fetch_comps_metadata_table())
  if (source == "sim")  return(.fetch_simsyn_metadata_table())
  if (source == "nom")  return(.fetch_nominal_metadata_table())
  .fetch_metadata_table()
}



#' Retrieve IRW Tags Table
#'
#' Fetches the tags table from Redivis and returns it as a tibble.
#' Link to Redivis table: https://redivis.com/datasets/bdxt-4fqe5tyf4/tables/7nkh-6m7wq01yv

#' Automatically checks for updates and refreshes only when needed.
#'
#' @param tables Optional. A character vector of table name(s) to filter by.
#'
#' @return A tibble containing tags information.
#' @export
irw_tags <- function(tables = NULL) {
  tags <- .fetch_tags_table()
  
  if (is.null(tables)) {
    return(tags)
  }
  
  if (!is.character(tables)) {
    stop("`tables` must be a character vector.")
  }
  
  out <- tags[tags$table %in% tables, ]
  
  invalid <- setdiff(tables, out$table)
  
  if (length(invalid) > 0) {
    warning(
      sprintf(
        "Unknown table name(s): %s",
        paste(invalid, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  out
}



