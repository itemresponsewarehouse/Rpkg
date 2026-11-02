#' Fetch a Single IRW Table
#'
#' Internal helper that fetches a single dataset from IRW, simulation, or competition datasets,
#' converts it to a tibble, and applies response recoding and optional deduplication.
#'
#' @param table_id Character. Name of the table.
#' @param source Character. The data family, one of \code{"core"}, \code{"nom"}, \code{"sim"}, \code{"comp"}.
#' @param dedup Logical. If TRUE, apply deduplication logic to responses.
#' @param max_rows Integer or NULL. Stop after this many rows, pushed to Redivis.
#' @param columns Character vector or NULL. Fetch only these columns, pushed to Redivis.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#'
#' @return A tibble representing the dataset.
#' @keywords internal
fetch_single_data <- function(table_id, source = "core", dedup = FALSE,
                              max_rows = NULL, columns = NULL,
                              sim = FALSE, comp = FALSE, nom = FALSE) {
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)
  ds_list <- .irw_shard_first(
    .irw_order_datasources(.initialize_datasource(source = source), source = source),
    name = table_id, source = source, build = FALSE
  )
  errors <- .irw_new_error_collector()

  i <- 0L
  while (i < length(ds_list)) {
    i <- i + 1L
    if (i == 2L) {
      ds_list <- .irw_shard_first_after_miss(ds_list, name = table_id, source = source)
    }
    ds <- ds_list[[i]]
    df <- tryCatch(
      {
        withCallingHandlers({
          ds$get()
          tbl <- ds$table(table_id)
          tbl$get()
          # Not retried: a retry would export the table again against the
          # 30-day export cap. See .retry_with_backoff(). Served from the disk
          # cache when this exact table (by content hash) was fetched before.
          # Dedup needs every column -- it groups on id, item and wave -- so
          # `columns` is not pushed down when it is set, and irw_fetch() drops
          # the extra columns afterwards. Same rule as the Python client.
          .irw_cached_download(tbl, "tables", max_rows = max_rows,
                               columns = if (dedup) NULL else columns)
        },
        warning = function(w) {
          # Both halves of it: the dataset read above warns too, and until
          # 2026-10-25 only the table half was muffled here.
          if (grepl(.irw_redivis_advice_pattern, conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        })
      },
      error = function(e) {
        # A bad column name is the caller's mistake, not a shard miss: trying
        # the next warehouse would end in "table does not exist in IRW" for a
        # table that exists.
        if (inherits(e, "irw_bad_columns")) stop(e)
        .irw_handle_datasource_error(conditionMessage(e), table_name = table_id, ds_list = ds_list, errors = errors)
        NULL
      }
    )

    if (is.null(df)) next

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
    if (dedup && source == "conj") {
      message(sprintf("Deduplication skipped for dataset '%s': conjoint tables have one row per id, task and profile.", table_id))
    } else if (dedup && source == "rank") {
      message(sprintf("Deduplication skipped for dataset '%s': a rankings table keeps duplicate marks as recorded; see irw_rank_compress().", table_id))
    } else if (dedup) {
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
  }

  # Only report the table as missing when every datasource actually said
  # "not found"; anything else (quota, timeout, transport) is re-raised so it is
  # not mistaken for a table that was removed from IRW.
  failure <- .irw_collected_error_message(errors)
  if (!is.null(failure)) {
    message("Error fetching dataset ", shQuote(table_id), ":", failure)
    return(invisible(NULL))
  }

  pinned <- .irw_pinned_not_found_message(table_id, source)
  if (!is.null(pinned)) {
    message("Error fetching dataset ", shQuote(table_id), ":", pinned)
    return(invisible(NULL))
  }

  # Which of the two reasons applies is worth the lookup: a table the user has
  # just seen in a listing and cannot fetch needs the explanation, not a bare
  # not-found (ben-domingue/irw#3127).
  if (.irw_table_listed_in_metadata(table_id, source = source)) {
    message(
      "Error fetching dataset ",
      shQuote(table_id),
      ":",
      .irw_metadata_only_table_message(table_id)
    )
    return(invisible(NULL))
  }

  message(
    "Error fetching dataset ",
    shQuote(table_id),
    ": table does not exist in IRW."
  )
  invisible(NULL)
}

#' Check `max_rows`/`columns` and normalise them
#'
#' The messages match the Python client's (`_validate_pushdown`), because the
#' two clients are documented as interchangeable and a user who hits a type
#' error in one should recognise it in the other (R-22).
#'
#' @keywords internal
#' @noRd
.irw_check_pushdown <- function(max_rows = NULL, columns = NULL) {
  if (!is.null(max_rows)) {
    # TRUE is not 1 here: irw_fetch(..., max_rows = TRUE) is a mistake, the
    # same one the Python client rejects because bool subclasses int.
    if (is.logical(max_rows) || !is.numeric(max_rows) || length(max_rows) != 1L ||
        is.na(max_rows) || max_rows != as.integer(max_rows)) {
      stop("max_rows must be a single whole number or NULL.", call. = FALSE)
    }
    if (max_rows < 1) {
      stop(sprintf("max_rows must be at least 1; got %s.", max_rows), call. = FALSE)
    }
    max_rows <- as.integer(max_rows)
  }
  if (!is.null(columns)) {
    if (!is.character(columns)) {
      stop("columns must be a character vector of column names, or NULL.", call. = FALSE)
    }
    if (length(columns) == 0L) {
      stop("columns cannot be empty; pass NULL for all columns.", call. = FALSE)
    }
    if (anyNA(columns) || any(!nzchar(trimws(columns)))) {
      stop("All column names must be non-empty strings.", call. = FALSE)
    }
  }
  list(max_rows = max_rows, columns = columns)
}

#' Fetch Table(s) from the Item Response Warehouse
#'
#' Retrieves one or more tables from IRW and returns them as tibbles.
#' By default this downloads every row of each table and counts against the
#' Redivis export quota. Pass `max_rows` and/or `columns` to transfer a page
#' rather than a table -- both are pushed to Redivis, so the rest is never
#' exported. If you only need the item or response value sets, or per-item
#' counts and ranges, use [irw_table_sets()], which answers with a server-side
#' query.
#' If the table includes a character `resp` column, the function attempts to
#' coerce it into numeric. Strings like `"NA"`, `""`, and `NA` are treated as missing values.
#' A warning is issued only if other non-numeric values are encountered.
#'
#' Each table downloaded is also kept on disk (see [irw_cache_dir()]) and reused
#' by later fetches, in this session or another, for as long as the table is
#' unchanged on Redivis; the Python package shares the same copies. Switch this
#' off with `options(irw.cache = FALSE)` or `IRW_CACHE=0`.
#'
#' A table the IRW found through another collection, such as openESM, prints
#' that collection's note and citation once per session per collection.
#' Silence it with `options(irw.source_note = FALSE)` or `IRW_SOURCE_NOTE=0`.
#'
#' @param name Character vector of one or more table names (IRW table IDs).
#' @param source Character. The data family: \code{"core"} (default), \code{"nom"}, \code{"sim"}, \code{"comp"}, \code{"conj"} (conjoint experiments; see \code{\link{irw_conj_long}}), or \code{"rank"} (rankings; see \code{\link{irw_rank_compress}}).
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param dedup Logical, optional. If TRUE, deduplicates responses based on timing variables. Defaults to FALSE.
#'   If a \code{date} column is present, no deduplication is performed.
#'   If only a \code{wave} column is present, one random response is retained per (id, item) group within each wave.
#'   If neither \code{date} nor \code{wave} is present, one random response is retained per (id, item) pair.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#' @param max_rows Integer, optional. Stop after this many rows. Pushed to
#'   Redivis, so only those rows are exported and counted against the account's
#'   export quota -- which is the point: 326 IRW tables are over a million rows,
#'   and a cap applied after the download has already cost the quota. A slice is
#'   never written to the disk cache, because it cannot stand in for the table;
#'   a cached whole table does serve a slice.
#' @param columns Character vector, optional. Fetch only these columns, also
#'   pushed to Redivis. A column the table does not have is an error. Ignored
#'   for the download when \code{dedup = TRUE} (deduplication needs every
#'   column); the columns are then dropped after deduplicating.
#' @param resp Logical, optional. If TRUE, returns response matrix via \code{irw_long2resp()}. Defaults to FALSE.
#'
#' @return If a single name is provided, returns a tibble. If multiple, returns a named list
#'         of tibbles (or error messages, if retrieval failed).
#'
#' @seealso [irw_table_sets()] for value sets and summaries without a download;
#'   [irw_cache_info()] for the tables kept on disk.
#'
#' @examplesIf irw_has_credentials() || nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))
#' \donttest{
#' # Main IRW data
#' irw_fetch("environment_ltm")
#'
#' # A look at a large table without exporting all of it
#' irw_fetch("neurips_2020", max_rows = 100)
#' irw_fetch("neurips_2020", columns = c("id", "item", "resp"), max_rows = 1000)
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
irw_fetch <- function(name, source = "core", dedup = FALSE, max_rows = NULL,
                      columns = NULL, sim = FALSE, comp = FALSE, nom = FALSE,
                      resp = FALSE) {
  if (missing(name)) {
    stop(
      "Please provide the IRW table name(s) to fetch.\n",
      "Tip: Use `irw_list_tables()` to see available tables.",
      call. = FALSE
    )
  }

  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)
  bounds <- .irw_check_pushdown(max_rows, columns)
  max_rows <- bounds$max_rows
  columns <- bounds$columns

  process_one <- function(nm) {
    dat <- fetch_single_data(nm, source = source, dedup = dedup,
                             max_rows = max_rows, columns = columns)
    # Dedup grouped on columns the caller may not have asked for, so narrow now.
    if (!is.null(dat) && !is.null(columns) && dedup) {
      missing <- setdiff(columns, names(dat))
      if (length(missing) > 0) {
        stop(sprintf("Table %s has no column(s) %s.", shQuote(nm),
                     paste(shQuote(missing), collapse = ", ")), call. = FALSE)
      }
      dat <- dat[, columns, drop = FALSE]
    }
    # Credit a table found through an intermediary such as openESM (#2421).
    # Never raises and is silent on any failure.
    if (!is.null(dat)) .irw_source_note(nm, source)
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
#' Each source has its own columns. For \code{source = "conj"} a row is one
#' conjoint experiment: \code{n_respondents}, \code{n_rows}, \code{n_tasks},
#' \code{n_profiles}, \code{n_attributes}, \code{outcomes},
#' \code{n_optout_tasks}, and the design facts \code{country},
#' \code{display_language}, \code{label_language}, \code{restrictions}
#' (\code{none}, \code{yes}, \code{observed} or \code{unknown}),
#' \code{restrictions_note}, \code{task_source} and \code{profile_source}
#' (\code{recorded} or \code{inferred} from row order), and the topic tags
#' \code{domain} (what the study is about; one or more values joined with
#' \code{";"}, primary first) and \code{choice_object} (what one profile
#' represents). Both are filters in \code{\link{irw_filter}}, which documents
#' their vocabularies.
#'
#' For \code{source = "rank"} a row is one rankings table: the counts
#' \code{n_respondents}, \code{n_rows}, \code{n_tasks}, \code{n_items},
#' \code{max_resp} (highest position marked) and \code{n_duplicate_marks},
#' and the design record (\code{method}, \code{seats}, \code{n_positions},
#' \code{n_candidates}, \code{n_ballots_cast}, \code{n_ballots_with_marks},
#' \code{jurisdiction}, \code{date}, \code{office}, ...), whose codes are
#' documented in the irw repo's \code{data/rankings/README.md}.
#'
#' @param source Character. The data family: \code{"core"} (default), \code{"nom"}, \code{"sim"}, \code{"comp"}, \code{"conj"} (conjoint experiments; see \code{\link{irw_conj_long}}), or \code{"rank"} (rankings; see \code{\link{irw_rank_compress}}).
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
  if (source == "conj") return(.fetch_conj_metadata_table())
  if (source == "rank") return(.fetch_rank_metadata_table())
  .fetch_metadata_table()
}



#' Retrieve IRW Tags Table
#'
#' Fetches the tags table from Redivis and returns it as a tibble.
#' Link to Redivis table: https://redivis.com/datasets/bdxt-4fqe5tyf4/tables/7nkh-6m7wq01yv

#' Automatically checks for updates and refreshes only when needed.
#'
#' @param tables Optional. A character vector of table name(s) to filter by.
#' @param source Character. The data family: \code{"core"} (default) or \code{"nom"}.
#'   \code{"comp"} and \code{"sim"} have no tags by design and error.
#'
#' @return A tibble containing tags information.
#' @examplesIf irw_has_credentials() || nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))
#' \donttest{
#' irw_tags()                  # core
#' irw_tags(source = "nom")    # nominal
#' }
#' @export
irw_tags <- function(tables = NULL, source = "core") {
  tags <- .irw_tags_for_source(source)
  
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



