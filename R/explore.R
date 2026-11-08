#' List Available Tables
#'
#' Retrieves a summary of available tables in the IRW database, including their name,
#' number of rows, and variable count, sorted alphabetically by table name.
#'
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#' @param source Character. The data family: \code{"core"} (default), \code{"nom"}, \code{"sim"}, \code{"comp"}, \code{"conj"} (conjoint experiments; see \code{\link{irw_conj_long}}), or \code{"rank"} (rankings; see \code{\link{irw_rank_compress}}).
#' @return A data frame with the following columns:
#'   \item{name}{The name of the table, sorted alphabetically.}
#'   \item{numRows}{The number of rows in the table.}
#'   \item{variableCount}{The number of variables in the table.}
#' @examplesIf irw_has_credentials() || nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))
#' \donttest{
#' irw_list_tables()               # Main IRW database
#' irw_list_tables(source = "sim")   # Simulation dataset
#' irw_list_tables(source = "comp")  # Competition dataset
#' }
#' @export
irw_list_tables <- function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)
  ds_list <- .irw_order_datasources(.initialize_datasource(source = source), source = source)

  tables_info_list <- lapply(ds_list, function(ds) {
    tables <- .retry_with_backoff(function() ds$list_tables())
    data.frame(
      name = vapply(tables, function(tbl) tbl$name, character(1)),
      numRows = vapply(tables, function(tbl) tbl$properties$numRows, numeric(1)),
      variableCount = vapply(tables, function(tbl) tbl$properties$variableCount, numeric(1)),
      stringsAsFactors = FALSE
    )
  })
  
  tables_info <- do.call(rbind, tables_info_list)
  .irw_warn_shard_duplicates(tables_info, source)
  tables_info <- .irw_dedup_table_info(tables_info)
  tables_info <- tables_info[order(tables_info$name), ]
  rownames(tables_info) <- NULL
  
  return(tables_info)
}


#' Totals across a list of opened Redivis datasets
#'
#' @param ds_list List of opened datasets.
#' @return List with tables, size_gb, and created/updated (seconds since the epoch).
#' @keywords internal
#' @noRd
.irw_datasets_totals <- function(ds_list) {
  prop <- function(name) {
    vapply(ds_list, function(ds) as.numeric(ds$properties[[name]]), numeric(1))
  }
  list(
    tables = sum(prop("tableCount")),
    size_gb = sum(prop("totalNumBytes")) / (1024^3),
    created = min(prop("createdAt")) / 1000,
    updated = max(prop("updatedAt")) / 1000
  )
}

#' @keywords internal
#' @noRd
.irw_format_utc <- function(seconds) {
  format(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
}

#' Print table count and size for every data family, with totals
#'
#' A family that cannot be opened is shown as unavailable and named in a
#' warning, so that one family's outage neither stops the report nor passes
#' as a smaller total.
#'
#' @return The opened core datasets, for the optional per-dataset breakdown.
#' @keywords internal
#' @noRd
.irw_print_all_families_info <- function() {
  family_labels <- c(core = "Core", nom = "Nominal", sim = "Simsyn",
                     comp = "Competitions", conj = "Conjoint", rank = "Rankings")

  opened <- lapply(.irw_sources, function(src) {
    tryCatch(.initialize_datasource(source = src), error = function(e) {
      warning(sprintf("Could not open the '%s' family: %s", src, conditionMessage(e)),
              call. = FALSE)
      list()
    })
  })
  names(opened) <- .irw_sources

  available <- lengths(opened) > 0
  if (!any(available)) {
    stop("Could not open any IRW data family.", call. = FALSE)
  }
  totals <- lapply(opened[available], .irw_datasets_totals)

  message(strrep("-", 50))
  message("IRW Database Information (All Families)")
  message(strrep("-", 50))
  message(sprintf("%-14s %-8s %8s %14s", "Family", "source", "Tables", "Size"))
  for (src in .irw_sources) {
    if (available[[src]]) {
      message(sprintf("%-14s %-8s %8d %11.2f GB", family_labels[[src]], src,
                      totals[[src]]$tables, totals[[src]]$size_gb))
    } else {
      message(sprintf("%-14s %-8s %8s", family_labels[[src]], src, "unavailable"))
    }
  }
  message(strrep("-", 50))
  total_label <- if (all(available)) "Total Table Count:" else "Total (available only):"
  message(sprintf("%-25s %d", total_label,
                  sum(vapply(totals, function(x) x$tables, numeric(1)))))
  message(sprintf("%-25s %.2f GB", "Total Data Size:",
                  sum(vapply(totals, function(x) x$size_gb, numeric(1)))))
  message(sprintf("%-25s %s", "Earliest Created At:",
                  .irw_format_utc(min(vapply(totals, function(x) x$created, numeric(1))))))
  message(sprintf("%-25s %s", "Latest Updated At:",
                  .irw_format_utc(max(vapply(totals, function(x) x$updated, numeric(1))))))
  message("One family only: irw_info(source = \"<source>\")")

  opened$core
}


#' Retrieve and Print IRW Database or Table Information
#'
#' Prints information about the IRW datasets or a specific table.
#'
#' If \code{table_name} and \code{source} are both omitted, shows the table count
#' and size of every data family, with totals across them. Name a \code{source}
#' to see one family on its own. Use \code{details = TRUE} to also print a
#' per-dataset breakdown (main IRW only). If a table name is provided, shows
#' details for that table, looked up in \code{source}.
#'
#' @param table_name Optional. Table name to describe; if \code{NULL}, prints database‑level info.
#' @param details Logical. When \code{TRUE} and \code{table_name} is \code{NULL}, also
#'   prints a breakdown by dataset. Defaults to \code{FALSE}.
#' @param source Character. The data family: \code{"core"} (default for a table lookup), \code{"nom"}, \code{"sim"}, \code{"comp"}, \code{"conj"} (conjoint experiments; see \code{\link{irw_conj_long}}), or \code{"rank"} (rankings; see \code{\link{irw_rank_compress}}). Omit it with no \code{table_name} for every family at once.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#'
#' @return Invisibly returns \code{NULL}.
#' @examplesIf irw_has_credentials() || nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))
#' \donttest{
#' irw_info()                 # Every data family, with totals
#' irw_info(source = "core")  # The main IRW datasets only
#' irw_info(details = TRUE)   # Every family + per-dataset breakdown of core
#' irw_info("frac20")      # Specific IRW table
#' irw_info("t20_hyper", source = "comp")  # Competition table
#' irw_info("gilbert_meta_3", source = "sim")
#' 
#' }
#' @export
irw_info <- function(table_name = NULL, details = FALSE, source = "core", comp = FALSE, sim = FALSE, nom = FALSE) {
  # Read before .irw_resolve_source() assigns to source, which makes it non-missing
  all_families <- is.null(table_name) && missing(source) &&
    !(isTRUE(sim) || isTRUE(comp) || isTRUE(nom))

  if (!is.logical(details) || length(details) != 1) {
    stop("'details' must be a single TRUE or FALSE value.")
  }
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)

  if (isTRUE(details) && source != "core") {
    stop("details = TRUE is only available for the main IRW datasource (source = \"core\").")
  }

  # label for messages
  db_label <- if (source == "sim") {
    "IRW Simulation Database Information"
  } else if (source == "comp") {
    "IRW Competition Database Information"
  } else if (source == "nom") {
    "IRW Nominal Database Information"
  } else if (source == "conj") {
    "IRW Conjoint Database Information"
  } else if (source == "rank") {
    "IRW Rankings Database Information"
  } else {
    "IRW Database Information (Combined)"
  }

  if (is.null(table_name)) {
    if (all_families) {
      ds_list <- .irw_print_all_families_info()
    } else {
      ds_list <- .initialize_datasource(source = source)
      totals <- .irw_datasets_totals(ds_list)

      message(strrep("-", 50))
      message(db_label)
      message(strrep("-", 50))
      message(sprintf("%-25s %d", "Total Table Count:", totals$tables))
      message(sprintf("%-25s %.2f GB", "Total Data Size:", totals$size_gb))
      message(sprintf("%-25s %s", "Earliest Created At:", .irw_format_utc(totals$created)))
      message(sprintf("%-25s %s", "Latest Updated At:", .irw_format_utc(totals$updated)))
    }
    message(strrep("-", 50))
    message(sprintf("%-25s %s", "Data Website:", "https://itemresponsewarehouse.org/"))
    message(sprintf("%-25s %s", "Methodology:",
                    "Tables harmonized as per https://itemresponsewarehouse.org/standard.html"))
    message(sprintf("%-25s %s", "Usage Information:",
                    "License & citation info: https://itemresponsewarehouse.org/docs.html"))
    message(strrep("-", 50))

    # --- Optional per-dataset details ---
    if (details) {
      for (i in seq_along(ds_list)) {
        ds <- ds_list[[i]]
        message(sprintf("Dataset %d:", i))
        message(sprintf("  %-22s %s", "Version:", ds$properties$version$tag))
        message(sprintf("  %-22s %d", "Table Count:", ds$properties$tableCount))
        message(sprintf("  %-22s %.2f GB", "Total Data Size:",
                        ds$properties$totalNumBytes / (1024^3)))
        message(sprintf("  %-22s %s", "Created At:",
                        format(as.POSIXct(ds$properties$createdAt / 1000,
                                          origin = "1970-01-01", tz = "UTC"),
                               "%Y-%m-%d %H:%M:%S")))
        message(sprintf("  %-22s %s", "Last Updated At:",
                        format(as.POSIXct(ds$properties$updatedAt / 1000,
                                          origin = "1970-01-01", tz = "UTC"),
                               "%Y-%m-%d %H:%M:%S")))
        message(sprintf("  %-22s %s", "Redivis URL:", ds$properties$url))
        message(strrep("-", 50))
      }
    }
  } else {
    # --- Table-specific info ---
    table <- .fetch_redivis_table(table_name, source = source)
    ds_version <- attr(table, "dataset_version")
    
    # Variable names
    vars <- NA
    if (source == "core") {
      m <- .fetch_metadata_table()
      v <- m$variables[m$table == table_name]
      vars <- if (length(v) > 0 && !is.na(v[1]) && nzchar(v[1])) v[1] else NA
    } else {
      # sim / comp / nom: infer from the table's schema rather than its rows,
      # so listing variables does not export the whole table
      vars <- paste(.irw_table_variable_names(table), collapse = " | ")
    }

    # Tags: core and nom only (comp/sim have no tags by design)
    construct <- "Not available for this datasource"
    if (source %in% .irw_tag_sources) {
      construct <- "No construct specified"
      tags <- .irw_tags_for_source(source)
      cvec <- tags$construct_name[tags$table == table_name]
      if (length(cvec) > 0 && !is.na(cvec[1]) && nzchar(cvec[1])) {
        construct <- cvec[1]
      }
    }
    
    # Collections: core only (issue #1633). Reads the same list filter.R does
    # rather than testing source == "core".
    collections <- "Not available for this datasource"
    if (source %in% .irw_collection_sources) {
      collections <- tryCatch({
        mem <- .fetch_collection_members_table()
        hit <- sort(unique(mem$collection[tolower(mem$table) == tolower(table_name)]))
        if (length(hit) > 0) paste(hit, collapse = ", ") else "None"
      }, error = function(e) "None")
    }

    # Biblio: every source has its own (core, comp, sim, nom, conj, rank)
    description <- NA
    doi <- NA
    url_data <- NA
    licence <- NA
    licence_terms <- NA_character_
    reference <- NA
    bib_found <- FALSE
    
    bib <- if (source == "comp") {
      .fetch_comps_biblio_table()
    } else if (source == "sim") {
      .fetch_simsyn_biblio_table()
    } else if (source == "nom") {
      .fetch_nominal_biblio_table()
    } else if (source == "conj") {
      .fetch_conj_biblio_table()
    } else if (source == "rank") {
      .fetch_rank_biblio_table()
    } else {
      .fetch_biblio_table()
    }
    
    thisbib <- bib[bib$table == table_name, , drop = FALSE]
    bib_found <- nrow(thisbib) > 0L
    
    if (bib_found) {
      doi <- thisbib$DOI__for_paper_
      url_data <- thisbib$URL__for_data_
      licence <- thisbib$Derived_License
      licence_terms <- .irw_license_terms(thisbib)[1]
      description <- thisbib$Description
      reference <- thisbib$Reference_x
    }
    
    num_rows <- table$properties$numRows
    data_size <- table$properties$numBytes / 1024
    variable_count <- table$properties$variableCount
    table_url <- table$properties$url
    
    message(strrep("-", 50))
    message("Table Information for: ", table_name,
            if (source == "sim") " (simulation)" else if (source == "comp") " (competition)" else if (source == "nom") " (nominal)" else if (source == "conj") " (conjoint)" else if (source == "rank") " (rankings)" else "")
    message(strrep("-", 50))
    message(strrep("-", 50))
    message(sprintf("%-25s %s", "Description:", description))
    message(sprintf("%-25s %s", "Construct:", construct))
    message(sprintf("%-25s %s", "Collections:", collections))
    message(sprintf("%-25s %d", "Number of Rows:", num_rows))
    message(sprintf("%-25s %d", "Variable Count:", variable_count))
    message(sprintf("%-25s %s", "Variables:", vars))
    message(sprintf("%-25s %.2f KB", "Data Size (KB):", data_size))
    message(strrep("-", 50))
    if (!is.null(ds_version) && !is.na(ds_version) && nzchar(ds_version)) {
      message(sprintf("%-25s %s", "Dataset Version:", ds_version))
    }
    message(sprintf("%-25s %s", "Redivis URL:", table_url))
    message(sprintf("%-25s %s", "Data URL:", url_data))
    message(sprintf("%-25s %s", "DOI:", doi))
    message(sprintf("%-25s %s", "License:", licence))
    # Terms are shown whenever recorded, not only for "Custom": some CC BY
    # tables carry attribution conditions here too.
    if (!is.na(licence_terms)) {
      message(sprintf("%-25s %s", "License Terms:", licence_terms))
    } else if (isTRUE(any(licence == "Custom"))) {
      message(sprintf("%-25s %s", "License Terms:",
                      "not recorded in IRW; check the data source before reuse"))
    }
    message(strrep("-", 50))
    message(sprintf("%s%s", "Reference:\n", reference))
    message(strrep("-", 50))
    
    if (!bib_found) {
      message("NOTE: No bibliography row found for this table in the selected source.")
      if (source == "core") {
        message("Hint: If this is a competition / simulation / nominal table, try source = \"comp\", source = \"sim\", or source = \"nom\".")
      }
    }
  }
  
  invisible(NULL)
}
