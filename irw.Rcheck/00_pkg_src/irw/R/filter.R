#' View Available Tag Values with Frequencies
#'
#' Returns a data frame of unique tag values from a given tag metadata column,
#' along with the number of datasets each tag appears in. Handles multi-tag fields
#' with quoted values containing commas.
#'
#' @param column A character string specifying the tag column name.
#' @return A data.frame with columns: `tag` and `count`, sorted by descending frequency.
#' @export
irw_tag_options <- function(column) {
  tags <- .fetch_tags_table()
  
  if (missing(column)) {
    message("Available tag columns:\n", paste(names(tags), collapse = ", "))
    return(invisible(names(tags)))
  }
  
  if (!column %in% colnames(tags)) {
    stop(sprintf("'%s' is not a valid column in the tags table. Use names(irw_tags()) to see available options.", column))
  }
  
  all_values <- tags[[column]]
  all_values <- all_values[!is.na(all_values)]
  
  parse_mixed_quoted_tags <- function(x) {
    x <- gsub('\\"', '"', x)
    if (!grepl('"', x) && grepl("^Internet-based \\(Mturkers, etc\\)$", x)) {
      return("Internet-based (Mturkers, etc)")
    }
    
    chars <- strsplit(x, "")[[1]]
    in_quotes <- FALSE
    buffer <- ""
    parts <- character()
    
    for (ch in chars) {
      if (ch == '"') {
        in_quotes <- !in_quotes
      } else if (ch == "," && !in_quotes) {
        parts <- c(parts, trimws(buffer))
        buffer <- ""
      } else {
        buffer <- paste0(buffer, ch)
      }
    }
    parts <- c(parts, trimws(buffer))
    parts <- gsub('^"|"$', '', parts)
    parts[nzchar(parts)]
  }
  
  parsed_list <- lapply(all_values, function(x) {
    result <- tryCatch(parse_mixed_quoted_tags(x), error = function(e) character(0))
    if (length(result) > 0 && all(nzchar(result))) result else NULL
  })
  
  parsed_flat <- unlist(parsed_list, use.names = FALSE)
  parsed_clean <- parsed_flat[nzchar(parsed_flat)]
  
  freq_table <- sort(table(parsed_clean), decreasing = TRUE)
  out <- data.frame(
    tag = names(freq_table),
    count = as.integer(freq_table),
    row.names = NULL
  )
  
  return(out)
}

#' View Unique License Options with Frequencies
#'
#' Returns a data frame showing the number of datasets associated with each license.
#'
#' @param source Character. Data source: \code{"core"} (default), \code{"sim"},
#'   \code{"nom"}, or \code{"comp"}.
#' @param comp Deprecated. Use \code{source = "comp"} instead.
#' @param sim Deprecated. Use \code{source = "sim"} instead.
#' @param nom Deprecated. Use \code{source = "nom"} instead.
#' @return A data.frame with 'license' and 'count' columns.
#' @export
irw_license_options <- function(source = "core", comp = FALSE, sim = FALSE, nom = FALSE) {
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)
  bib <- .irw_filter_biblio(source)
  
  freqs <- sort(table(bib$Derived_License), decreasing = TRUE)
  
  data.frame(
    license = names(freqs),
    count = as.integer(freqs),
    row.names = NULL
  )
}

.irw_filter_format_value <- function(x) {
  if (is.character(x)) {
    quoted <- sprintf("'%s'", x)
    if (length(quoted) == 1L) {
      return(quoted)
    }
    return(paste0("c(", paste(quoted, collapse = ", "), ")"))
  }

  if (is.logical(x) && length(x) == 1L) {
    return(tolower(as.character(x)))
  }

  if (length(x) == 1L) {
    return(as.character(x))
  }

  paste0("c(", paste(as.character(x), collapse = ", "), ")")
}

.irw_filter_describe <- function(name, value) {
  if (is.numeric(value) && length(value) == 2L) {
    return(sprintf("%s in [%s, %s]", name, value[1], value[2]))
  }

  sprintf("%s = %s", name, .irw_filter_format_value(value))
}

.irw_filter_no_match <- function(filter_name, value) {
  message("0 tables matched ", .irw_filter_describe(filter_name, value), ".")
  character(0)
}

.irw_filter_biblio <- function(source) {
  switch(source,
         core = .fetch_biblio_table(),
         sim = .fetch_simsyn_biblio_table(),
         nom = .fetch_nominal_biblio_table(),
         comp = .fetch_comps_biblio_table())
}

.irw_filter_comp_impl <- function(n_responses = NULL,
                                  n_actors = NULL,
                                  license = NULL) {
  meta <- .fetch_comps_metadata_table()

  if (!all(c("table", "n_responses", "n_actors") %in% names(meta))) {
    stop("Competition metadata table must contain columns: 'table', 'n_responses', 'n_actors'.")
  }

  if (!is.null(license)) {
    bib <- .fetch_comps_biblio_table()

    if (!"Derived_License" %in% names(bib)) {
      warning("No 'Derived_License' column found in competition bibliography table. Skipping license filter.")
    } else {
      keep <- !is.na(bib$Derived_License) & bib$Derived_License %in% license
      matched_tables <- bib$table[keep]
      meta <- meta[meta$table %in% matched_tables, , drop = FALSE]
      if (nrow(meta) == 0L) {
        return(.irw_filter_no_match("license", license))
      }
    }
  }

  numeric_filters <- list(
    n_responses = n_responses,
    n_actors = n_actors
  )
  numeric_filters <- numeric_filters[vapply(numeric_filters, Negate(is.null), logical(1))]

  for (nm in names(numeric_filters)) {
    rng <- numeric_filters[[nm]]
    if (!is.numeric(rng)) stop(sprintf("'%s' must be numeric.", nm))
    if (length(rng) == 1L) rng <- rep(rng, 2)
    if (length(rng) != 2L) stop(sprintf("'%s' must be length 1 or 2.", nm))

    meta <- meta[!is.na(meta[[nm]]) &
                   meta[[nm]] >= rng[1] &
                   meta[[nm]] <= rng[2], , drop = FALSE]
    if (nrow(meta) == 0L) {
      return(.irw_filter_no_match(nm, rng))
    }
  }

  sort(unique(meta$table))
}

#' Filter Available Datasets in IRW
#'
#' Returns the names of datasets in the Item Response Warehouse (IRW) that match user-specified
#' metadata, tag values, variable presence, and license criteria.
#'
#' Filtering is based on:
#' - **Numeric metadata**: number of responses, participants, items, etc.
#' - **Tag metadata**: e.g., construct type, sample, measurement tool
#' - **Variable presence**: e.g., `rt`, `wave`, `cov_`
#' - **License type**: e.g., `"CC BY 4.0"`, `"CC0 1.0"`
#'
#' ## Metadata and Tag-Based Filtering
#'
#' To explore available metadata:
#' - `summary(irw_metadata())` — numeric summaries (e.g., `n_responses`, `density`)
#' - `irw_tags()` — full tag metadata table (1 row per dataset)
#' - `irw_tag_options("column_name")` — valid values (with counts) for any tag column
#' - `irw_license_options()` — available license values with frequencies
#'
#' Tag-based metadata (e.g., `construct_type`, `sample`, `item_format`) can be passed
#' directly as named arguments. See the parameter list below for supported tag columns.
#'
#' @param n_responses Numeric vector of length 1 or 2. Filters datasets by total number of responses.
#'   - Length 1: exact value (e.g., `n_responses = 1000`)
#'   - Length 2: range (e.g., `n_responses = c(1000, Inf)`)
#' @param n_categories Numeric vector of length 1 or 2. Filters by number of unique response categories.
#' @param n_participants Numeric vector of length 1 or 2. Filters by number of unique participants (`id`).
#' @param n_items Numeric vector of length 1 or 2. Filters by number of unique items.
#' @param responses_per_participant Numeric vector of length 1 or 2. Filters by average responses per participant.
#' @param responses_per_item Numeric vector of length 1 or 2. Filters by average responses per item.
#' @param density Numeric vector of length 1 or 2, or `NULL`. Filters by matrix density.
#'   - Default is `c(0.5, 1)` to exclude sparse matrices.
#'   - Use `NULL` to disable this filter.
#' @param var Character vector. Filters datasets by presence of variables.
#'   - Use exact names (e.g., `"rt"`, `"wave"`), or
#'   - Use a prefix (e.g., `"cov_"`) to match any variable starting with that prefix.
#' @param longitudinal Logical or `NULL`. Filters longitudinal datasets with variables `wave` or `date`.
#'   - `TRUE`: include only datasets flagged as longitudinal
#'   - `FALSE`: exclude datasets flagged as longitudinal
#'   - `NULL` (default): no filter
#' @param age_range Character vector. Filters by participant age group (e.g., `"Adult (18+)"`).
#'   See `irw_tag_options("age_range")` for values.
#' @param child_age__for_child_focused_studies_ Character vector. Filters by child age subgroup.
#'   See `irw_tag_options("child_age__for_child_focused_studies_")` for values.
#' @param construct_type Character vector. Filters by high-level construct category
#'   (e.g., `"Affective/mental health"`). See `irw_tag_options("construct_type")`.
#' @param construct_name Character vector. Filters by specific construct (e.g., `"Big Five"`).
#'   See `irw_tag_options("construct_name")`.
#' @param sample Character vector. Filters by sample type or recruitment method
#'   (e.g., `"Educational"`, `"Clinical"`). See `irw_tag_options("sample")`.
#' @param measurement_tool Character vector. Filters by instrument type (e.g., `"Survey/questionnaire"`).
#'   See `irw_tag_options("measurement_tool")`.
#' @param item_format Character vector. Filters by item format (e.g., `"Likert Scale/selected response"`).
#'   See `irw_tag_options("item_format")`.
#' @param primary_language_s_ Character vector. Filters by language used (e.g., `"eng"`).
#'   See `irw_tag_options("primary_language_s_")`.
#' @param n_actors Numeric vector of length 1 or 2. Competition-only filter for the
#'   number of actors. Only used when `source = "comp"`.
#' @param source Character. Data source: `"core"` (default), `"nom"`, `"sim"`, or `"comp"`.
#' @param comp Deprecated. Use `source = "comp"` instead.
#' @param sim Deprecated. Use `source = "sim"` instead.
#' @param nom Deprecated. Use `source = "nom"` instead.
#' @param license Character vector. Filters datasets by license (e.g., `"CC BY 4.0"`).
#'   See `irw_license_options()` for available values.
#' @return A sorted character vector of dataset names that match all specified filters, or `character(0)` if no match is found.
#'
#' @examples
#' \dontrun{
#' # Numeric filters
#' irw_filter(n_responses = c(1000, Inf), n_items = c(10, 50))
#' irw_filter(n_participants = c(500, Inf), density = c(0.3, 0.9))
#'
#' # Variable presence
#' irw_filter(var = "rt")
#' irw_filter(var = c("wave", "cov_"), density = NULL)
#'
#' # Tag metadata filtering
#' irw_filter(construct_type = "Affective/mental health", sample = "Educational")
#' irw_tag_options("construct_type")  # view tag values
#'
#' # License filtering
#' irw_license_options()
#' irw_filter(license = "CC BY 4.0")
#'
#' # Filter by response category complexity
#' irw_filter(n_categories = 2, density = NULL)           # binary
#' irw_filter(n_categories = c(3, 5), density = NULL)     # small multi-category
#' irw_filter(n_categories = c(10, Inf), density = NULL)  # large category sets
#' irw_filter(source = "sim", n_items = c(10, Inf))
#' irw_filter(source = "comp", n_actors = c(2, 10))
#' }
#' @export
irw_filter <- function(n_responses = NULL,
                       n_categories = NULL,
                       n_participants = NULL,
                       n_items = NULL,
                       responses_per_participant = NULL,
                       responses_per_item = NULL,
                       density = c(0.5, 1),
                       var = NULL,
                       age_range = NULL,
                       child_age__for_child_focused_studies_ = NULL,
                       construct_type = NULL,
                       construct_name = NULL,
                       sample = NULL,
                       measurement_tool = NULL,
                       item_format = NULL,
                       primary_language_s_ = NULL,
                       longitudinal = NULL,
                       n_actors = NULL,
                       source = "core",
                       comp = FALSE,
                       sim = FALSE,
                       nom = FALSE,
                       license = NULL) {
  density_supplied <- !missing(density)
  source <- .irw_resolve_source(source = source, sim = sim, comp = comp, nom = nom)

  if (!is.null(n_actors) && source != "comp") {
    stop("`n_actors` is only available when `source = \"comp\"`.")
  }

  tag_filters <- list(
    age_range = age_range,
    child_age__for_child_focused_studies_ = child_age__for_child_focused_studies_,
    construct_type = construct_type,
    construct_name = construct_name,
    sample = sample,
    measurement_tool = measurement_tool,
    item_format = item_format,
    primary_language_s_ = primary_language_s_
  )
  tag_filters <- tag_filters[vapply(tag_filters, Negate(is.null), logical(1))]

  if (source == "comp") {
    unsupported_filters <- list(
      n_categories = n_categories,
      n_participants = n_participants,
      n_items = n_items,
      responses_per_participant = responses_per_participant,
      responses_per_item = responses_per_item,
      density = if (density_supplied) density else NULL,
      var = var,
      age_range = age_range,
      child_age__for_child_focused_studies_ = child_age__for_child_focused_studies_,
      construct_type = construct_type,
      construct_name = construct_name,
      sample = sample,
      measurement_tool = measurement_tool,
      item_format = item_format,
      primary_language_s_ = primary_language_s_,
      longitudinal = longitudinal
    )
    unsupported_filters <- unsupported_filters[vapply(unsupported_filters, Negate(is.null), logical(1))]

    if (length(unsupported_filters) > 0L) {
      stop(
        "These filters are not available for `source = \"comp\"`: ",
        paste(names(unsupported_filters), collapse = ", "),
        "."
      )
    }

    return(.irw_filter_comp_impl(
      n_responses = n_responses,
      n_actors = n_actors,
      license = license
    ))
  }

  if (source != "core" && length(tag_filters) > 0L) {
    stop(
      "Tag filters are only available for `source = \"core\"`. Unsupported filter(s): ",
      paste(names(tag_filters), collapse = ", "),
      "."
    )
  }

  metadata <- irw_metadata(source = source)

  if (length(tag_filters) > 0) {
    tags <- .fetch_tags_table()

    for (colname in names(tag_filters)) {
      value <- tag_filters[[colname]]
      if (!is.null(value) && colname %in% colnames(tags)) {
        tags <- tags[
          !is.na(tags[[colname]]) &
            sapply(tags[[colname]], function(x) {
              tag_list <- trimws(unlist(strsplit(x, ",")))
              any(tag_list %in% value)
            }),
        ]
        if (nrow(tags) == 0L) {
          return(.irw_filter_no_match(colname, value))
        }
      } else {
        warning(sprintf("Column '%s' not found in tags table. Ignored.", colname))
      }
    }

    metadata <- metadata[tolower(metadata$table) %in% tolower(tags$table), ]
    if (nrow(metadata) == 0L) {
      return(.irw_filter_no_match(names(tag_filters)[length(tag_filters)], tag_filters[[length(tag_filters)]]))
    }
  }

  if (!is.null(license)) {
    biblio <- .irw_filter_biblio(source)

    if (!"Derived_License" %in% names(biblio)) {
      warning("No license information found in bibliography table. Skipping license filter.")
    } else {
      keep_rows <- !is.na(biblio[["Derived_License"]]) & biblio[["Derived_License"]] %in% license
      matched_tables <- biblio$table[keep_rows]
      metadata <- metadata[tolower(metadata$table) %in% tolower(matched_tables), ]
      if (nrow(metadata) == 0L) {
        return(.irw_filter_no_match("license", license))
      }
    }
  }

  if (!is.null(longitudinal)) {
    if (!"longitudinal" %in% names(metadata)) {
      warning("No 'longitudinal' column in irw_metadata(); skipping longitudinal filter.")
    } else {
      keep_flag <- isTRUE(longitudinal)
      metadata <- metadata[!is.na(metadata$longitudinal) & metadata$longitudinal == keep_flag, ]
      if (nrow(metadata) == 0L) {
        return(.irw_filter_no_match("longitudinal", longitudinal))
      }
    }
  }

  if (!is.null(var)) {
    metadata$variables_list <- strsplit(tolower(metadata$variables), "\\|\\s*")
    var_lc <- tolower(var)

    metadata <- metadata[vapply(metadata$variables_list, function(vars) {
      all(vapply(var_lc, function(v) {
        if (grepl("_", v)) {
          any(grepl(paste0("^", v), vars))
        } else {
          v %in% vars
        }
      }, logical(1)))
    }, logical(1)), ]
    if (nrow(metadata) == 0L) {
      return(.irw_filter_no_match("var", var))
    }
  }

  numeric_filters <- list(
    n_responses = n_responses,
    n_categories = n_categories,
    n_participants = n_participants,
    n_items = n_items,
    responses_per_participant = responses_per_participant,
    responses_per_item = responses_per_item
  )
  
  numeric_filters <- numeric_filters[vapply(numeric_filters, Negate(is.null), logical(1))]
  
  for (filter_name in names(numeric_filters)) {
    filter_value <- numeric_filters[[filter_name]]
    if (length(filter_value) == 1) filter_value <- rep(filter_value, 2)
    metadata <- metadata[metadata[[filter_name]] >= filter_value[1] &
                           metadata[[filter_name]] <= filter_value[2], ]
    if (nrow(metadata) == 0L) {
      return(.irw_filter_no_match(filter_name, filter_value))
    }
  }

  user_specified_density <- density_supplied
  if (!is.null(density)) {
    if (length(density) == 1) density <- rep(density, 2)

    metadata_before_density <- metadata
    metadata <- metadata[metadata$density >= density[1] &
                           metadata$density <= density[2], ]

    num_removed_by_density <- nrow(metadata_before_density) - nrow(metadata)

    if (!user_specified_density &&
        identical(density, c(0.5, 1)) &&
        num_removed_by_density > 0) {
      message(sprintf(
        "Note: Default density filter (0.5-1) removed %d dataset(s). Set density = NULL to disable.",
        num_removed_by_density
      ))
    }

    if (nrow(metadata) == 0L) {
      return(.irw_filter_no_match("density", density))
    }
  }

  return(sort(metadata$table))
}



#' Filter Available Competition Datasets in IRW
#'
#' Returns the names of competition tables that match user-specified criteria.
#' Competition filtering uses:
#' - Numeric metadata: `n_responses`, `n_actors`
#' - License: via `comp_biblio` (`Derived_License`)
#'
#' Notes:
#' - Competition datasets do not have tags or item text metadata.
#' - Competition metadata table names are stored in lowercase.
#'
#' @param n_responses Numeric vector length 1 or 2. Filters by number of responses.
#'   - Length 1: exact value (e.g., `n_responses = 300`)
#'   - Length 2: range (e.g., `n_responses = c(300, Inf)`)
#' @param n_actors Numeric vector length 1 or 2. Filters by number of actors.
#' @param license Character vector. Filters by license (e.g., "CC BY 4.0").
#'   See `irw_license_options(comp = TRUE)` for available values.
#'
#' @return A sorted character vector of competition table names (lowercase),
#'   or `character(0)` if no match is found.
#' @export
irw_filter_comp <- function(n_responses = NULL,
                            n_actors = NULL,
                            license = NULL) {
  irw_filter(
    n_responses = n_responses,
    n_actors = n_actors,
    license = license,
    source = "comp"
  )
}


