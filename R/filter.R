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
#' @return A data.frame with 'license' and 'count' columns.
#' @export
irw_license_options <- function() {
  bib <- .fetch_biblio_table()
  freqs <- sort(table(bib$Derived_License), decreasing = TRUE)
  data.frame(
    license = names(freqs),
    count = as.integer(freqs),
    row.names = NULL
  )
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
                       license = NULL) {
  
  metadata <- irw_metadata()
  
  # --- TAG FILTERING ---
  tag_filters <- list(
    age_range = age_range,
    child_age__for_child_focused_studies_ = child_age__for_child_focused_studies_,
    construct_type = construct_type,
    sample = sample,
    measurement_tool = measurement_tool,
    item_format = item_format,
    primary_language_s_ = primary_language_s_
  )
  
  # Remove NULL filters
  tag_filters <- tag_filters[vapply(tag_filters, Negate(is.null), logical(1))]
  
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
      } else {
        warning(sprintf("Column '%s' not found in tags table. Ignored.", colname))
      }
    }
    
    if (nrow(tags) == 0) {
      return(character(0))
    }
    
    metadata <- metadata[tolower(metadata$table) %in% tolower(tags$table), ]
  }
  
  # --- LICENSE FILTERING ---
  if (!is.null(license)) {
    biblio <- .fetch_biblio_table()
    
    if (!"Derived_License" %in% names(biblio)) {
      warning("No license information found in bibliography table. Skipping license filter.")
    } else {
      keep_rows <- !is.na(biblio[["Derived_License"]]) & biblio[["Derived_License"]] %in% license
      matched <- biblio[keep_rows, "table"]
      metadata <- metadata[tolower(metadata$table) %in% tolower(matched$table), ]
    }
  }
  
  # --- LONGITUDINAL FILTERING ---
  if (!is.null(longitudinal)) {
    if (!"longitudinal" %in% names(metadata)) {
      warning("No 'longitudinal' column in irw_metadata(); skipping longitudinal filter.")
    } else {
      keep_flag <- isTRUE(longitudinal)
      metadata <- metadata[!is.na(metadata$longitudinal) & metadata$longitudinal == keep_flag, ]
    }
  }
  
  
  # --- VARIABLE FILTERING ---
  if (!is.null(var)) {
    # build lazily here
    metadata$variables_list <- strsplit(tolower(metadata$variables), "\\|\\s*")
    var_lc <- tolower(var)
    
    metadata <- metadata[vapply(metadata$variables_list, function(vars) {
      all(vapply(var_lc, function(v) {
        # treat any string with "_" as a prefix indicator (as before)
        if (grepl("_", v)) {
          any(grepl(paste0("^", v), vars))
        } else {
          v %in% vars
        }
      }, logical(1)))
    }, logical(1)), ]
  }
  
  if (nrow(metadata) == 0) return(character(0))
  
  # --- NUMERIC FILTERING ---
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
  }
  
  user_specified_density <- !missing(density)
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
  }
  
  return(sort(metadata$table))
}


