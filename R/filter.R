#' View Available Tag Values
#'
#' Returns all unique individual tag values from a given tag metadata column
#' (e.g., "age_range", "construct_type"). Handles multi-tag fields that may include
#' commas inside quoted strings.
#'
#' @param column A character string specifying the tag column name.
#' @return A sorted character vector of unique tag values
#' @examples
#' \dontrun{
#' irw_tag_options("construct_type")
#' irw_tag_options("sample")
#' }
#' @export
irw_tag_options <- function(column) {
  tags <- .fetch_tags_table()
  
  if (!column %in% colnames(tags)) {
    stop(sprintf("'%s' is not a valid column in the tags table. Use names(irw_tags()) to see available options.", column))
  }
  
  all_values <- tags[[column]]
  all_values <- all_values[!is.na(all_values)]
  
  parse_mixed_quoted_tags <- function(x) {
    # Step 1: Replace escaped quotes with real quotes
    x <- gsub('\\"', '"', x)
    
    # Step 2: If string has no quote and is a known atomic tag, treat it as a whole
    if (!grepl('"', x) && grepl("^Internet-based \\(Mturkers, etc\\)$", x)) {
      return("Internet-based (Mturkers, etc)")
    }
    
    # Step 3: Normal quote-aware parsing
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
  parsed_clean <- sort(unique(parsed_flat[nzchar(parsed_flat)]))
  parsed_clean
}


#' Filter Available Datasets in IRW
#'
#' Identifies datasets in the Item Response Warehouse (IRW) based on user-defined criteria.
#' This function filters datasets using **precomputed metadata**, which contains summary statistics
#' for each dataset (e.g., number of responses, number of participants, density scores, etc.),
#' as well as tag-based metadata (e.g., age range, construct type, sample type, etc.).
#' 
#' ## Exploring Metadata and Tags
#' To understand available dataset properties before filtering, run `summary(irw_metadata())`.
#' To explore tag-based metadata, use `irw_tags()` to view the full tags table.
#'
#' To see the valid values for a specific tag column (e.g., "age_range"), use:
#' `irw_tag_options("column_name")`
#'
#' @param n_responses Numeric vector of length 2 specifying range for total responses.
#' @param n_categories Numeric vector of length 2 specifying range for unique response categories.
#' @param n_participants Numeric vector of length 2 specifying range for number of participants.
#' @param n_items Numeric vector of length 2 specifying range for number of items.
#' @param responses_per_participant Numeric vector of length 2 specifying range for avg responses per participant.
#' @param responses_per_item Numeric vector of length 2 specifying range for avg responses per item.
#' @param density Numeric vector of length 2 specifying range for data density. Default `c(0.5, 1)`; disable with `NULL`.
#' @param var A character vector specifying one or more variables. For a list of available variables, see: https://datapages.github.io/irw/standard.html
#'            - If **exact variable names** are provided, only datasets containing **all specified variables** will be returned.
#'            - If a variable name **contains an underscore** (e.g., `"cov_"`, `"Qmatrix_"`), the function will match all datasets that
#'              contain **at least one variable** that starts with that prefix.
#' @param age_range Character value specifying the age group of participants (e.g., "Adult (18+)", "Child (<18y)").
#' @param child_age__for_child_focused_studies_ Character value indicating the age group for child-focused studies (e.g., "Early (<6y)").
#' @param construct_type Character value specifying the psychological or educational construct being measured.
#' @param sample Character value specifying the sample type or recruitment method (e.g., "Educational", "Clinical").
#' @param measurement_tool Character value specifying the type of instrument used for measurement (e.g., "Survey/questionnaire").
#' @param item_format Character value describing the format of the items (e.g., "Likert Scale/selected response").
#' @param primary_language_s_ Character value indicating the primary language(s) used in the instrument.
#' @param longitudinal Logical or NULL. If TRUE, returns only longitudinal datasets 
#'        (i.e., those with variables like 'wave' or 'date'). 
#'        If FALSE, excludes those datasets. If NULL (default), includes all datasets.
#'        
#' @return Sorted character vector of dataset names matching **all specified criteria** or empty if none found.
#' 
#'
#' @examples
#' \dontrun{
#' irw_filter(n_responses = c(1000, Inf), var = "rt")
#' irw_filter(var = "wave", density = NULL)
#' irw_filter(n_participants = c(500, Inf), density = c(0.3, 0.8))
#' irw_filter(var = c("treat", "rt", "cov_"), density= NULL)
#' 
#' # View valid options for a tag column
#' irw_tag_options("construct_type")
#' 
#' irw_filter(construct_type = "Affective/mental health")
#' 
#' # IRW tables based on response categories
#' # all tables with purely dichotomous responses
#' irw_filter(n_categories=2,density = NULL) 
#' # all tables with responses in 3-5 categories
#' irw_filter(n_categories=c(3,5),density = NULL)
#' # all tables with relatively large numbers of response categories
#' irw_filter(n_categories=c(10,Inf),density = NULL) 
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
                       sample = NULL,
                       measurement_tool = NULL,
                       item_format = NULL,
                       primary_language_s_ = NULL,
                       longitudinal = NULL) {
  
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
  
  # Convert variables column to list for filtering
  metadata$variables_list <- strsplit(metadata$variables, "\\| ")
  
  longitudinal_vars <- c("wave", "date")
  
  if (!is.null(longitudinal)) {
    has_longitudinal <- vapply(metadata$variables_list, function(vars) {
      any(tolower(vars) %in% longitudinal_vars)
    }, logical(1))
    
    if (isTRUE(longitudinal)) {
      metadata <- metadata[has_longitudinal, ]
    } else if (isFALSE(longitudinal)) {
      metadata <- metadata[!has_longitudinal, ]
      
      # Optional: helpful message
      if (!is.null(var) && any(tolower(var) %in% longitudinal_vars)) {
        message("Note: datasets with longitudinal variables like 'wave' or 'date' were excluded. Set longitudinal = NULL or TRUE to include them.")
      }
    }
  }
  
  # --- VARIABLE FILTERING ---
  if (!is.null(var)) {
    metadata <- metadata[vapply(metadata$variables_list, function(vars) {
      all(vapply(var, function(v) {
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
