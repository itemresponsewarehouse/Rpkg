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
#' @param var A character vector specifying one or more variables.
#'            - If **exact variable names** are provided, only datasets containing **all specified variables** will be returned.
#'            - If a variable name **contains an underscore** (e.g., `"cov_"`, `"Qmatrix_"`), the function will match all datasets that
#'              contain **at least one variable** that starts with that prefix.
#' @param age_range Character value specifying the age group of participants (e.g., "Adult (18+)", "Child").
#' @param child_age__for_child_focused_studies_ Character value indicating the age group for child-focused studies (e.g., "6-10", "11-14").
#' @param construct_type Character value specifying the psychological or educational construct being measured.
#' @param sample Character value specifying the sample type or recruitment method (e.g., "Internet-based", "Lab-based").
#' @param measurement_tool Character value specifying the type of instrument used for measurement (e.g., "Survey/questionnaire").
#' @param item_format Character value describing the format of the items (e.g., "Likert Scale/selected response").
#' @param primary_language_s_ Character value indicating the primary language(s) used in the instrument.
#'
#' @return Sorted character vector of dataset names matching **all specified criteria** or empty if none found.
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
                       primary_language_s_ = NULL) {
  
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
  count_before_density <- nrow(metadata)
  numeric_filters <- list(
    n_responses = n_responses,
    n_categories = n_categories,
    n_participants = n_participants,
    n_items = n_items,
    responses_per_participant = responses_per_participant,
    responses_per_item = responses_per_item,
    density = density
  )
  
  numeric_filters <- numeric_filters[vapply(numeric_filters, Negate(is.null), logical(1))]
  user_specified_density <- !missing(density)
  
  for (filter_name in names(numeric_filters)) {
    filter_value <- numeric_filters[[filter_name]]
    metadata <- metadata[metadata[[filter_name]] >= filter_value[1] &
                           metadata[[filter_name]] <= filter_value[2], ]
  }
  
  num_removed_by_density <- count_before_density - nrow(metadata)
  if (!user_specified_density && identical(density, c(0.5, 1)) && num_removed_by_density > 0) {
    message(sprintf(
      "Note: Default density filter (0.5-1) removed %d dataset(s). Set density = NULL to disable.",
      num_removed_by_density
    ))
  }
  
  return(sort(metadata$table))
}
