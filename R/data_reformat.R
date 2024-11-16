# Functions for in-session data transformations
# These functions are used to transform data in-session, for example, to clean or reshape data before analysis.

#' @title returns irw dataframes in the format required by the package
#'
#' @description
#' Overview ---------------------------------------------------------------------
#' The `reformat` function is designed to take a data frame and reformat it into the
#' format required by various R packages for analysis. The function is designed to
#' work with the following packages: mirt, lavaan, sem, psych, ltm, mokken, and lme4.
#' The function can handle a variety of data formats including wide and long formats,
#' as well as data with covariates, groups, item groups, raters, and more. The function
#' will automatically identify and convert factor columns to dummy variables if needed.
#'
#'
#'
#'
#'
#'
#' Supported packages:
#' mirt, lavaan, sem, psych, ltm, mokken, lme4
#' Note: not all functions within each package are supported. See details.
#'
#' Manuals ---------------------------------------------------------------------
#'  mirt: https://cran.r-project.org/web/packages/mirt/mirt.pdf
#'  lavaan: https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
#'  sem: https://cran.r-project.org/web/packages/sem/sem.pdf
#'  psych: https://cran.r-project.org/web/packages/psych/psych.pdf
#'  ltm: https://cran.r-project.org/web/packages/ltm/ltm.pdf
#'  mokken: https://cran.r-project.org/web/packages/mokken/mokken.pdf
#'  lme4: https://cran.r-project.org/web/packages/lme4/lme4.pdf
#' as
#' Parameter ordering where {} are values pivoted wider where applicable
#' default: id, {item, resp}
#' if groups: id, group, {item, resp}
#' if covariates: id, cov1, cov2, ..., {item, resp}
#' if group_covariates: id, group, gcov1, gcov2, ..., {item, resp}
#' if raters: id,  rater, {item, resp}
#' if rater_covariates: id, rater, rcov1, rcov2, ..., {item, resp}
#' if process_data: id, {item, {resp, process}} (if pivoting wide each item would have a process column, all of which would come after the respective item resp columns)
#' if longitudinal: id, {item, {resp, time}} (if sem, lavaan time is combined with item pivoting wide)
#' if qmatrix: id, item, resp, q1, q2, ...
#' if item_groups: id,  item_groups, {item, resp}
#' additional covariates always go last
#' Parameter specification if user input requests are conflicting
#' id, item, resp, group, raters, process, longitudinal, item_groups, qmatrix,  covariates, group_covariates, rater_covariates
#' Current package functions ---------------------------------------------------
#' mirt
#'  - mirt::mirt
#'  - mirt::mirt.model
#'

##
## Arguments -------------------------------------------------------------------
## mirt: wide format with each item as a column and covdata as a separate dataframe
## mixedmirt: has data and covdata as a separate data.frame
## lavaan: very wide format with each item, date, group, etc. as a separate column
## sem: similar to lavaan, but only one "group" column which is a factor
## psych: wide format with each item as a column for 'fa'.
## ltm: wide format with each item as a column
## mokken: wide format with each item as a column
## lme4: long format with each item as a row
# covariates = ,
# groups = ,
# group_covariates = ,
# item_groups = ,
# rt = ,
# raters = ,
# qmatrix = ,
# rater_covariates = ,
# longitudinal = ,
# facts2dummies = ,
# item_prefix = "item_",


#' @importFrom dplyr as_tibble mutate select across pivot_longer left_join uncount everything pivot_wider
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stats model.matrix
#' @importFrom tidyselect all_of matches
#' @export

reformat = function(data,
                    package = "mirt",
                    groups = NULL,
                    covariates = NULL,
                    group_covariates = NULL,
                    item_groups = NULL,
                    process_data = NULL,
                    raters = NULL,
                    rater_covariates = NULL,
                    qmatrix = NULL,
                    longitudinal = NULL,
                    facts2dummies = NULL,
                    item_prefix = "item_",
                    return_obj = "tibble",
                    return_options = NULL) {
  data = as_tibble(data)
  
  # Define supported packages
  package_options = c("mirt", "lavaan", "sem", "psych", "ltm", "mokken", "lme4")
  return_obj_options = c("tibble",
                         "data.frame",
                         "data.matrix",
                         "matrix",
                         "model.matrix")
  required_cols = c("id", "item", "resp")
  
  if (!package %in% package_options) {
    stop(
      "Package not supported. Please choose from: ",
      paste(package_options, collapse = ", ")
    )
  }
  
  if (!return_obj %in% return_obj_options) {
    warning("Return object not supported. Returning as tibble.")
    return_obj = "tibble"
  }
  
  # Standardize column names to lowercase
  colnames(data) = tolower(colnames(data))
  
  # Check for required columns
  if (!all(required_cols %in% colnames(data))) {
    stop("Data must contain columns: ",
         paste(required_cols, collapse = ", "))
  }
  # Initialize a list to keep track of used columns
  used_columns = required_cols
  available_cols = colnames(data)[-which(colnames(data) %in% required_cols)]
  
  ## set up columns
  cov_cols = c()
  group_cols = c()
  group_cov_cols = c()
  item_group_cols = c()
  process_data_cols = c()
  rater_cols = c()
  qmatrix_cols = c()
  rater_cov_cols = c()
  longitudinal_cols = c()
  
  
  ## convert id to factor
  data = data |> mutate(id = as.factor(id))
  
  ref_cols = colnames(data)
  
  user_cols = c(
    covariates,
    groups,
    group_covariates,
    item_groups,
    process_data,
    raters,
    rater_covariates,
    qmatrix,
    longitudinal
  )
  
  missing_cols = check_col_presence(user_cols, ref_cols)
  available_cols = available_cols[-which(available_cols %in% missing_cols)]
  
  # Convert 'resp' to numeric if not already
  data = data |> mutate(resp = as.numeric(resp))
  
  # Check whether 'item' needs the item_prefix added (whether items are already prefixed or numeric)
  if (all(grepl("^\\d+$", unique(data$item)))) {
    item_prefix = item_prefix
  } else {
    item_prefix = ""
  }
  
  # Automatically identify group column if groups = TRUE
  if (!is.null(groups) & is.character(groups)) {
    # groups = groups
    group_col = grep(groups, available_cols, value = TRUE)
    used_columns = c(used_columns, group_col)
    
    # if (length(group_col) > 0) {
    #   data = data |> mutate(group = data[[group_col[1]]])
    #   used_columns = c(used_columns, group_col[1])
    # }
  } else if (isTRUE(groups)) {
    group_cols = data |> dplyr::select(matches("group|cluster|country|study|wave|treat")) |> names()
    used_columns = c(used_columns, group_cols)
    ## convert to factor
    # data = data |> mutate(group = as.factor(data[[group_col]]))
    
    
    # if (length(group_col) > 0) {
    #   data = data |> mutate(group = data[[group_col[1]]])
    #   used_columns = c(used_columns, group_col[1])
    # }
  }
  
  
  
  
  # Automatically identify covariate columns if covariates = TRUE
  if (!is.null(covariates) & is.character(covariates)) {
    used_columns = c(used_columns, covariates)
  } else if (isTRUE(covariates)) {
    covariate_cols = grep("cov_|age|gender|income|education",
                          colnames(data),
                          value = TRUE)
    if (length(covariate_cols) > 0) {
      used_columns = c(used_columns, covariate_cols)
    } else {
      warning("No covariate columns found.")
    }
  }
  
  
  
  # Automatically identify response time (rt) column if rt = TRUE or find the columns specified returning an error if columns specified are not in data
  if (isTRUE(rt)) {
    rt_col = grep("rt|response_time|process", colnames(data), value = TRUE)
    if (length(rt_col) > 0) {
      data = data |> mutate(rt = data[[rt_col[1]]])
      used_columns = c(used_columns, rt_col[1])
    }
  }
  
  # Automatically identify rater column if raters = TRUE
  if (isTRUE(raters) | (!is.null(raters))) {
    rater_col = grep("rater|judge|evaluator", colnames(data), value = TRUE)
    if (length(rater_col) > 0) {
      data = data |> mutate(rater = data[[rater_col[1]]])
      used_columns = c(used_columns, rater_col[1])
    }
  }
  
  # Automatically identify longitudinal column if longitudinal = TRUE
  if (isTRUE(longitudinal) | !is.null(longitudinal)) {
    longitudinal_col = grep("time|wave|session|visit|date", colnames(data), value = TRUE)
    if (length(longitudinal_col) > 0) {
      data = data |> mutate(time = data[[longitudinal_col[1]]])
      used_columns = c(used_columns, longitudinal_col[1])
    }
  }
  
  # Automatically identify qmatrix columns if qmatrix = TRUE
  if (isTRUE(qmatrix) | !is.null(qmatrix)) {
    qmatrix_cols = grep("qmatrix|q_matrix|skill|trait", colnames(data), value = TRUE)
    if (length(qmatrix_cols) > 0) {
      data = data |> mutate(qmatrix = data[[qmatrix_cols[1]]])
      used_columns = c(used_columns, qmatrix_cols[1])
    }
  }
  
  # Automatically identify item group columns if item_groups = TRUE
  if (isTRUE(item_groups) | !is.null(item_groups)) {
    item_group_cols = grep("item_group|item_grouping|item_group_id",
                           colnames(data),
                           value = TRUE)
    if (length(item_group_cols) > 0) {
      data = data |> mutate(item_group = data[[item_group_cols[1]]])
      used_columns = c(used_columns, item_group_cols[1])
    }
  }
  
  # Automatically identify group covariate columns if group_covariates = TRUE
  if (isTRUE(group_covariates) | !is.null(group_covariates)) {
    group_cov_cols = grep("group_cov|group_covariate|group_covariate_id",
                          colnames(data),
                          value = TRUE)
    if (length(group_cov_cols) > 0) {
      data = data |> mutate(group_covariate = data[[group_cov_cols[1]]])
      used_columns = c(used_columns, group_cov_cols[1])
    }
  }
  
  
  data = data |> dplyr::select(dplyr::all_of(used_columns))
  
  
  
  
  # Convert specified factor columns to dummy variables if facts2dummies = TRUE
  if (isTRUE(facts2dummies)) {
    factor_cols = names(Filter(is.factor, data))
    for (col in factor_cols) {
      data = data |>
        mutate(!!col := as.numeric(as.factor(data[[col]])))
      used_columns = c(used_columns, col)
    }
  }
  
  # Pivot data based on the package requirements
  if (package == "mirt") {
    # For mirt, wide format with each item as a column
    formatted_data = data |>
      pivot_wider(
        names_from = item,
        values_from = resp,
        names_prefix = item_prefix
      ) |> column_to_rownames(var = "id")
  } else if (package == "lavaan" || package == "sem") {
    # lavaan/sem typically uses long format
    formatted_data = data
  } else if (package %in% c("psych", "ltm")) {
    # psych and ltm often use wide format
    formatted_data = data |>
      pivot_wider(
        names_from = item,
        values_from = resp,
        names_prefix = item_prefix
      ) |> select(id, everything())
  } else if (package == "mokken") {
    # mokken uses wide format without prefixes
    formatted_data = data |>
      pivot_wider(names_from = item, values_from = resp) |>
      select(id, everything())
  } else if (package == "lme4") {
    # lme4 expects long format for mixed models
    formatted_data = data
  }
  
  # Warn about columns that were dropped
  dropped_columns = setdiff(colnames(data), used_columns)
  if (length(dropped_columns) > 0) {
    warning("The following columns were dropped: ",
            paste(dropped_columns, collapse = ", "))
  }
  
  # Return formatted data based on return_obj specification
  if (return_obj == "tibble") {
    return(as_tibble(formatted_data))
  } else if (return_obj == "data.frame") {
    return(as.data.frame(formatted_data))
  } else if (return_obj == "data.matrix") {
    return(data.matrix(formatted_data))
  } else if (return_obj == "matrix") {
    return(as.matrix(formatted_data))
  } else if (return_obj == "model.matrix") {
    return(model.matrix(~ . - 1, data = formatted_data))
  } else {
    stop(
      "Invalid return_obj specified. Choose from 'tibble', 'data.frame', 'matrix', or 'model.matrix'."
    )
  }
}



## check if any of the parameters are not NULL and have character values, if so, convert to lower case and check to make sure they are in the data
check_col_presence = function(x, ref_cols) {
  if (!is.null(x) & is.character(x)) {
    x = tolower(x)
    if (!all(x %in% ref_cols)) {
      missing = setdiff(x, ref_cols)
      warning(
        "Columns specified must be in the data. The following are missing and will be dropped: ",
        paste(missing, collapse = ", ")
      )
    }
  }
}