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
#' if rt: id, {item, {resp, process}} (if pivoting wide each item would have a process column, all of which would come after the respective item resp columns)
#' if timedate: id, {item, {resp, time}} (if sem, lavaan time is combined with item pivoting wide)
#' if qmatrix: id, item, resp, q1, q2, ...
#' if item_groups: id,  item_groups, {item, resp}
#' additional covariates always go last
#' Parameter specification if user input requests are conflicting
#' id, item, resp, group, raters, process, timedate, item_groups, qmatrix,  covariates, group_covariates, rater_covariates
#' Current package functions ---------------------------------------------------
#' mirt
#'  - mirt::mirt
#'  - mirt::mirt.model
#'

##
## Arguments -------------------------------------------------------------------
## mirt: wide format with each item as a column and covdata as a separate dataframe and `itemdesign` would be an "item group" vars
## mixedmirt: has data and covdata as a separate data.frame
## lavaan: very wide format with each item, date, group, etc. as a separate column
## sem: similar to lavaan, but only one "group" column which is a factor
## psych: wide format with each item as a column for 'fa'.
## ltm: wide format with each item as a column
## mokken: wide format with each item as a column
## lme4: long format with each item as a row


#' @importFrom dplyr as_tibble mutate select across pivot_longer left_join uncount everything pivot_wider
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stats model.matrix
#' @importFrom tidyselect all_of matches
#' @export

## The suported funcs list will be used to create data templates for more complex requests in future iterations
supported_funcs = list(
  mirt = list(
    mirt = list(
      call = "mirt::mirt",
      expect_format = "wide",
      supported_var_types = c("required"),
      id_as_row_names = T,
      resp_as_int = T,
      other_outputs = list(covdata = "a data.frame of data used for latent regression models", 
                           itemdesign = "data.frame with rows equal to the number of items and columns containing any item-design effects. rownames must be defined and matched with colnames in the data input."), 
                           func_support = T,
                           other_output_support = F
                           
      ),
      mixedmirt = list(
        call = "mirt::mixedmirt",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(covdata = "data.frame that consists of the nrow(data) by K ’person level’ fixed and random predictors", itemdesign = "data.frame object used to create a design matrix for the items, where each nrow(itemdesign) == nitems and the number of columns is equal to the number of fixed effect predictors (i.e., item intercepts)."), 
        func_support = F,
        other_output_support = F
      ),
      bfactor = list(
        call = "mirt::bfactor",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(group = "a factor variable indicating group membership used for multiple group analyses"), 
        func_support = F,
        other_output_support = F
      ),
      multipleGroup = list(
        call = "mirt::multipleGroup",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(group = "a factor variable indicating group membership used for multiple group analyses"), 
        func_support = F,
        other_output_support = F
      ),
      mdirt = list(
        call = "mirt::mdirt",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(
          covdata = "a data.frame of data used for latent regression models",
          item.Q = "a list of item-level Q-matrices indicating how the respective categories should be modeled by the underlying attributes. Each matrix must represent a Ki × A matrix, where Ki represents the number of categories for the ith item, and A is the number of attributes included in the Theta matrix; otherwise, a value ofNULL will default to a matrix consisting of 1’s for each Ki × A element except for the first row, which contains only 0’s for proper identification. Incidentally, the first row of each matrix must contain only 0’s so that the first category represents the reference category for identification",
          group = "a factor variable indicating group membership used for multiple group analyses"
        ), 
        func_support = F,
        other_output_support = F
      )
      
    ),
    ltm = list(
      rasch = list(
        call = "ltm::rasch",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      ),
      gpcm = list(
        call = "ltm::gpcm",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      ),
      grm = list(
        call = "ltm::grm",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      ),
      rcor.test  = list(
        call = "ltm::rcor.test ",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      ),
      tpm = list(
        call = "ltm::tpm",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      )
    ),
    psych = list(
      alpha = list(
        call = "psych::alpha",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = F,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      ),
      bigCor = list(
        call = "psych::bigCor",
        expect_format = "wide",
        supported_var_types = c("all"),
        id_as_row_names = T,
        resp_as_int = F,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      ),
      fa = list(
        call = "psych::fa",
        expect_format = "wide",
        supported_var_types = c("all"),
        id_as_row_names = T,
        resp_as_int = F,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      ),
      irt.fa = list(
        call = "psych::irt.fa",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      ),
      omega = list(
        call = "psych::omega",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      ),
      omegah = list(
        call = "psych::omegah",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      ),
      omegaSem = list(
        call = "psych::omegaSem",
        expect_format = "wide",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      )
      # ,
      # mlr = list(
      #   call = "psych::mlr",
      #   expect_format = "long",
      #   supported_var_types = c("any"),
      #   desc = " first four columns in the long output are id, time, values, and item names, the remaining columns are the extra values. These could be something such as a trait measure for each subject, or the situation in which the items are given",
      #   id_as_row_names = F,
      #   resp_as_int = F,
      #   other_outputs = list(), 
      #   func_support = F,
      #   other_output_support = F
      # )
      # 
      
    ),
    lavaan = list(
      any = list(
        call = "lavaan",
        expect_format = "long",
        supported_var_types = c("required"),
        id_as_row_names = T,
        resp_as_int = F,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      )
    ),
    sem = list(
      sem = list(
        call = "sem::sem",
        expect_format = "long",
        supported_var_types = c("required", "group"),
        #  the factor given as the group argument is used to split the data into groups
        id_as_row_names = T,
        resp_as_int = F,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      )
    ),
    lme4 = list(
      lmer = list(
        call = "lme4::lmer",
        expect_format = "long",
        supported_var_types = c("required"),
        id_as_row_names = F,
        resp_as_int = F,
        other_outputs = list(), 
        func_support = F,
        other_output_support = F
      )
    ),
    mokken = list(
      check.monotonicity = list(
        call = "mokken::check.monotonicity",
        expect_format = "wide",
        supported_var_types = c("required"),
        ## can handle respondent clustering
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(level.two.var = "vector of length nrow(X) or matrix with number of rows equal to nrow(X) that indicates the level two variable for nested data")
      ),
      aisp = list(
        call = "mokken::aisp",
        expect_format = "wide",
        supported_var_types = c("required"),
        ## can handle respondent clustering
        id_as_row_names = T,
        resp_as_int = T,
        other_outputs = list(level.two.var = "vector of length nrow(X) or matrix with number of rows equal to nrow(X) that indicates the level two variable for nested data")
      )
      
      
    )
)

pkg_wide_long = list(
  mirt = "wide",
  lavaan = "long",
  sem = "long",
  psych = "wide",
  ltm = "wide",
  mokken = "wide",
  lme4 = "long"
)

pkg_wide = list(
  mirt = T,
  lavaan = F,
  sem = F,
  psych = T,
  ltm = T,
  mokken = T,
  lme4 = F
)


var_roles = list(
  id = list(
    dtype = factor,
    desc = "subject identifier",
    expected = "id",
    grep = "id"),
  item = list(
    dtype = factor,
    desc = "item identifier",
    expected = "item",
    grep = "item"
    ),
  resp = list(
    dtype = numeric,
    desc = "response variable",
    expected = "resp",
    grep = "resp"
    ),
  groups = list(
    dtype = factor,
    desc = "grouping variable for subject",
    expected = "group",
    grep = "group|cluster|country|study|block|treat|sch"
  ),
  timedate = list(
    dtype = \(x) factor(x, ordered = T),
    desc = "longitudinal or date variable",
    expected = "date",
    grep = "wave|session|visit|date|time|year|month|day|hour"
  ),
  covariates = list(
    dtype = \(x) ifelse(is.character(x), factor(x), numeric(x)),
    desc = "covariates for the individual/subject",
    expected = "cov",
    grep = "cov_|age|gender|income|education"
  ),
  levels = list(
    dtype = factor,
    desc = "level of the grouping variable for hierarchical models",
    expected = "level",
    grep = "level|sublevel|region|country|state|city|school|class|group"
  ),
  rt = list(
    dtype = numeric,
    desc = "response time variable",
    expected = "rt",
    grep = "rt|response_time|process|time|duration|latency|speed|reaction"
  ),
  qmatrix = list(
    dtype = factor,
    desc = "Q-matrix for item response theory models",
    expected = "qmatrix",
    grep = "qmatrix|q_matrix|skill|trait|factor|domain|category|dimension|latent|construct|ability|knowledge|competence"
  ),
  item_groups = list(
    dtype = factor,
    desc = "grouping variable for items",
    expected = "item_group",
    grep = "subtest|pretest|block|item_group|test|form|block|section|part|group|cluster|factor|trait|skill|domain|category|dimension"
  ),
  group_covariates = list(
    dtype = numeric,
    desc = "covariates for the group",
    expected = "group_cov",
    grep = "group_cov|group|block|cluster|country|study|wave|treat|sch"
  ),
  raters = list(
    dtype = factor,
    desc = "rater variable",
    expected = "rater",
    grep = "rater|judge|evaluator|coder|observer|teacher|scorer|reader"
  ),
  rater_covariates = list(
    dtype = numeric,
    desc = "covariates for the rater",
    expected = "rater_cov",
    grep = "rater_cov|rater_"
  ),
  other = list(
    dtype = \(x) x,
    desc = "other variables",
    expected = NULL, # and any char returned for grep
    grep = ".*"
  )
  

)


## TODO: potential solutions for identifying and reorganizing irw data with conflicting names or bad datatypes
# df |> pivot_wider(id_cols = person_id, names_from = c(item,wave),values_from = resp, names_prefix = "i",names_sep = "_")
# df |> pivot_wider(id_cols = person_id, names_from = item, values_from = resp, names_prefix = "i",id_expand = T)

not_supported_cols = c("person_id","rater")



reformat = function(data,
                    package = "mirt",
                    id = "id",
                    item = "item",
                    resp = "resp",
                    groups = NULL,
                    covariates = NULL,
                    levels = NULL,
                    group_covariates = NULL,
                    item_groups = NULL,
                    rt = NULL,
                    raters = NULL,
                    rater_covariates = NULL,
                    qmatrix = NULL,
                    timedate = NULL,
                    facts2dummies = NULL,
                    as_args_list = F,
                    item_prefix = "item_",
                    return_obj = "tibble",
                    return_options = NULL) {
  if (!is_tibble(data)) {
    data <- as_tibble(data)
  }
  
  # Define supported packages
  package_options = c("mirt", "lavaan", "sem", "psych", "ltm", "mokken", "lme4")
  package_options = c("mirt", "lme4")
  
  return_obj_options = c("tibble",
                         "data.frame",
                         # "data.matrix",
                         "matrix"
                         # "model.matrix"
                         )
  # required_cols = c("id", "item", "resp")
  required_cols = c(id, item, resp)
  
  ## save user old columns for error messages
  old_cols = colnames(data)
  
  # Standardize column names to lowercase and fix column names
  data = irw_rename(data)
  
  if(any(not_supported_cols %in% colnames(data))){
    stop("The structure of this dataset is currently not supported. Try another dataset.")
  }
  
  ## create a list where the keys are the new column names and the values are the old column names
  col_map = setNames(old_cols, colnames(data))
  
  ## get data types
  dtypes =  lapply(data, typeof) |> unlist()
  
  ## perform checks on the user input
  ## check if the package is supported
  if (!package %in% package_options) {
    stop(
      "Package not supported. Please choose from: ",
      paste(package_options, collapse = ", ")
    )
  }
  
  ## check if the return object is supported
  if (!return_obj %in% return_obj_options) {
    warning("Return object not supported. Returning as tibble.")
    return_obj = "tibble"
  }
  
  # Check for required columns
  if (!all(required_cols %in% colnames(data))) {
    stop("Data must contain columns: ",
         paste(required_cols, collapse = ", "))
  }
  # Initialize a list to keep track of used columns
  used_columns = required_cols
  available_cols = colnames(data)[-which(colnames(data) %in% required_cols)]

  
  ## create a named list of the user input column types
  user_vars = list(
    covariates = list(old_cols = covariates),
    groups = list(old_cols = groups),
    group_covariates = list(old_cols = group_covariates),
    item_groups = list(old_cols = item_groups),
    rt = list(old_cols = rt),
    raters = list(old_cols = raters),
    rater_covariates = list(old_cols = rater_covariates),
    qmatrix = list(old_cols = qmatrix),
    timedate = list(old_cols = timedate)
  )
  

  ## drop any of the user vars that are null or empty
  user_vars = user_vars[sapply(user_vars, function(x) any(!is.null(x$old_cols) & x$old_cols != "" & x$old_cols != F)) |> unlist()]
  # print(user_vars)
  
  user_cols = c()
  user_cols_old = c()
  
  user_vars = lapply(user_vars, function(x) {
    if (!is.null(x$old_cols) & is.character(x$old_cols)) {
      x$cols = x$old_cols |> irw_rename()
      user_cols = c(user_cols, x$cols)
      user_cols_old = c(user_cols_old, x$old_cols)
    } else if (isTRUE(x$old_cols)) {
      x$cols = T
    } else {
      x$cols = NULL
    }
    x
  })
  
  ## if user submitted any vars (nonempty nonboolean), check if they are in the dataset provided
  
  

  # user_cols = unlist(lapply(user_vars, function(x) x$cols))
  # user_cols_old = unlist(lapply(user_vars, function(x) x$old_cols))
  
  if (any(!(user_cols %in% colnames(data)))) {
    missing_cols_old = user_cols_old[!(user_cols %in% colnames(data))]
    cli::cli_abort(c(
      x = "The following elements are not found in the data:",
      "*" = "{.and {.field {missing_cols_old}}}."
    ))
  }
  
  ## check if the user input columns are in the data
  # missing_cols = check_col_presence(user_cols, colnames(data))
  missing_cols = user_cols[!(user_cols %in% colnames(data))]
  if (length(missing_cols) > 0) {
    available_cols = available_cols[-which(available_cols %in% missing_cols)]
  }


  ## add element to user_vars to keep track of the used columns for each var group in list
  for (i in names(user_vars)) {
    user_vars[[i]]$avail_cols = user_vars[[i]]$cols[which(user_vars[[i]]$cols %in% available_cols)] 
    if(length(user_vars[[i]]$avail_cols) > 0 & !is.null(user_vars[[i]]$cols) & is.character(user_vars[[i]]$avail_cols)){
      used_columns = c(used_columns, user_vars[[i]]$avail_cols)
    } else if (isTRUE(user_vars[[i]]$cols)){
      pat = variable_roles[[i]][["grep"]]
      # print(pat)
      user_vars[[i]]$avail_cols = grep(pat, available_cols, value = TRUE)
      used_columns = c(used_columns, user_vars[[i]]$avail_cols)
    }
    
  }

  # for(var in user_vars){
  #   if(length(var$avail_cols) > 0){
  #     used_columns = c(used_columns, var$avail_cols)
  #   }
  # }
  
  # print(used_columns)
  # vars = prepend(list())
  
  
  # ## set up columns
  # cov_cols = c()
  # group_cols = c()
  # group_cov_cols = c()
  # item_group_cols = c()
  # rt_cols = c()
  # rater_cols = c()
  # qmatrix_cols = c()
  # rater_cov_cols = c()
  # timedate_cols = c()
  # 
  
  

  
  # Check whether 'item' needs the item_prefix added (whether items are already prefixed or numeric)
  if (all(grepl("^\\d+$", unique(data$item)))) {
    item_prefix = item_prefix
  } else {
    item_prefix = ""
  }
  
  
  ## Perform transformations according to the template
  # Convert 'resp' to numeric if not already
  if(!is.numeric(data$resp)){
    data = data |> mutate(resp = as.numeric(resp))
  }
    
  ## convert id to factor if not already
  if(!is.factor(data$id)){
    data = data |> mutate(id = as_factor(id))
  }
  
  ## convert item to factor if not already
  if(!is.factor(data$item)){
    data = data |> mutate(item = as_factor(item))
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
    print(data)
    # For mirt, wide format with each item as a column
    formatted_data = data |>
      # dplyr::select(id, item, resp) |>
      pivot_wider_irw(
        # names_from = item,
        # values_from = resp,
        names_prefix = item_prefix
      ) 
    # |> 
    #   column_to_rownames(var = "id")
  } else if (package == "lavaan" || package == "sem") {
    # lavaan/sem typically uses long format
    formatted_data = data
  } else if (package %in% c("psych", "ltm")) {
    # psych and ltm often use wide format
    formatted_data = data |>
      pivot_wider_irw(      
      # pivot_wider(
        names_from = item,
        values_from = resp,
        names_prefix = item_prefix
      ) 
    # |> dplyr::select(id, everything())
  } else if (package == "mokken") {
    # mokken uses wide format without prefixes
    formatted_data = data |>
      pivot_wider_irw(
        names_from = item,
        values_from = resp,
        names_prefix = item_prefix
        )
      # pivot_wider(names_from = item, values_from = resp) |>
      # dplyr::select(id, everything())
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