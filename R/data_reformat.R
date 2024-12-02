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
#' Currently supported packages:
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


# var_roles list is used to create a catalog object to keep track of and catalog all the variables in the tibble -------

var_roles = list(
  resp = list(
    dtype = numeric,
    desc = "response variable",
    expected = "resp",
    grep = NULL,
    multiple_allowed = F,
    priority = 1,
    required = T,
    support_to_role = NULL
  ),
  id = list(
    dtype = factor,
    desc = "subject identifier",
    expected = "id",
    grep = NULL,
    multiple_allowed = T,
    priority = 2,
    required = T,
    support_to_role = NULL
  ),
  item = list(
    dtype = factor,
    desc = "item identifier",
    expected = "item",
    grep = NULL,
    multiple_allowed = T,
    priority = 3,
    required = T,
    support_to_role = NULL
  ),
  groups = list(
    dtype = factor,
    desc = "grouping variable for subject",
    expected = "group",
    grep = "group|cluster|country|study|block|treat|sch",
    multiple_allowed = T,
    priority = 4,
    required = F,
    support_to_role = c("id")
  ),
  timedate = list(
    dtype = \(x) as_factor(x, ordered = T),
    desc = "longitudinal or date variable",
    expected = "date",
    grep = "wave|session|visit|date|time",
    multiple_allowed = T,
    priority = 5,
    required = F,
    support_to_role = c("id", "item")
  ),
  covariates = list(
    dtype = \(x) ifelse(is.character(x), as_factor(x), as.numeric(x)),
    desc = "covariates for the individual/subject",
    expected = "cov",
    grep = "cov_|age|gender|income|education",
    multiple_allowed = T,
    priority = 6,
    required = F,
    support_to_role = "id"
  ),
  levels = list(
    dtype = factor,
    desc = "level of the grouping variable for hierarchical models",
    expected = "level",
    grep = "level|sublevel|region|country|state|city|school|class|group",
    multiple_allowed = T,
    priority = 7,
    required = F,
    support_to_role = c("id", "item", "groups")
  ),
  rt = list(
    dtype = numeric,
    desc = "response time variable",
    expected = "rt",
    grep = "rt|response_time|process|time|duration|latency|speed|reaction",
    multiple_allowed = T,
    priority = 8,
    required = F,
    support_to_role = NULL
  ),
  qmatrix = list(
    dtype = factor,
    desc = "Q-matrix for item response theory models",
    expected = "qmatrix",
    grep = "qmatrix|q_matrix|skill|trait|factor|domain|category|dimension|latent|construct|ability|knowledge|competence",
    multiple_allowed = T,
    priority = 9,
    required = F,
    support_to_role = c("item")
  ),
  item_groups = list(
    dtype = factor,
    desc = "grouping variable for items",
    expected = "item_group",
    grep = "subtest|pretest|block|item_group|test|form|block|section|part|group|cluster|factor|trait|skill|domain|category|dimension",
    multiple_allowed = T,
    priority = 10,
    required = F,
    support_to_role = c("item", "qmatrix")
  ),
  group_covariates = list(
    dtype = numeric,
    desc = "covariates for the group",
    expected = "group_cov",
    grep = "group_cov|group|block|cluster|country|study|wave|treat|sch",
    multiple_allowed = T,
    priority = 11,
    required = F,
    support_to_role = c("group", "level", "id")
  ),
  raters = list(
    dtype = factor,
    desc = "rater variable",
    expected = "rater",
    grep = "rater|judge|evaluator|coder|observer|teacher|scorer|reader",
    multiple_allowed = T,
    priority = 12,
    required = F,
    support_to_role = NULL
  ),
  rater_covariates = list(
    dtype = numeric,
    desc = "covariates for the rater",
    expected = "rater_cov",
    grep = "rater_cov|rater_",
    multiple_allowed = T,
    priority = 13,
    required = F,
    support_to_role = "rater"
  ),
  other = list(
    dtype = \(x) x,
    desc = "other variables",
    expected = NULL,
    # and any char returned for grep
    grep = ".*",
    multiple_allowed = T,
    priority = 100,
    required = F,
    support_to_role = NULL
  )
)

## The supported packages list will be used to create data templates for more complex requests in future iterations -------
## The suported funcs list will be used to create data templates for more complex requests in future iterations
supported_funcs = list(
  mirt = list(
    mirt = list(
      call = "mirt::mirt",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(covdata = "a data.frame of data used for latent regression models", itemdesign = "data.frame with rows equal to the number of items and columns containing any item-design effects. rownames must be defined and matched with colnames in the data input."),
      func_support = T,
      other_output_support = F
      
    ),
    mixedmirt = list(
      call = "mirt::mixedmirt",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(covdata = "data.frame that consists of the nrow(data) by K ’person level’ fixed and random predictors", itemdesign = "data.frame object used to create a design matrix for the items, where each nrow(itemdesign) == nitems and the number of columns is equal to the number of fixed effect predictors (i.e., item intercepts)."),
      func_support = F,
      other_output_support = F
    ),
    bfactor = list(
      call = "mirt::bfactor",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(group = "a factor variable indicating group membership used for multiple group analyses"),
      func_support = F,
      other_output_support = F
    ),
    multipleGroup = list(
      call = "mirt::multipleGroup",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(group = "a factor variable indicating group membership used for multiple group analyses"),
      func_support = F,
      other_output_support = F
    ),
    mdirt = list(
      call = "mirt::mdirt",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(
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
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    ),
    gpcm = list(
      call = "ltm::gpcm",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    ),
    grm = list(
      call = "ltm::grm",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    ),
    rcor.test  = list(
      call = "ltm::rcor.test ",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    ),
    tpm = list(
      call = "ltm::tpm",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    )
  ),
  psych = list(
    alpha = list(
      call = "psych::alpha",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = F,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    ),
    bigCor = list(
      call = "psych::bigCor",
      expect_format = "wide",
      var_roles = c("any"),
      id_as_row_names = T,
      resp_as_int = F,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    ),
    fa = list(
      call = "psych::fa",
      expect_format = "wide",
      var_roles = c(
        "resp",
        "id",
        "item",
        "covariates",
        "groups",
        "levels",
        "group_covariates",
        "timedate"
      ),
      id_as_row_names = T,
      resp_as_int = F,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    ),
    irt.fa = list(
      call = "psych::irt.fa",
      expect_format = "wide",
      var_roles = c(
        "resp",
        "id",
        "item",
        "covariates",
        "groups",
        "levels",
        "group_covariates",
        "timedate"
      ),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    ),
    omega = list(
      call = "psych::omega",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    ),
    omegah = list(
      call = "psych::omegah",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    ),
    omegaSem = list(
      call = "psych::omegaSem",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    )
    
  ),
  lavaan = list(
    any = list(
      call = "lavaan",
      expect_format = "long",
      var_roles = c("any"),
      id_as_row_names = T,
      resp_as_int = F,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    )
  ),
  sem = list(
    sem = list(
      call = "sem::sem",
      expect_format = "long",
      var_roles = c("any"),
      #  the factor given as the group argument is used to split the data into groups
      id_as_row_names = T,
      resp_as_int = F,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    )
  ),
  lme4 = list(
    lmer = list(
      call = "lme4::lmer",
      expect_format = "long",
      var_roles = c("any"),
      id_as_row_names = F,
      resp_as_int = F,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(),
      func_support = F,
      other_output_support = F
    )
  ),
  mokken = list(
    check.monotonicity = list(
      call = "mokken::check.monotonicity",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      ## can handle respondent clustering
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(level.two.var = "vector of length nrow(X) or matrix with number of rows equal to nrow(X) that indicates the level two variable for nested data")
    ),
    aisp = list(
      call = "mokken::aisp",
      expect_format = "wide",
      var_roles = c("resp", "id", "item"),
      ## can handle respondent clustering
      id_as_row_names = T,
      resp_as_int = T,
      as_args_list = F,
      ## currently not supported
      args_list_outputs = list(level.two.var = "vector of length nrow(X) or matrix with number of rows equal to nrow(X) that indicates the level two variable for nested data")
    )
    
  )
)

piv_wide_pkg = list(
  mirt = T,
  lavaan = F,
  sem = F,
  psych = T,
  ltm = T,
  mokken = T,
  lme4 = F
)

cov_wide_supps = list(
  mirt = F,
  lavaan = T,
  sem = T,
  psych = T,
  ltm = F,
  mokken = F,
  lme4 = T
)


not_supported_cols = c("rater", "raters", "rater_covariates", "rt") ## currently not supported


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
                    keep_all = F,
                    facts2dummies = NULL,
                    as_args_list = F,
                    ## currently not supported
                    drop_na_vals = F,
                    item_prefix = "item_",
                    sep = "_",
                    return_obj = "tibble",
                    return_options = NULL) {
  if (!is_tibble(data)) {
    data <- as_tibble(data)
  }
  if (package == "lme4") {
    keep_all = T
  }
  package_options = names(supported_funcs)
  if (!package %in% package_options) {
    stop(
      paste0(
        "The package '",
        package,
        "' is not currently supported. Supported packages are: ",
        paste(package_options, collapse = ", ")
      )
    )
  }
  
  return_obj_options = c("tibble", "data.frame", "matrix")
  
  required_cols = c(id, item, resp)
  
  # Create a catalog object to keep track of and catalog all the variables in the tibble
  catalog <- list()
  catalog_names <- list()
  catalog_names$original_names <- names(data)
  data <- data |> irw_rename()
  catalog_names$cleaned_names <- names(data)
  # Check to ensure the identified columns “id”, “item”, and “resp” are present in the tibble (if not it will return an error)
  if (!all(c(id,item, resp) %in% names(data))) {
    stop("The columns 'id', 'item', and 'resp' must be present in the data")
  }
  
  # Function to add variables to the catalog and convert them to the appropriate data type
  add_to_catalog <- function(catalog = catalog, var_name, role) {
    catalog[[var_name]] <- list(
      name = var_name,
      role = role,
      dtype = var_roles[[role]]$dtype,
      priority = var_roles[[role]]$priority
    )
    catalog
  }
  
  
  # Convert resp to numeric if necessary
  data <- data |> check_resp(resp, item)
  
  
  catalog <- add_to_catalog(catalog, resp, "resp")
  catalog <- add_to_catalog(catalog, id, "id")
  catalog <- add_to_catalog(catalog, item, "item")
  
  
  # Add variables with id and item roles to catalog, converting their data types appropriately
  data <- data |> mutate(id = catalog[["id"]]$dtype(id))
  data <- data |> mutate(item = catalog[["item"]]$dtype(item))
  
  
  if (keep_all) {
    user_specified_char_columns_found_in_args = names(data)[!names(data) %in% c("id", "item", "resp")]
  } else {
    user_specified_char_columns_found_in_args = character(0)
    applicable_args = c(
      groups,
      covariates,
      levels,
      group_covariates,
      item_groups,
      rt,
      raters,
      rater_covariates,
      qmatrix,
      timedate
    )
    
    for (arg_value in applicable_args) {
      if (is.character(arg_value)) {
        user_specified_char_columns_found_in_args <- c(user_specified_char_columns_found_in_args,
                                                       arg_value)
      }
    }
  }
  
  # Add all other variables indicated in args to the catalog
  # for each role in var_roles, check if the role is in args, if so, add to catalog
  # This loop iterates over the names of the elements in the 'var_roles' list.
  # 'var_roles' is assumed to be a predefined global variable containing role information.
  # Each iteration processes one role from 'var_roles', done in order of priority within var_roles (assuming resp, item, and id have already been added).
  remaining_roles_in_order_of_priority <- names(var_roles)[!names(var_roles) %in% c("resp", "id", "item", "other")]
  for (role in remaining_roles_in_order_of_priority) {
    # if corresponding argument is not NULL, add to catalog
    role_col <- eval(parse(text = role))
    if (!is.null(role_col)) {
      # if character is in not_supported_cols, return an error
      if (role %in% not_supported_cols) {
        stop(paste0("Arguments for '", role, "' are not currently supported"))
      }
      # if the role is a character (or character vector) and not in the not_supported_cols list
      # check if the role is in the data, if not, return an error
      if (is.character(role_col)) {
        if (!all(role_col %in% names(data))) {
          stop(paste0("The column '", role_col, "' must be present in the data"))
        }
        
        # check if the variable name has already been used in the catalog by a higher priority var_role (if check fails, do not add variable to catalog informative warning  about which have already been used)
        for (r in c(role_col)) {
          if (r %in% names(catalog)) {
            ## warning states if variable has already been used and if so which role it has been used for
            warning(
              paste0(
                "The column '",
                role_col,
                "' has already been used in the catalog for the role '",
                catalog[[role_col]]$role,
                "'"
              )
            )
          } else {
            catalog <- add_to_catalog(catalog, r, role)
            data <- data |> mutate(across(all_of(r), catalog[[r]]$dtype))
          }
        }
        # add the role to the catalog
      } else if (isTRUE(role_col) |
                 keep_all) {
        # if the role is boolean and true,
        pat = var_roles[[role]]$grep
        candidate_cols = names(data)[grepl(pat, names(data))]
        # remove any columns that have already been used in the catalog
        candidate_cols <- setdiff(candidate_cols, names(catalog))
        # remove any columns that have been specified in the args
        candidate_cols <- setdiff(candidate_cols,
                                  user_specified_char_columns_found_in_args)
        for (col in candidate_cols) {
          catalog <- add_to_catalog(catalog, col, role)
          data <- data |> mutate(across(all_of(col), catalog[[col]]$dtype))
        }
      }
    }
  }
  
  ## if keep_all is true, add all columns not already in the catalog to the catalog
  if (keep_all) {
    remaining_cols <- setdiff(names(data), names(catalog))
    for (col in remaining_cols) {
      catalog <- add_to_catalog(catalog, col, "other")
      data <- data |> mutate(across(all_of(col), catalog[[col]]$dtype))
      
    }
  }
  
  
  # create copy of data with only the columns in the catalog
  data_cleaned <- data[, names(catalog)]
  
  # Check whether 'item' needs the item_prefix added (whether items are already prefixed or numeric)
  if (all(grepl("^\\d+$", unique(data$item)))) {
    item_prefix = item_prefix
  } else {
    item_prefix = ""
  }
  
  # check if output format is wide
  if (package %in% names(supported_funcs)) {
    if (piv_wide_pkg[[package]]) {
      # check if uni que identification is possible with id, item, and resp
      # get id_cols by finding all variables with id role in catalog
      id_cols <- names(catalog)[sapply(catalog, function(x)
        x$role == "id")]
      # get names_from by finding all variables with item role in catalog
      names_from <- names(catalog)[sapply(catalog, function(x)
        x$role == "item")]
      
      
      # find pivot arguments try first from data_cleaned and if not possible, try to find from data
      pivot_args <- NULL
      try({
        pivot_args <- find_pivot_args(data_cleaned, id_cols, names_from, resp, catalog = catalog)
      }, silent = TRUE)
      if (is.null(pivot_args)) {
        pivot_args <- find_pivot_args(data, id_cols, names_from, resp, catalog = catalog)
        data_cleaned <- data[, c(names(catalog), setdiff(as.character(unlist(
          pivot_args, use.names = F
        )), names(catalog)))]
      }
      
      ## print notification if pivot_args need to be combined
      if (needs_combined_columns(pivot_args)) {
        notify_combined_columns(pivot_args)
      }
      
      # pivot data
      data_formatted <- data_cleaned |> pivot_wider(
        names_from = pivot_args$names_from,
        values_from = pivot_args$values_from,
        id_cols = pivot_args$id_cols,
        names_prefix = item_prefix,
        names_sep = sep
      )
      
      unused_vars <- setdiff(names(data_cleaned), as.character(unlist(pivot_args, use.names = F)))
      ## issue warning if cov_wide_supps[[package]] is false and there are other unused variables in the catalog and state which package does not support them and which variables will be ignored
      
      if ((length(unused_vars) > 0) & !cov_wide_supps[[package]]) {
        warning(
          paste0(
            "The package '",
            package,
            "' does not support the following variables: ",
            paste(unused_vars, collapse = ", ")
          )
        )
      }
      # if there are other unused variables in the catalog and if any of the supported methods for the package have var_roles of the unused variables, add them to the data by joining them back to the data_cleaned
      if ((length(unused_vars) > 0) & cov_wide_supps[[package]]) {
        tmpdata = data_cleaned[, c(unused_vars, pivot_args$id_cols)] |> distinct()
        data_formatted <- data_formatted |> left_join(tmpdata, by = pivot_args$id_cols)
        
      }
      ## combine any id_cols to create unique rownames and then drop them
      data_formatted <- data_formatted |>
        unite("rowid",
              pivot_args$id_cols,
              sep = sep,
              remove = T) |>
        column_to_rownames("rowid")
      
    } else {
      data_formatted <- data_cleaned |> as_tibble()
    }
  } else {
    stop("The specified package is not supported")
  }
  ## check if any columns in dataformatted need to be dropped due to NAs and drop them with a warning message
  cols_to_drop <- colnames(data_formatted)[colSums(is.na(data_formatted)) == nrow(data_formatted)]
  if (length(cols_to_drop) > 0) {
    warning(paste0(
      "The following columns have been dropped due to all missing values: ",
      paste(cols_to_drop, collapse = ", ")
    ))
    data_formatted <- data_formatted |> select(-all_of(cols_to_drop))
  }
  
  rows_to_drop <- rownames(data_formatted)[rowSums(is.na(data_formatted)) == ncol(data_formatted)]
  if (length(rows_to_drop) > 0) {
    warning(paste0(
      "A total of ",
      length(rows_to_drop),
      " rows have been dropped due to all  missing values"
    ))
    data_formatted <- data_formatted |>  filter(!rownames(data_formatted) %in% rows_to_drop)
  }
  
  
  ## if psych package, convert factors to numeric if ordered, else convert to dummies
  if (package == "psych") {
    for (col in names(data_formatted)) {
      ## first check if the column has only one value and if so, drop it
      if (one_value_check(data_formatted[[col]])) {
        data_formatted <- data_formatted[, !(names(data_formatted) %in% col)]
        
      } else if (class(data_formatted[[col]]) %in% c("factor", "ordered", "character", "logical")) {
        data_formatted <- check_resp(data_formatted, col)
        if (is.factor(data_formatted[[col]])) {
          dummy_cols <- psych::dummy.code(data_formatted[[col]], na.rm = T)
          data_formatted <- cbind(data_formatted, dummy_cols)
          # cbind(data_formatted, dummy_cols)
          data_formatted <- data_formatted[, !(names(data_formatted) %in% col)]
        }
      }
    }
  }
  
  ## drop na rows if drop_na_vals is true or if the package is mokken
  if (drop_na_vals | package == "mokken") {
    data_formatted <- data_formatted |> drop_na()
  }
  
  # Return the formatted data
  if (return_obj == "tibble") {
    data_formatted <- as_tibble(data_formatted)
  } else if (return_obj == "data.frame") {
    data_formatted <- as.data.frame(data_formatted)
  } else if (return_obj == "matrix") {
    data_formatted <- as.matrix(data_formatted)
  } else {
    stop("Unsupported return object type")
  }
  
  # Add class to the formatted data
  class(data_formatted) <- c("irw_format", class(data_formatted))
  
  return(data_formatted)
  
  
  
}
