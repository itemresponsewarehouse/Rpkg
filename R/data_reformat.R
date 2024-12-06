# Functions for in-session data transformations
# These functions are used to transform data in-session, for example, to clean or reshape data before analysis.

#' var_roles list is used to create a catalog object to keep track of and catalog all the variables in the tibble
#' @format A list of lists

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
    dtype = \(x) forcats::as_factor(x),
    desc = "longitudinal or date variable",
    expected = "date",
    grep = "wave|session|visit|date|time",
    multiple_allowed = T,
    priority = 5,
    required = F,
    support_to_role = c("id", "item")
  ),
  covariates = list(
    dtype = ~ if(is.character(.)) forcats::fct(.,na="NA") else as.numeric(.),
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
      args_list_outputs = list(covdata = "data.frame that consists of the nrow(data) by K person level fixed and random predictors", itemdesign = "data.frame object used to create a design matrix for the items, where each nrow(itemdesign) == nitems and the number of columns is equal to the number of fixed effect predictors (i.e., item intercepts)."),
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
        item.Q = "a list of item-level Q-matrices indicating how the respective categories should be modeled by the underlying attributes. Each matrix must represent a Ki x A matrix, where Ki represents the number of categories for the ith item, and A is the number of attributes included in the Theta matrix; otherwise, a value of NULL will default to a matrix consisting of 1s for each Ki x A element except for the first row, which contains only 0s for proper identification. Incidentally, the first row of each matrix must contain only 0s so that the first category represents the reference category for identification",
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
      func_support = T,
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
    efa = list(
      call = "efa",
      expect_format = "wide",
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
  # sem = list(
  #   rawMoments = list(
  #     call = "sem::rawMoments",
  #     expect_format = "long",
  #     var_roles = c("any"),
  #     #  the factor given as the group argument is used to split the data into groups
  #     id_as_row_names = T,
  #     resp_as_int = F,
  #     as_args_list = F,
  #     ## currently not supported
  #     args_list_outputs = list(),
  #     func_support = F,
  #     other_output_support = F
  #   )
  # ),
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
  lavaan = T,
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
#' @param data data.frame, tibble, or matrix: The data to be reformatted. The data should have names compatible with IRW nomenclature (see details).
#' @param package character: The name of the package for which the data should be reformatted. Currently supported packages are: mirt, lavaan, psych, ltm, mokken, and lme4. (see details)
#' @param id character: The name or names of the variables/column(s) in the data to be used in the role of subject identifier during reformatting. Default is "id".
#' @param item character: The name or names of the variables/column(s) in the data to be used in the role of item identifier during reformatting. Default is "item".
#' @param resp character: The name of the variable/column in the data to be used in the role of response variable. Default is "resp". Currently only one response variable is supported.
#' @param groups character or boolean: The name(s) of the variables/columns in the data to be used in the role of grouping subject-level observations OR a boolean where `TRUE` asks the function to attempt to automatically identify grouping variables. Default is NULL. (see details)
#' @param timedate character or boolean: The name(s) of the variables/columns in the data to be used in the role of longitudinal, session, or date-time OR a boolean where `TRUE` asks the function to attempt to automatically identify longitudinal, session, or date-time variables. Default is NULL. (see details)
#' @param covariates character or boolean: The name(s) of the variables/columns in the data to be used in the role of covariates for the individual/subject OR a boolean where `TRUE` asks the function to attempt to automatically identify covariates. Default is NULL. (see details)
#' @param levels character or boolean: The name(s) of the variables/columns in the data to be used in the role of level of the grouping variable for hierarchical models OR a boolean where `TRUE` asks the function to attempt to automatically identify levels. Default is NULL. (see details)
#' @param rt character or boolean: The name(s) of the variables/columns in the data to be used in the role of response time variable OR a boolean where `TRUE` asks the function to attempt to automatically identify response time variables. Default is NULL. (see details)
#' @param qmatrix character or boolean: The name(s) of the variables/columns in the data to be used in the role of Q-matrix for item response theory models OR a boolean where `TRUE` asks the function to attempt to automatically identify Q-matrix variables. Default is NULL. (see details)
#' @param item_groups character or boolean: The name(s) of the variables/columns in the data to be used in the role of grouping variable for items OR a boolean where `TRUE` asks the function to attempt to automatically identify item group variables. Default is NULL. (see details)
#' @param group_covariates character or boolean: The name(s) of the variables/columns in the data to be used in the role of covariates for the group OR a boolean where `TRUE` asks the function to attempt to automatically identify group covariate variables. Default is NULL. (see details)
#' @param raters character or boolean: The name(s) of the variables/columns in the data to be used in the role of rater variable OR a boolean where `TRUE` asks the function to attempt to automatically identify rater variables. Default is NULL. (see details)
#' @param rater_covariates character or boolean: The name(s) of the variables/columns in the data to be used in the role of covariates for the rater OR a boolean where `TRUE` asks the function to attempt to automatically identify rater covariate variables. Default is NULL. (see details)
#' @param keep_all boolean: If `TRUE`, the function will attempt to keep all columns in the data and identify the variable roles, regardless of whether they are used in the reformatting process. Default is `FALSE`.(see details)
#' @param facts2dummies character: The name(s) of the variables/columns in the data to be converted to dummy variables. Default is NULL. (see details)
#' @param as_args_list boolean: Does nothing. Currently not supported. Default is `FALSE`.
#' @param drop_na_vals boolean: If `TRUE`, the function will drop rows with missing values before returning. Default is `FALSE`.
#' @param item_prefix character: The prefix to be added to the item names when pivoting. Default is "item_".
#' @param sep character: The separator to be used when combining variables in the data. Default is "_".
#' @param return_obj character: The format in which the data should be returned. Options are "tibble", "data.frame", or "matrix". Default is "tibble".
#' @param return_options list: Currently does nothing. A list of additional options to be passed to the return object. Default is NULL. (see details)
#' @return 
#' An object of class `irw_format` inheriting from either a data frame, tibble, or matrix in the format required by the specified package. 
#' 
#' @examples
#' ## Example 1: Reformat data for mirt package
#' df = data.frame(
#' id = rep(rep(1:3,3),2),
#' item = rep(rep(letters[1:3],each=3),2),
#' resp = sample(0:1,9*2,replace=TRUE),
#' cov_1 = rep(rep(rnorm(3),3),2),
#' group = rep(c('G1','G2','G2'),each=3),
#' wave = rep(c(1,2),each=9)
#' )
#' df
#' reformat(df)
#' 
#' ## Example 2: Reformat data for lavaan package
#' reformatted_data = reformat(df, package = "lavaan", timedate = TRUE)
#' 
#' ## Example 3: Reformat data for psych package
#' reformatted_data = reformat(df, package = "psych", covariates = TRUE)
#' 
#' @details
#' The `reformat` function simplifies the process of reformatting data for use with various R packages. The function is designed to work with a variety of data formats, including wide and long formats, as well as data with covariates, groups, item groups, raters, and more. With the exception of `lme4`, all other package selections default to performing a wider pivot (e.g. `tidyr::pivot_wider_spec`) on the data, transposing all items in the dataset into columns. 
#' This function is not meant for complex modeling tasks, but rather for quickly reformatting data for use with psychometric analysis packages to facilitate greater use of a wide variety of psychometric data and tools in `R`.
#' 
#' The benefits of using this function compared to a more robust suite of tools such as `recipes` and `workflows` tools are: 
#' 1) *simplicity* and, in most cases, *speed* by being able to quickly reformat data based on `irw` data standards
#' 2) *usability* as some psychometric researchers may not be familiar enough with how to adapt the complex `tidymodels` pipelines across many datasets for latent variable analyses (such as those found in `mirt`)
#' 3) *automation* of the most time-consuming parts of `tidymodels` (creating recipes where you define all the variable roles and transformations): the reformat function does this automatically by exploiting the knowledge of it being some kind of psychometric dataset. 
#' 
#' The `reformat` R function (and its supporting functions) is designed to take a data.frame (provided from the `irw` database) and reconstruct it in the format required by various `R` packages for psychometric analysis. It does this by transforming the reformatted data using user specifications, automatically identifying and cataloging available variables as needed, and matching the transformations with expected format for the user specified package, with robustness checks for various data and user combinations of parameters It returns the data in the desired format as an object with the additional class `irw_format` (which will be used for class methods in the future package iterations for greater efficiency).
#' These are the steps currently used for `reformat`:
#' **Analyzing the data provided**
#' The function creates a catalog of dataset variables, checking for compatability, identifying the roles of the variables, converting them to expected data types, and prioritizing their importance with respect to the user specifications
#' When specified and where possible, the package automatically coerces and prioritizes the variables in the data to the expected data types and roles. The function will also check for the presence of the variables in the data and return an informative error if they are not found.
#' The user can provide either column names or a boolean/logical to specify the variables. If the user provides a boolean, the function will attempt to automatically identify the variables based on the user specifications.
#' Not every package has support for every variable role currently. The function will return either an error or a warning if the user specifies a variable or output type that is not supported by the package, is in conflict with other data, or is otherwise incompatible. Working with the user base, future iterations of the package will include more support for additional packages and variable roles to better serve the psychometric community. Currently, rater variables are only supported by the `lme4` package configuration.
#' **Reformatting the data**
#' For wider pivots, checks for unique identification of the `resp` variable by `id` and `item` variables (if not found, automatically searches for and uses other variables that would allow for unique identification, and returns an error in the rare case that no unique identification is found in an `irw` dataset). 
#' The default package is `mirt`, which is the most common package used for psychometric analysis. The function will pivot the data into a wide format with each item as a column. It will retain `NAs` in the data by default, but the user can specify to drop them if needed. This is the same for the `ltm` package.
#' The `mokken` package supports are similar to `mirt`, but are more restrictive. The function will pivot the data into a wide format with each item as a column. For `mokken`, all rows with missing values will be dropped. 
#' For the `lavaan` package, the function will pivot the data into a very wide format with each item, date, group, etc. are combined into unique variables. 
#' For users interested in the `sem` package, users would probably benefit the most from setting the supported package to `lavaan` , as many of the same structural assumptions hold. The `lavaan` configuration can support additional variables and covariates that can be uniquely identified and matched on the wide pivoted data.
#' For the `psych` package, the function will pivot the data into a wide format with each item as a column for factor analysis. This package, like `lavaan`, can support additional variables and covariates that can be uniquely identified and matched on the wide pivoted data including automatically transforming dummy variables as needed, and thus easily supports functions like, `fa`, `bigCor`, etc. However, the function will not support all functions within the package. A user wishing to perform instrument reliability checks such as `alpha` or `omega` should not specify covariates or other variable roles.
#' For the `lme4` package, the function will return the data into a long format, converting all variables and variable names in the data to the expected format for linear mixed effects in the package.
#' The function will automatically identify and convert factor columns to dummy variables if needed. The function currently supports the following packages: `mirt`, `lavaan`, `psych`, `ltm`, `mokken`, and `lme4`.
#' @section Resources: 
#' Package Manuals:
#' `mirt`: https://cran.r-project.org/web/packages/mirt/mirt.pdf
#' `lavaan`: https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
#' `psych`: https://cran.r-project.org/web/packages/psych/psych.pdf
#' `ltm`: https://cran.r-project.org/web/packages/ltm/ltm.pdf
#' `mokken`: https://cran.r-project.org/web/packages/mokken/mokken.pdf
#' `lme4`: https://cran.r-project.org/web/packages/lme4/lme4.pdf
#  sem: https://cran.r-project.org/web/packages/sem/sem.pdf
#' @seealso [recipes::recipe()],[recipes::prep()],[recipes::bake()],[workflows::workflow()]
#' @importFrom dplyr as_tibble mutate select across left_join everything all_of setdiff distinct filter
#' @importFrom tidyr pivot_wider pivot_longer drop_na
# @importFrom stats model.matrix
#' @importFrom psych dummy.code
#' @importFrom tibble is_tibble
#' @importFrom tidyselect all_of matches
#' @importFrom forcats as_factor fct
#' @export
reformat = function(data,
                    package = "mirt",
                    id = "id",
                    item = "item",
                    resp = "resp",
                    groups = NULL,
                    timedate = NULL,
                    covariates = NULL,
                    levels = NULL,
                    rt = NULL,
                    qmatrix = NULL,
                    item_groups = NULL,
                    group_covariates = NULL,
                    raters = NULL,
                    rater_covariates = NULL,
                    keep_all = F,
                    facts2dummies = NULL,
                    as_args_list = F, ## currently not supported
                    drop_na_vals = F,
                    item_prefix = "item_",
                    sep = "_",
                    return_obj = "tibble",
                    return_options = NULL) {
  if (!tibble::is_tibble(data)) {
    data = dplyr::as_tibble(data)
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
  catalog = list()
  catalog_names = list()
  catalog_names$original_names = names(data)
  data = data |> irw_rename()
  catalog_names$cleaned_names = names(data)
  # Check to ensure the identified columns "id", "item", and "resp" are present in the tibble (if not it will return an error)
  if (!all(c(id,item, resp) %in% names(data))) {
    stop("The columns for 'id', 'item', and 'resp' must be present in the data")
  }
  
  # Function to add variables to the catalog and convert them to the appropriate data type
  add_to_catalog = function(catalog = catalog, var_name, role) {
    catalog[[var_name]] = list(
      name = var_name,
      role = role,
      dtype = var_roles[[role]]$dtype,
      priority = var_roles[[role]]$priority
    )
    catalog
  }
  
  
  # Convert resp to numeric if necessary
  data = data |> check_numeric(resp, item)
  
  
  catalog = add_to_catalog(catalog, resp, "resp")
  for (col in c(id)) {
    catalog = add_to_catalog(catalog, col, "id")
  }
  for (col in c(item)) {
    catalog = add_to_catalog(catalog, col, "item")
  }
  
  # Add variables with id and item roles to catalog, converting their data types appropriately
  data = data |> dplyr::mutate(id = catalog[["id"]]$dtype(id))
  data = data |> dplyr::mutate(item = catalog[["item"]]$dtype(item))
  
  # Check if any other variables have been specified in the args and add them to the catalog
  if (keep_all) {
    user_specified_char_columns_found_in_args = names(data)[!names(data) %in% c(id, item, resp)]
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
        user_specified_char_columns_found_in_args = c(user_specified_char_columns_found_in_args,
                                                       arg_value)
      }
    }
  }
  
  # Add all other variables indicated in args to the catalog
  # for each role in var_roles, check if the role is in args, if so, add to catalog
  # This loop iterates over the names of the elements in the 'var_roles' list.
  # 'var_roles' is assumed to be a predefined global variable containing role information.
  # Each iteration processes one role from 'var_roles', done in order of priority within var_roles (assuming resp, item, and id have already been added).
  remaining_roles_in_order_of_priority = names(var_roles)[!names(var_roles) %in% c("resp", "id", "item", "other")]
  for (role in remaining_roles_in_order_of_priority) {
    # if corresponding argument is not NULL, add to catalog
    role_col = eval(parse(text = role))
    if (!is.null(role_col)) {
      # if character is in not_supported_cols, return an error
      if ((role %in% not_supported_cols) & !cov_wide_supps[[package]] & piv_wide_pkg[[package]]) {
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
            catalog = add_to_catalog(catalog, r, role)
            data = data |> dplyr::mutate(dplyr::across(dplyr::all_of(r), catalog[[r]]$dtype))
          }
        }
        # add the role to the catalog
      } else if (isTRUE(role_col) |
                 keep_all) {
        # if the role is boolean and true,
        pat = var_roles[[role]]$grep
        candidate_cols = names(data)[grepl(pat, names(data))]
        # remove any columns that have already been used in the catalog
        candidate_cols = dplyr::setdiff(candidate_cols, names(catalog))
        # remove any columns that have been specified in the args
        candidate_cols = dplyr::setdiff(candidate_cols,
                                  user_specified_char_columns_found_in_args)
        for (col in candidate_cols) {
          catalog = add_to_catalog(catalog, col, role)
          data = data |> dplyr::mutate(dplyr::across(dplyr::all_of(col), catalog[[col]]$dtype))
        }
      }
    }
  }

  ## if keep_all is true, add all columns not already in the catalog to the catalog
  if (keep_all) {
    remaining_cols = setdiff(names(data), names(catalog))
    for (col in remaining_cols) {
      catalog = add_to_catalog(catalog, col, "other")
      data = data |> dplyr::mutate(dplyr::across(dplyr::all_of(col), catalog[[col]]$dtype))
      
    }
  }
  
  
  # create copy of data with only the columns in the catalog
  data_cleaned = data[, names(catalog)]
  
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
      id_cols = names(catalog)[sapply(catalog, function(x)
        x$role == "id")]
      
      # get names_from by finding all variables with item role in catalog
      names_from = names(catalog)[sapply(catalog, function(x)
        x$role == "item")]
      # if lavaan or sem, check if there are either timedate or item_groups and if so, add them to names_from
      if (package %in% c("lavaan", "sem")) {
        names_from = c(names_from, names(catalog)[sapply(catalog, function(x)
          x$role %in% c("timedate", "item_groups"))])
      }

      # find pivot arguments try first from data_cleaned and if not possible, try to find from data
      pivot_args = NULL
      try({
        pivot_args = find_pivot_args(data_cleaned, id_cols, names_from, resp, catalog = catalog)
      }, silent = TRUE)
      if (is.null(pivot_args)) {
        pivot_args = find_pivot_args(data, id_cols, names_from, resp, catalog = catalog)
        data_cleaned = data[, c(names(catalog), setdiff(as.character(unlist(
          pivot_args, use.names = F
        )), names(catalog)))]
      }
      
      ## print notification if pivot_args need to be combined
      if (needs_combined_columns(pivot_args)) {
        notify_combined_columns(pivot_args)
      }
      # pivot data
      data_formatted = data_cleaned |> tidyr::pivot_wider(
        names_from = pivot_args$names_from,
        values_from = pivot_args$values_from,
        id_cols = pivot_args$id_cols,
        names_prefix = item_prefix,
        names_sep = sep
      )
      
      unused_vars = setdiff(names(data_cleaned), as.character(unlist(pivot_args, use.names = F)))
      ## issue warning if cov_wide_supps[[package]] is false and there are other unused variables in the catalog and state which package does not support them and which variables will be ignored
     
      if ((length(unused_vars) > 0) & !cov_wide_supps[[package]]) {
        warning(
          paste0(
            "The support for package '",
            package,
            "' does not currently allow the following dropped variables: ",
            paste(unused_vars, collapse = ", ")
          )
        )
      }
      # if there are other unused variables in the catalog and if any of the supported methods for the package have var_roles of the unused variables, add them to the data by joining them back to the data_cleaned
      if ((length(unused_vars) > 0) & cov_wide_supps[[package]]) {
        tmpdata = data_cleaned[, c(unused_vars, pivot_args$id_cols)] |> dplyr::distinct()
        ## if the number of unique rows in tmpdata is greater than the number of rows in data_formatted, check if any individual columns have more unique values than the length of data_formatted and drop them
        if (nrow(tmpdata) > nrow(data_formatted)) {
          for (col in names(tmpdata)) {
            if (length(unique(tmpdata[[col]])) > nrow(data_formatted)) {
              warning(paste0(
                "The column '",
                col,
                "' has been dropped due to having more unique values than the id values. To keep, set package to `lme4` and set keep_all to `TRUE`."
              ))
              tmpdata = tmpdata |> dplyr::select(-dplyr::all_of(col)) |> dplyr::distinct()
            }
          }
        }
        data_formatted = data_formatted |> dplyr::left_join(tmpdata, by = pivot_args$id_cols)
      }
      ## combine any id_cols to create unique rownames and then drop them
      data_formatted = data_formatted |>
        tidyr::unite("rowid",
              pivot_args$id_cols,
              sep = sep,
              remove = T) |>
        tibble::column_to_rownames("rowid")

    } else {
      data_formatted = data_cleaned |> dplyr::as_tibble()
    }
  } else {
    stop("The specified package is not supported")
  }
  ## check if any columns in dataformatted need to be dropped due to NAs and drop them with a warning message
  cols_to_drop = colnames(data_formatted)[colSums(is.na(data_formatted)) == nrow(data_formatted)]
  if (length(cols_to_drop) > 0) {
    warning(paste0(
      "The following columns have been dropped due to all missing values: ",
      paste(cols_to_drop, collapse = ", ")
    ))
    data_formatted = data_formatted |> dplyr::select(-dplyr::all_of(cols_to_drop))
  }
  
  rows_to_drop = rownames(data_formatted)[rowSums(is.na(data_formatted)) == ncol(data_formatted)]
  if (length(rows_to_drop) > 0) {
    warning(paste0(
      "A total of ",
      length(rows_to_drop),
      " rows have been dropped due to all missing values"
    ))
    data_formatted = data_formatted |>  dplyr::filter(!rownames(data_formatted) %in% rows_to_drop)
  }

  
  ## if psych package, convert factors to numeric if ordered, else convert to dummies
  if (package %in% c("psych","lavaan")){
    for (col in names(data_formatted)) {
      ## first check if the column has only one value and if so, drop it
      if (one_value_check(data_formatted[[col]])) {
        data_formatted = data_formatted[, !(names(data_formatted) %in% col)]
        print(paste0("The column '", col, "' has been dropped due to having only one unique value (zero variance)"))
        
      } else if (class(data_formatted[[col]]) %in% c("factor", "ordered", "character", "logical")) {
        data_formatted = check_numeric(data_formatted, col)
        if (is.factor(data_formatted[[col]]) & package %in% c("psych")) {
          dummy_cols = psych::dummy.code(data_formatted[[col]], na.rm = T)
          data_formatted = cbind(data_formatted, dummy_cols)
          # cbind(data_formatted, dummy_cols)
          data_formatted = data_formatted[, !(names(data_formatted) %in% col)]
        }
      }
    }
  }
  
  ## drop na rows if drop_na_vals is true or if the package is mokken
  if (drop_na_vals | package == "mokken") {
    data_formatted = data_formatted |> tidyr::drop_na()
  }
  
  # Return the formatted data
  if (return_obj == "tibble") {
    data_formatted = dplyr::as_tibble(data_formatted)
  } else if (return_obj == "data.frame") {
    data_formatted = as.data.frame(data_formatted)
  } else if (return_obj == "matrix") {
    data_formatted = as.matrix(data_formatted)
  } else {
    stop("Unsupported return object type")
  }
  
  # Add class to the formatted data
  class(data_formatted) = c("irw_format", class(data_formatted))
  
  return(data_formatted)
  
  
  
}
