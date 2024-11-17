#' @title Cleans names for consistent IRW objects during transformations
#'
#' @description 
#' Similar to but less flexible than `janitor::clean_names()`, resulting strings 
#' are unique and consist only of the `_` character, numbers, and lower case letters in lower case.
#' character, numbers, and letters to put files in compliance 
#'
#' The order of operations is: make replacements,
#' remove initial spaces and punctuation, apply `tolower()`, and add numeric suffixes
#' to resolve any duplicated names. 
#'
#' @param string A character vector of names to clean.
#' @param replace A named character vector where the name is replaced by the
#'   value.
#'
#' @return Returns the "cleaned" object
#' @seealso [janitor::clean_names()]
#' @examples
#'
#' # cleaning the names of a vector:
#' x <- structure(1:3, names = c("name with space", "TwoWords", "total $ (2009)"))
#' x
#' names(x) <- irw_name_fix(names(x))
#' x # now has cleaned names

#' @importFrom stringr str_replace str_replace_all
#' @noRd

irw_name_fix <- function(string,
                         replace =
                           c(
                             "'" = "",
                             "\"" = "_",
                             "%" = "_pct",
                             "#" = "_num",
                             " " = "_",
                             "-" = "_",
                             "/" = "_",
                             "__"= "_",
                             "@" = "_at_"
                           ),
                         digit_first_ok = FALSE,
                         digit_prefix = "x",
                         ...) {
  if (is.data.frame(string)) {
    stop("`string` must not be a data.frame, use clean_names()")
  }
  
  replaced_names <-
    stringr::str_replace_all(string = string, pattern = replace)
  
  # Remove starting spaces and punctuation
  str_start <-
    stringr::str_replace(string = replaced_names,
                         # Description of this regexp:
                         # \A: beginning of the string (rather than beginning of the line as ^ would indicate)
                         # \h: any horizontal whitespace character (spaces, tabs, and anything else that is a Unicode whitespace)
                         # \s: non-unicode whitespace matching (it may overlap with \h)
                         # \p{}: indicates a unicode class of characters, so these will also match (P) punctuation, (S) symbols, (Z) separators, and (C) "other" characters
                         # * means all of the above zero or more times (not + so that the capturing part of the regexp works)
                         # (.*)$: captures everything else in the string for the replacement
                         pattern = "\\A[\\h\\s\\p{P}\\p{S}\\p{Z}\\p{C}]*(.*)$",
                         replacement = "\\1")
  # Convert all interior spaces and punctuation to single dots
  cleaned_names <-
    stringr::str_replace_all(string = str_start,
                             pattern = "[\\h\\s\\p{P}\\p{S}\\p{Z}\\p{C}]+",
                             replacement = "_")
  
  new_names <- tolower(cleaned_names)
  
  ## correct any names beginning with a digit
  if (any(grepl("\\A\\d", new_names)) & !digit_first_ok) {
    ## add prefix to those staring w digit
    new_names[grepl("\\A\\d", new_names)] <-
      paste0(digit_prefix, new_names[grepl("\\A\\d", new_names)])
    
  }
  
  
  # add counters to duplicated names
  while (any(duplicated(new_names))) {
    dupes <-
      vapply(seq_along(new_names), function(i) {
        sum(new_names[i] == new_names[1:i])
      }, 1L)
    
    new_names[dupes > 1] <-
      paste(new_names[dupes > 1], dupes[dupes > 1], sep = "_")
  }
  # }
  
  new_names
}

#' ## function to apply irw_name_fix to a vector of arbitrary length
#' #' @param x a vector of names to clean
#' #' @noRd
#' #' @export
#' 
#' irw_name_fix_all <- function(x, ...) {
#'   if (is.data.frame(x)) {
#'     stop("`x` must not be a data.frame, use clean_names()")
#'   }
#'   vapply(x, irw_name_fix, character(1), ...)
#' }

## function to apply irw_name_fix to an object with names 
#' @param x an object with names to clean
#' @noRd
#' @export
#' 
irw_rename = function(x, ...) {
  if (is.data.frame(x)) {
    x |> dplyr::rename_with(function(.name) {
      irw_name_fix(.name)
    })
  } else if (is.character(x)) {
    map_chr(c(x),irw_name_fix)
  } else {
    stop("x must be a data frame or character vector")
  }
}

# irw_name_fixer = function(x, ...) {
#   if (is.data.frame(x)) {
#     irw_rename(x)
#   } else if (is.character(x)) {
#     map_chr(c(x),irw_name_fix)
#   } else {
#     stop("x must be a data frame or character vector")
#   }
# }



#' @param df returns the data frame with cleaned column names,
#' @noRd
irw_col_clean = function(df,...) {
  colnames(df) = irw_name_fix(colnames(df))
  df
}

#' @param df returns the data frame with cleaned row names,
#' @noRd
irw_row_clean = function(df,...) {
  rownames(df) = irw_name_fix(rownames(df), digit_first_ok = TRUE)
  df
}

#' @param df returns the data frame with cleaned row names,
#' @param dims columns 'c', rows 'r', or both 'b'
#' @export
irw_name_cleaner = function(df,dims = "c",...) {
  if (dims == "c") {
    irw_col_clean(df)
  } else if (dims == "r") {
    irw_row_clean(df)
  } else {
    irw_col_clean(irw_row_clean(df))
  }
  
}


dtypes <- function(x) {
  res <- lapply(x, .get_data_types)
  tibble(variable = names(res), type = unname(res))
}

one_value_check = function(x) {
  x = x[!is.na(x)]
  length(unique(x)) < 2
}

one_value_check_grp = function(x, f) {
  x_split = split(x, f)
  any(vapply(x_split, one_value_check, logical(1)))
}

#' Rename variables by name using dplyr
#'
#' `step_rename()` creates a *specification* of a recipe step that will add
#' variables using [dplyr::rename()].
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... One or more unquoted expressions separated by commas. See
#'  [dplyr::rename()] where the convention is **`new_name = old_name`**.
#' @param inputs Quosure(s) of `...`.
#' @template step-return
#' @details When an object in the user's global environment is referenced in
#'  the expression defining the new variable(s), it is a good idea to use
#'  quasiquotation (e.g. `!!`) to embed the value of the object in the
#'  expression (to be portable between sessions).
#'
#'  # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{character, `rename` expression}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @family dplyr steps
#' @export
#' @examples
#' recipe(~., data = iris) %>%
#'   step_rename(Sepal_Width = Sepal.Width) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   slice(1:5)
#'
#' vars <- c(var1 = "cyl", var2 = "am")
#' car_rec <-
#'   recipe(~., data = mtcars) %>%
#'   step_rename(!!!vars)
#'
#' car_rec %>%
#'   prep() %>%
#'   bake(new_data = NULL)
#'
#' car_rec %>%
#'   tidy(number = 1)
step_rename <- function(recipe, ...,
                        role = "predictor",
                        trained = FALSE,
                        inputs = NULL,
                        skip = FALSE,
                        id = rand_id("rename")) {
  inputs <- enquos(..., .named = TRUE)
  
  add_step(
    recipe,
    step_rename_new(
      terms = terms,
      trained = trained,
      role = role,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}

step_rename_new <-
  function(terms, role, trained, inputs, skip, id) {
    step(
      subclass = "rename",
      terms = terms,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_rename <- function(x, training, info = NULL, ...) {
  step_rename_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    inputs = x$inputs,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_rename <- function(object, new_data, ...) {
  dplyr::rename(new_data, !!!object$inputs)
}

#' @export
print.step_rename <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Variable renaming for "
    trained_names <- names(x$inputs)
    
    untrained_terms <- rlang::parse_quos(
      trained_names %||% "",
      rlang::current_env()
    )
    print_step(trained_names, untrained_terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_rename <- function(x, ...) {
  var_expr <- map(x$inputs, quo_get_expr)
  var_expr <- map_chr(var_expr, quo_text, width = options()$width, nlines = 1)
  
  tibble(
    terms = names(x$inputs) %||% character(),
    value = unname(var_expr) %||% character(),
    id = rep(x$id, length(x$inputs))
  )
}