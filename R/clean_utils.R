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
#' x = structure(1:3, names = c("name with space", "TwoWords", "total $ (2009)"))
#' x
#' names(x) = irw_name_fix(names(x))
#' x # now has cleaned names
#' @importFrom dplyr as_tibble mutate select across left_join everything all_of setdiff distinct filter group_by summarise
#' @importFrom tidyr pivot_wider pivot_longer drop_na
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace str_replace_all
#' @noRd

irw_name_fix = function(string,
                         replace =
                           c(
                             "'" = "",
                             "\"" = "_",
                             "%" = "_pct",
                             "#" = "_num",
                             " " = "_",
                             "-" = "_",
                             "/" = "_",
                             "__" = "_",
                             "@" = "_at_"
                           ),
                         digit_first_ok = FALSE,
                         digit_prefix = "x",
                         ...) {
  if (is.data.frame(string)) {
    stop("`string` must not be a data.frame, use clean_names()")
  }
  
  replaced_names =
    stringr::str_replace_all(string = string, pattern = replace)
  
  # Remove starting spaces and punctuation
  str_start =
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
  cleaned_names =
    stringr::str_replace_all(string = str_start,
                             pattern = "[\\h\\s\\p{P}\\p{S}\\p{Z}\\p{C}]+",
                             replacement = "_")
  
  new_names = tolower(cleaned_names)
  
  ## correct any names beginning with a digit
  if (any(grepl("\\A\\d", new_names)) & !digit_first_ok) {
    ## add prefix to those staring w digit
    new_names[grepl("\\A\\d", new_names)] =
      paste0(digit_prefix, new_names[grepl("\\A\\d", new_names)])
    
  }
  
  
  # add counters to duplicated names
  while (any(duplicated(new_names))) {
    dupes =
      vapply(seq_along(new_names), function(i) {
        sum(new_names[i] == new_names[1:i])
      }, 1L)
    
    new_names[dupes > 1] =
      paste(new_names[dupes > 1], dupes[dupes > 1], sep = "_")
  }

  new_names
}

## Function to apply irw_name_fix to an object with names
#' Apply renaming to an object with names
#' 
#' @param x an object with names to clean
#' @param ... additional arguments to be passed down to irw_name_fix
#' 
#' @details
#' This function is the main standardization function for IRW objects. It applies irw_name_fix to the names of an object.
#' 
#' @return the object with cleaned names
#' @examples
#' irw_rename(data.frame("A B" = 1:3, "C D" = 4:6))
#' irw_rename(c("A B", "C D"))
#' @export
irw_rename = function(x, ...) {
  if (is.data.frame(x)) {
    x |> dplyr::rename_with(function(.name) {
      irw_name_fix(.name)
    })
  } else if (is.character(x)) {
    purrr::map_chr(c(x), irw_name_fix)
  } else {
    stop("x must be a data frame or character vector")
  }
}

one_value_check = function(x) {
  x = x[!is.na(x)]
  length(unique(x)) < 2
}

one_value_check_grp = function(x, f) {
  x_split = split(x, f)
  any(vapply(x_split, one_value_check, logical(1)))
}
