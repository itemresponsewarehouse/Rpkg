library(dplyr)
library(tidyr)

#' @title Check if the response variable is numeric
#' @description
#' This function checks if the response variable is numeric and if not, converts it to numeric.
#' If more than 75% of values of resp would be NAs when converting resp to numeric, it will return an error. Otherwise it will attempt to make it a factor.
#' If the number of unique values < total number of unique items, it will convert resp to factor, with an informative warning, and change its dtype in the catalog.
#' @param data A data frame with the response variable
#' @param resp The name of the response variable
#' @param item The name of the item variable
#' @return Returns the data frame with the response variable converted to numeric
#' @export
#' @examples
#' data <- data.frame(id = c(1, 2, 3), item = c(1, 2, 3), resp = c(1, 2, 3))
#' check_resp(data, "resp", "item")
#' @noRd

check_resp <- function(data, resp, item = NULL) {
  if (!is.numeric(data[[resp]])) {
    if (sum(is.na(as.numeric(data[[resp]]))) > 0.75 * nrow(data) &
        !is.null(item) & (resp == "resp")) {
      if (length(unique(data[[resp]])) < length(unique(data[[item]]))) {
        warning(
          "The variable ",
          resp,
          " is not numeric. Because the number  of unique values is less than the total number of unique items, it will be converted to a factor."
        )
        data[[resp]] <- as.factor(data[[resp]])
      } else {
        stop("More than 75% of values would be NAs when converting resp to numeric")
      }
    } else if (is.null(item) & (resp != "resp")) {
      if (sum(is.na(as.numeric(data[[resp]]))) > 0.75 * nrow(data)) {
        data[[resp]] <- as.factor(data[[resp]])
      } else {
        data[[resp]] <- as.numeric(data[[resp]])
      }
    }
    else {
      data[[resp]] <- as.numeric(data[[resp]])
    }
  }
  data
}



# Check if the dataframe is uniquely identified by the specified columns
check_uniqueness <- function(data, cols) {
  data %>%
    group_by(across(all_of(cols))) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1) %>%
    nrow() == 0
}

# Function to find the pivot arguments for pivot_wider
find_pivot_args <- function(data,
                            id_cols = "id",
                            names_from = "item",
                            values_from = "resp",
                            catalog = catalog) {
  # Initial check using provided id_cols and names_from
  if (check_uniqueness(data, c(id_cols, names_from))) {
    return(list(
      id_cols = id_cols,
      names_from = names_from,
      values_from = values_from
    ))
  }
  
  # Identify additional columns
  additional_cols <- setdiff(names(data), c(id_cols, names_from, values_from))
  if (length(additional_cols) == 0) {
    stop("Could not find a combination of columns to ensure unique identification.")
  }
  
  
  # Try adding columns to id_cols based on
  for (col in additional_cols) {
    if (check_uniqueness(data, c(id_cols, col, names_from))) {
      return(list(
        id_cols = c(id_cols, col),
        names_from = names_from,
        values_from = values_from
      ))
    }
  }
  
  # Try adding columns to names_from
  for (col in additional_cols) {
    if (check_uniqueness(data, c(id_cols, names_from, col))) {
      return(list(
        id_cols = id_cols,
        names_from = c(names_from, col),
        values_from = values_from
      ))
    }
  }
  if (length(additional_cols) < 2) {
    stop("Could not find a combination of columns to ensure unique identification.")
  }
  
  # Try combinations of additional columns
  for (cols in combn(additional_cols, 2, simplify = FALSE)) {
    if (check_uniqueness(data, c(id_cols, names_from, cols))) {
      return(list(
        id_cols = id_cols,
        names_from = c(names_from, cols),
        values_from = values_from
      ))
    }
    if (check_uniqueness(data, c(id_cols, cols, names_from))) {
      return(list(
        id_cols = c(id_cols, cols),
        names_from = c(names_from)
      ))
    }
  }
  
  stop("Could not find a combination of columns to ensure unique identification.")
}

# Example usage:
# df <- tibble::tibble(
#   id = c(1, 1, 2, 2),
#   item = c("A", "B", "A", "B"),
#   resp = c(10, 20, 10, 20),
#   extra = c("X", "Y", "X", "Y")
# )
# piv_args = find_pivot_args(df, id_cols = "id", names_from = "item")
# newdf = inject(pivot_wider(df,!!!piv_args))

# list_of_all_piv_args

needs_combined_columns <- function(args) {
  # Check each argument for length > 1
  for (arg_name in names(args)) {
    arg_value <- args[[arg_name]]
    
    if (length(arg_value) > 1) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

notify_combined_columns <- function(args) {
  # Pre-allocate messages vector
  messages <- character(0)
  
  # Single pass through arguments
  combined_info <- Filter(function(x)
    length(x) > 1, args)
  
  # Build all messages at once if needed
  if (length(combined_info) > 0) {
    messages <- sprintf(
      "The following columns are being combined for '%s': %s",
      names(combined_info),
      vapply(combined_info, function(x)
        paste(x, collapse = ", "), character(1))
    )
    # Single message call
    message(paste(messages, collapse = "\n"))
  }
}
