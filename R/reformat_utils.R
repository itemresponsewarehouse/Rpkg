library(dplyr)
library(tidyr)

find_pivot_args <- function(data, id_cols = "id", names_from = "item", values_from = "resp") {
  # Check if the dataframe is uniquely identified by the specified columns
  check_uniqueness <- function(data, cols) {
    data %>%
      group_by(across(all_of(cols))) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n > 1) %>%
      nrow() == 0
  }
  
  # Initial check using provided id_cols and names_from
  if (check_uniqueness(data, c(id_cols, names_from))) {
    return(list(id_cols = id_cols, names_from = names_from, values_from = values_from))
  }
  
  # Identify additional columns
  additional_cols <- setdiff(names(data), c(id_cols, names_from, values_from))
  
  # Try adding columns to id_cols
  for (col in additional_cols) {
    if (check_uniqueness(data, c(id_cols, col, names_from))) {
      return(list(id_cols = c(id_cols, col), names_from = names_from, values_from = values_from))
    }
  }
  
  # Try adding columns to names_from
  for (col in additional_cols) {
    if (check_uniqueness(data, c(id_cols, names_from, col))) {
      return(list(id_cols = id_cols, names_from = c(names_from, col), values_from = values_from))
    }
  }
  
  # Try combinations of additional columns
  for (cols in combn(additional_cols, 2, simplify = FALSE)) {
    if (check_uniqueness(data, c(id_cols, names_from, cols))) {
      return(list(id_cols = id_cols, names_from = c(names_from, cols), values_from = values_from))
    }
    if (check_uniqueness(data, c(id_cols, cols, names_from))) {
      return(list(id_cols = c(id_cols, cols), names_from = c(names_from), values_from = values_from))
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

notify_combined_columns <- function(args) {
  # Check each argument for length > 1
  for (arg_name in names(args)) {
    arg_value <- args[[arg_name]]
    
    if (length(arg_value) > 1) {
      # Print a notification for combined columns
      combined_cols <- paste(arg_value, collapse = ", ")
      message(
        sprintf("The following columns are being combined for '%s': %s", 
                arg_name, combined_cols)
      )
    }
  }
}

pivot_wider_irw  = function(data, names_from = "item", values_from = "resp", 
                            id_cols = "id", names_sep = "_",...) {
  # Find the pivot arguments
  piv_args <- find_pivot_args(data, id_cols, names_from, values_from)
  
  # Notify the user about combined columns
  notify_combined_columns(piv_args)
  
  # Perform the pivot wider operation
  newdf = inject(pivot_wider(data, !!!piv_args, ...))
  
  newdf = newdf |> unite("id", piv_args$id_cols, sep = names_sep, remove = T) |> column_to_rownames(var = "id")
  
  # If id_cols length > 1, then combine them into a single index column called 'id' with "_" separating the values
  # if(length(piv_args$id_cols) > 1){
  #   newdf <- newdf %>% mutate(id = do.call(paste, c(., sep = names_sep, list(select(., all_of(piv_args$id_cols)))))) %>% 
  #     select(-all_of(setdiff(c("id"), piv_args$id_cols)))
  # }
  
  # set id as rownames
  # newdf = newdf |> column_to_rownames(var = "id")
  
  return(newdf)

}