# Declare global variable to suppress check note
utils::globalVariables(c("datasource"))

#' Initialize Datasource
#'
#' This function initializes the Redivis datasource connection if it is not already set.
initialize_datasource <- function() {
  if (is.null(datasource)) {
    datasource <<- redivis::user("datapages")$dataset("item_response_warehouse")
  }
  return(datasource)
}
