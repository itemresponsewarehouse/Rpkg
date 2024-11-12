# Declare global variable to suppress check note
utils::globalVariables(c("datasource"))

#' Initialize Datasource
#'
#' This function initializes the Redivis datasource connection if it is not already set.
#' If the global variable `datasource` doesn't exist or is NULL, it sets up a new connection.
#' @return The initialized datasource connection.
initialize_datasource <- function() {
  # Check if 'datasource' exists in the global environment and is non-NULL
  if (!exists("datasource", envir = .GlobalEnv) || is.null(datasource)) {
    datasource <<- redivis::user("datapages")$dataset("item_response_warehouse")
  }
  return(datasource)
}
