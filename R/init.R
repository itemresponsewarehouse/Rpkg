utils::globalVariables("datasource")
#' Initialize Datasource
#'
#' This function initializes the Redivis datasource connection if it is not already set.
#' @return The initialized Redivis datasource connection.
#' @keywords internal
initialize_datasource <- function() {
  if (is.null(datasource)) {
    # Super-assign to make it globally available within the package
    datasource <<- redivis::user("datapages")$dataset("item_response_warehouse")
  }
  return(datasource)
}

