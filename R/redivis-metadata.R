# 1. dataset management (getting datasets, caching)
# 2. table fetching (getting specific tables)
# 3. metadata operations (metadata, biblio, tags, itemtext)



#' Fetch Metadata Table
#'
#' Retrieves the metadata table from Redivis user("bdomingu")$dataset("irw_meta")$table("metadata").
#' Only fetches new data if the table version tag has changed.
#'
#' @return A cached or newly fetched tibble containing metadata information.
#' @keywords internal
.fetch_metadata_table <- function() {
  dataset <- redivis::redivis$user("bdomingu")$dataset("irw_meta:bdxt")
  dataset$get()
  # Retrieve version tag
  latest_version_tag <- dataset$properties$version$tag
  
  # If metadata exists in cache and the version tag is the same, return cached tibble
  if (!is.null(latest_version_tag) &&
      exists("metadata_tibble", envir = .irw_env) &&
      exists("metadata_version", envir = .irw_env) &&
      identical(.irw_env$metadata_version, latest_version_tag)) {
    return(.irw_env$metadata_tibble) # Return cached metadata tibble
  }
  
  # Fetch new metadata table and convert it to a tibble
  table <- dataset$table("metadata:h5gs")
  
  .irw_env$metadata_tibble <- .retry_with_backoff(function() {
    table$to_tibble()
  })
  
  # Store the new version tag
  .irw_env$metadata_version <- latest_version_tag
  
  return(.irw_env$metadata_tibble)
}


#' Fetch Bibliography Metadata Table
#'
#' Retrieves the metadata table from Redivis user("bdomingu")$dataset("irw_meta")$table("biblio").
#' Only fetches new data if the table version tag has changed.
#' This version filters out any tables that do not exist in the IRW database.
#'
#' @return A tibble containing filtered biblio information, with only the tables that exist in the IRW database.
#' @keywords internal
.fetch_biblio_table <- function() {
  dataset <- redivis::redivis$user("bdomingu")$dataset("irw_meta:bdxt")
  dataset$get() 
  latest_version_tag <- dataset$properties$version$tag
  
  # Return cached if version unchanged
  if (!is.null(latest_version_tag) &&
      exists("biblio_tibble", envir = .irw_env) &&
      identical(.irw_env$biblio_version, latest_version_tag)) {
    return(.irw_env$biblio_tibble)
  }
  
  # Fetch fresh biblio table
  table <- dataset$table("biblio:qahg")
  biblio_tibble <- .retry_with_backoff(function() table$to_tibble())
  
  # Get all table names from all datasets
  ds_list <- .initialize_datasource(sim = FALSE)
  table_name_list <- tolower(unlist(lapply(ds_list, function(ds) {
    ds$get()  
    vapply(ds$list_tables(), function(tbl) tbl$name, character(1))
  })))
  
  # Filter biblio table
  biblio_tibble$table_lower <- tolower(biblio_tibble$table)
  filtered_biblio <- biblio_tibble[biblio_tibble$table_lower %in% table_name_list, ]
  filtered_biblio$table_lower <- NULL
  
  # Cache
  .irw_env$biblio_version <- latest_version_tag
  .irw_env$biblio_tibble <- filtered_biblio
  
  return(filtered_biblio)
}

#' Fetch Tags Metadata Table
#'
#' Retrieves the tags metadata table from Redivis user("bdomingu")$dataset("irw_meta")$table("tags").
#' Only fetches new data if the table version tag has changed.
#' Filters out any tags referring to tables that do not exist in the IRW database.
#'
#' @return A tibble containing filtered tags information.
#' @keywords internal
.fetch_tags_table <- function() {
  dataset <- redivis::redivis$user("bdomingu")$dataset("irw_meta:bdxt")
  dataset$get()
  latest_version_tag <- dataset$properties$version$tag
  
  # Return cached if version unchanged
  if (!is.null(latest_version_tag) &&
      exists("tags_tibble", envir = .irw_env) &&
      identical(.irw_env$tags_version, latest_version_tag)) {
    return(.irw_env$tags_tibble)
  }
  
  # Fetch and clean tags table
  table <- dataset$table("tags:7nkh")
  tags_tibble <- .retry_with_backoff(function() table$to_tibble())
  
  tags_tibble <- as.data.frame(tags_tibble)
  tags_tibble[] <- lapply(tags_tibble, function(col) {
    if (is.character(col)) col[col == "NA"] <- NA
    col
  })
  tags_tibble <- tibble::as_tibble(tags_tibble)
  
  # Get all table names from all datasets
  ds_list <- .initialize_datasource(sim = FALSE)
  table_name_list <- tolower(unlist(lapply(ds_list, function(ds) {
    ds$get()  
    vapply(ds$list_tables(), function(tbl) tbl$name, character(1))
  })))
  
  # Filter tags table
  tags_tibble$table_lower <- tolower(tags_tibble$table)
  filtered_tags <- tags_tibble[tags_tibble$table_lower %in% table_name_list, ]
  filtered_tags$table_lower <- NULL
  
  # Cache
  .irw_env$tags_version <- latest_version_tag
  .irw_env$tags_tibble <- filtered_tags
  
  return(filtered_tags)
}


#' Access the IRW item text dataset object
#'
#' Returns the Redivis dataset object for IRW item text metadata,
#' and ensures metadata is loaded via \code{get()}.
#'
#' @return A Redivis dataset object.
#' @keywords internal
.get_irw_itemtext_dataset <- function() {
  if (!exists("itemtext_dataset", envir = .irw_env) || is.null(.irw_env$itemtext_dataset)) {
    dataset <- redivis::redivis$user("bdomingu")$dataset("irw_text:07b6")
    dataset$get() 
    .irw_env$itemtext_dataset <- dataset
  }
  .irw_env$itemtext_dataset
}


#' Fetch item text table from IRW Redivis dataset 
#'
#' Retrieves an item text table from the IRW Redivis text dataset,
#' given a known base table name. If not available, it returns `NULL` with a message.
#'
#' @param table_name Character. The base table name (e.g., "gilbert_meta_49").
#' @return A tibble with item text metadata or `NULL`.
#' @keywords internal
.fetch_itemtext_table <- function(table_name) {
  available_tables <- irw_list_itemtext_tables()
  
  if (!(table_name %in% available_tables)) {
    message(glue::glue("Item text not available for table: '{table_name}'"))
    return(NULL)
  }
  
  dataset <- .get_irw_itemtext_dataset()
  full_name <- paste0(table_name, "__items")
  
  table <- dataset$table(full_name)
  suppressWarnings(.retry_with_backoff(function() table$to_tibble()))
}

