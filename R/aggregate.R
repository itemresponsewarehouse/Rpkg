#' Run a Redivis SQL query and return a tibble
#'
#' Server-side aggregates return a handful of rows and are not charged against
#' the Redivis export quota, unlike \code{table$to_tibble()}, which materialises
#' every row of the table.
#'
#' @param sql A single SQL string.
#' @return A tibble with the query result.
#' @keywords internal
#' @noRd
.irw_query_tibble <- function(sql) {
  tryCatch(
    suppressWarnings(redivis::redivis$query(sql)$to_tibble()),
    error = function(e) {
      msg <- conditionMessage(e)
      err_type <- .irw_redivis_error_type(msg)
      if (err_type == "auth") stop(.irw_auth_error_message(), call. = FALSE)
      if (err_type == "quota") stop(.irw_quota_error_message(msg), call. = FALSE)
      stop(paste("\nAn error occurred while querying IRW:", .irw_sanitize_redivis_error(msg)),
           call. = FALSE)
    }
  )
}

#' Variable names of a Redivis table, without downloading it
#'
#' @param tbl A Redivis table object.
#' @return Character vector of variable names.
#' @keywords internal
#' @noRd
.irw_table_variable_names <- function(tbl) {
  vars <- tbl$list_variables()
  vapply(vars, function(v) v$name, character(1))
}

#' Summarize the value sets of an IRW table without downloading it
#'
#' Answers set and summary questions about a table -- which item codes it
#' contains, which response values occur, how many rows each item has -- using
#' server-side aggregate queries rather than exporting the table. This matters
#' for large tables: computing the item set of a 68-million-row table with
#' \code{irw_fetch()} downloads all 68 million rows, while
#' \code{irw_table_sets()} returns the same answer in seconds and does not
#' consume the Redivis export quota.
#'
#' @param name Character. Name of a single IRW table.
#' @param source Character. Data source: \code{"core"} (default), \code{"nom"},
#'   \code{"sim"}, or \code{"comp"}.
#' @param per_item Logical. If TRUE, also return a per-item summary (row count,
#'   response minimum, maximum, and number of distinct response values). One
#'   extra query; the result has one row per distinct item. Defaults to FALSE.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{table}{Fully qualified Redivis reference for the table.}
#'     \item{n_rows}{Total number of rows.}
#'     \item{items}{Sorted character vector of distinct \code{item} values.}
#'     \item{resp}{Sorted vector of distinct \code{resp} values, numeric when
#'       all values are numeric and character otherwise. \code{"NA"} and empty
#'       strings are treated as missing, matching \code{irw_fetch()}.}
#'     \item{per_item}{Data frame of per-item summaries, or \code{NULL} when
#'       \code{per_item = FALSE}.}
#'   }
#'
#' @examples
#' \dontrun{
#' sets <- irw_table_sets("rosenberg_selfesteem")
#' sets$items
#' sets$resp
#'
#' irw_table_sets("condon_2024_sapa_personality", per_item = TRUE)$per_item
#' }
#'
#' @export
irw_table_sets <- function(name, source = "core", per_item = FALSE) {
  if (!is.character(name) || length(name) != 1L) {
    stop("`name` must be a single table name.", call. = FALSE)
  }
  source <- .irw_resolve_source(source = source)

  tbl <- .fetch_redivis_table(name, source = source)
  ref <- tbl$qualified_reference
  vars <- .irw_table_variable_names(tbl)

  counts <- .irw_query_tibble(sprintf("SELECT COUNT(*) AS n FROM `%s`", ref))

  items <- NULL
  if ("item" %in% vars) {
    items <- .irw_query_tibble(sprintf(
      "SELECT DISTINCT CAST(item AS STRING) AS item FROM `%s` WHERE item IS NOT NULL ORDER BY item",
      ref
    ))$item
  }

  # `resp` is stored as a string and carries a literal "NA" token; drop it here
  # so the value set matches what irw_fetch() produces after coercion.
  not_missing <- "resp IS NOT NULL AND TRIM(CAST(resp AS STRING)) NOT IN ('NA', '')"

  resp <- NULL
  if ("resp" %in% vars) {
    resp_raw <- .irw_query_tibble(sprintf(
      "SELECT DISTINCT TRIM(CAST(resp AS STRING)) AS resp FROM `%s` WHERE %s ORDER BY resp",
      ref, not_missing
    ))$resp
    resp <- .irw_coerce_resp_set(resp_raw, source = source)
  }

  per_item_df <- NULL
  if (isTRUE(per_item) && "item" %in% vars) {
    per_item_df <- as.data.frame(.irw_query_tibble(sprintf(
      paste(
        "SELECT CAST(item AS STRING) AS item, COUNT(*) AS n,",
        "MIN(SAFE_CAST(TRIM(CAST(resp AS STRING)) AS FLOAT64)) AS resp_min,",
        "MAX(SAFE_CAST(TRIM(CAST(resp AS STRING)) AS FLOAT64)) AS resp_max,",
        "COUNT(DISTINCT TRIM(CAST(resp AS STRING))) AS n_resp_levels",
        "FROM `%s` WHERE %s GROUP BY item ORDER BY item"
      ),
      ref, not_missing
    )))
  }

  list(
    table = ref,
    n_rows = counts$n[1],
    items = items,
    resp = resp,
    per_item = per_item_df
  )
}

#' Coerce a distinct-response value set to numeric when possible
#'
#' @param x Character vector of distinct response values.
#' @param source Resolved source name; nominal responses stay character.
#' @keywords internal
#' @noRd
.irw_coerce_resp_set <- function(x, source = "core") {
  if (source == "nom" || length(x) == 0L) {
    return(x)
  }
  suppressWarnings(num <- as.numeric(x))
  if (anyNA(num)) {
    return(sort(x))
  }
  sort(num)
}
