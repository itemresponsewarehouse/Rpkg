# Response-level columns of the IRW long format. Everything else is a
# candidate person-level column.
.irw_response_cols <- c("item", "resp", "rater", "rt", "text", "date",
                        "source_table")

# TRUE if `col` takes a single value within every id.
.irw_is_constant_within_id <- function(df, col, id_col = "id") {
  pairs <- unique(df[, c(id_col, col), drop = FALSE])
  !anyDuplicated(pairs[[id_col]])
}

#' Extract Person-Level Covariates from IRW Long-Format Data
#'
#' Reduces IRW long-format data to one row per \code{id}, keeping the columns
#' that describe the person rather than the response. Optionally aligns those
#' rows to the row order of a wide response matrix, which is what most
#' psychometric packages need when a grouping variable is passed alongside a
#' response matrix.
#'
#' @details
#' \code{irw_long2resp()} keeps only \code{id}, \code{item}, and the response
#' column, so person-level covariates are dropped and the row order of its
#' result is not sorted. Aligning a covariate to that result by hand requires a
#' \code{match()} on \code{id}, and getting it wrong silently misassigns people
#' to groups. Passing the wide result as \code{align} does that match for you.
#'
#' When \code{cols} is \code{NULL}, the person-level columns are detected
#' automatically: every column other than \code{id} and the response-level
#' columns (\code{item}, \code{resp}, \code{rater}, \code{rt}, \code{text},
#' \code{date}, \code{source_table}) that takes exactly one value within every
#' \code{id}. A column with a value for some of a person's rows and \code{NA}
#' for others counts as varying and is not selected. Note that \code{wave} is
#' treated as person-level only when each person appears in a single wave.
#'
#' @param df A data frame in IRW long format, containing an \code{id} column.
#' @param cols Character vector of person-level columns to extract. Defaults to
#'   \code{NULL}, meaning detect them automatically as described above.
#' @param align Optional. A data frame with an \code{id} column (such as the
#'   result of \code{irw_long2resp()}), a matrix with \code{id} rownames, or a
#'   vector of ids. The returned rows are put in this order, one row per
#'   element, so the result lines up with the rows of a wide response matrix.
#'
#' @return A data frame with one row per \code{id} and columns \code{id}
#'   followed by the person-level covariates. When \code{align} is supplied,
#'   rows follow that order and ids not present in \code{df} yield \code{NA}
#'   rows.
#'
#' @seealso \code{\link{irw_long2resp}}
#'
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 2, 2),
#'   item = c("i1", "i2", "i1", "i2"),
#'   resp = c(1, 0, 1, 1),
#'   cov_group = c("A", "A", "B", "B"),
#'   stringsAsFactors = FALSE
#' )
#' irw_covariates(df)
#'
#' # Aligned to the row order of a wide response matrix
#' wide <- irw_long2resp(df, id_density_threshold = NULL)
#' irw_covariates(df, align = wide)
#'
#' @export
irw_covariates <- function(df, cols = NULL, align = NULL) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (!"id" %in% names(df)) {
    stop("Missing required IRW columns: id", call. = FALSE)
  }

  messages <- character(0)

  if (is.null(cols)) {
    candidates <- setdiff(names(df), c("id", .irw_response_cols))
    keep <- vapply(
      candidates,
      function(cl) .irw_is_constant_within_id(df, cl),
      logical(1L)
    )
    cols <- candidates[keep]
    dropped <- candidates[!keep]
    if (length(dropped) > 0L) {
      messages <- c(
        messages,
        paste0("Not person-level (varies within id), so not returned: ",
               paste(dropped, collapse = ", "))
      )
    }
    if (length(cols) == 0L) {
      messages <- c(
        messages,
        "No person-level covariates found; returning ids only."
      )
    }
  } else {
    if (!is.character(cols)) {
      stop("`cols` must be a character vector of column names.", call. = FALSE)
    }
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0L) {
      stop("Missing required IRW columns: ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    varying <- cols[!vapply(
      cols,
      function(cl) .irw_is_constant_within_id(df, cl),
      logical(1L)
    )]
    if (length(varying) > 0L) {
      stop("These columns are not person-level (they vary within id): ",
           paste(varying, collapse = ", "),
           ". A person-level covariate must take one value per id.",
           call. = FALSE)
    }
  }

  first <- !duplicated(df$id)
  out <- df[first, c("id", cols), drop = FALSE]
  rownames(out) <- NULL

  if (!is.null(align)) {
    ids <- .irw_align_ids(align)
    idx <- match(ids, out$id)
    n_missing <- sum(is.na(idx))
    if (n_missing > 0L) {
      messages <- c(
        messages,
        paste0(n_missing, " id(s) in `align` were not found in `df`; ",
               "those rows are NA.")
      )
    }
    out <- out[idx, , drop = FALSE]
    out$id <- ids
    rownames(out) <- NULL
  }

  if (length(messages) > 0L) {
    message(paste(messages, collapse = "\n"))
  }

  out
}

# Pull the vector of ids out of whatever `align` was supplied as.
.irw_align_ids <- function(align) {
  if (is.data.frame(align)) {
    if (!"id" %in% names(align)) {
      stop("`align` is a data frame without an `id` column. Supply the ",
           "result of irw_long2resp(), or pass a vector of ids.",
           call. = FALSE)
    }
    return(align$id)
  }
  if (is.matrix(align)) {
    nm <- rownames(align)
    if (is.null(nm)) {
      stop("`align` is a matrix without rownames to use as ids. Supply a ",
           "vector of ids instead.", call. = FALSE)
    }
    return(nm)
  }
  if (is.atomic(align)) {
    return(align)
  }
  stop("`align` must be a data frame with an `id` column, a matrix with ",
       "rownames, or a vector of ids.", call. = FALSE)
}
