#' Compress the positions of a rankings table
#'
#' Rankings tables (\code{source = "rank"}) keep the core column names, but
#' \code{resp} is a \emph{position}: one row per respondent (\code{id}), ranking
#' (\code{task}) and marked position, \code{resp = 1} for the first choice,
#' \strong{as recorded}. Two things in the stored table need care before a
#' person-by-item pivot or a Plackett-Luce fit: a skipped position stays a gap
#' (a ballot marked at positions 1 and 3 keeps \code{resp} 1 and 3), and a
#' candidate marked twice keeps both rows. Counting rules for both differ by
#' jurisdiction, so the table does not apply one; this function applies the
#' common one.
#'
#' Within each \code{id} x \code{task}, rows are taken in \code{resp} order.
#' With \code{drop_repeats = TRUE} an item that has already been marked at an
#' earlier position is dropped at every later one (most counting rules treat
#' the later mark as a skip). The reserved items \code{(write-in)} and
#' \code{(overvote)} are never dropped: two write-in rows are usually two
#' different people, and two overvote rows are two separate events. Then
#' \code{resp} is renumbered \code{1..k} in rank order, so the gaps close.
#' Every other column is kept as it was. The result is sorted by \code{id},
#' \code{task}, \code{resp}.
#'
#' The Python package's \code{irw.rank_compress()} does exactly the same; the
#' two are tested on one shared fixture.
#'
#' @param df A rankings table, as returned by
#'   \code{irw_fetch(name, source = "rank")}: columns \code{id}, \code{item},
#'   \code{resp}, and \code{task} (taken as a single task when absent).
#' @param drop_repeats Logical. Drop an item's later marks within a ranking?
#'   Default \code{TRUE}.
#' @return A data frame with the same columns as \code{df}, \code{resp} an
#'   integer running \code{1..k} within each \code{id} x \code{task}.
#' @examples
#' d <- data.frame(id = c(1, 1, 1, 2), task = 1, item = c("c01", "c03", "c01", "c02"),
#'                 resp = c(1, 3, 4, 2))
#' irw_rank_compress(d)
#' irw_rank_compress(d, drop_repeats = FALSE)
#' \dontrun{
#' d <- irw_fetch("eurovision_2016_final", source = "rank")
#' first_choices <- subset(irw_rank_compress(d), resp == 1)
#' }
#' @seealso \code{\link{irw_fetch}}, \code{\link{irw_metadata}} with
#'   \code{source = "rank"} for each table's design record.
#' @export
irw_rank_compress <- function(df, drop_repeats = TRUE) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  need <- c("id", "item", "resp")
  missing <- setdiff(need, names(df))
  if (length(missing)) {
    stop("Not a rankings table: missing ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (!isTRUE(drop_repeats) && !isFALSE(drop_repeats)) {
    stop("`drop_repeats` must be TRUE or FALSE.", call. = FALSE)
  }
  resp <- suppressWarnings(as.numeric(df$resp))
  if (anyNA(resp) || any(resp < 1) || any(resp != round(resp))) {
    stop("`resp` must be positive integer positions in a rankings table.", call. = FALSE)
  }
  task <- if ("task" %in% names(df)) df$task else rep(1L, nrow(df))
  if (!nrow(df)) {
    df$resp <- integer(0)
    return(df)
  }

  ord <- order(df$id, task, resp)
  df <- df[ord, , drop = FALSE]
  task <- task[ord]
  group <- paste(df$id, task, sep = "\r")

  if (drop_repeats) {
    repeatable <- df$item %in% .irw_rank_reserved
    keep <- repeatable | !duplicated(paste(group, df$item, sep = "\r"))
    df <- df[keep, , drop = FALSE]
    group <- group[keep]
  }

  df$resp <- as.integer(stats::ave(seq_along(group), group, FUN = seq_along))
  rownames(df) <- NULL
  df
}

#' The reserved item values of a rankings table (data/rankings/README.md, "Layout")
#' @keywords internal
#' @noRd
.irw_rank_reserved <- c("(write-in)", "(overvote)")
