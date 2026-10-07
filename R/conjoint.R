#' Error for features the conjoint source does not support yet
#'
#' The conjoint source (\code{source = "conj"}) can be listed and fetched, but
#' its metadata and bibliography tables are not published yet, so anything that
#' reads them stops here with a message rather than failing obscurely.
#'
#' @keywords internal
#' @noRd
.irw_conj_not_yet <- function(what) {
  stop(what, " is not available for the conjoint source (source = \"conj\") yet: ",
       "its metadata and bibliography are not published. irw_list_tables(source = \"conj\") ",
       "and irw_fetch(..., source = \"conj\") work.", call. = FALSE)
}

#' Convert a conjoint table to the IRW long format
#'
#' Conjoint tables (\code{source = "conj"}) have one row per respondent, task
#' and profile: \code{id}, \code{task}, \code{profile}, the outcomes
#' (\code{choice}, \code{rating}, and any \code{choice_<name>} or
#' \code{rating_<name>}), the profile's attribute levels in \code{attr_*}
#' columns, and respondent covariates in \code{cov_*}. There is no \code{item}
#' or \code{resp}, because every profile is a new random bundle of attributes.
#'
#' This function stacks the outcomes into the core layout so that tools built
#' for \code{id}/\code{item}/\code{resp} can be used: \code{item} is the name of
#' the outcome question (\code{"choice"}, \code{"rating"}, ...), \code{resp} its
#' value, and \code{task}, \code{profile} and the attributes become
#' \code{trial_} columns. Rows with a missing outcome are dropped.
#'
#' @param df A conjoint table, as returned by
#'   \code{irw_fetch(name, source = "conj")}.
#' @param outcomes Optional character vector of outcome columns to keep. Default:
#'   all of them.
#' @return A data frame with columns \code{id}, \code{item}, \code{resp},
#'   \code{trial_task}, \code{trial_profile}, \code{trial_attr_*}, and the
#'   remaining columns of \code{df}.
#' @examples
#' \dontrun{
#' d <- irw_fetch("kreps_2020_covid_vaccine", source = "conj")
#' long <- irw_conj_long(d)
#' table(long$item)
#' }
#' @export
irw_conj_long <- function(df, outcomes = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  need <- c("id", "task", "profile")
  if (!all(need %in% names(df))) {
    stop("Not a conjoint table: missing ", paste(setdiff(need, names(df)), collapse = ", "), ".",
         call. = FALSE)
  }
  all_out <- grep("^(choice|rating)(_.+)?$", names(df), value = TRUE)
  if (is.null(outcomes)) outcomes <- all_out
  bad <- setdiff(outcomes, all_out)
  if (length(bad)) stop("Not outcome columns: ", paste(bad, collapse = ", "), call. = FALSE)
  if (!length(outcomes)) stop("No outcome columns (choice, rating, ...) in this table.", call. = FALSE)

  rest <- setdiff(names(df), c(all_out, "task", "profile"))
  attrs <- grep("^attr_", rest, value = TRUE)
  others <- setdiff(rest, c("id", attrs))
  pieces <- lapply(outcomes, function(o) {
    keep <- !is.na(df[[o]])
    out <- data.frame(id = df$id[keep], item = o, resp = as.numeric(df[[o]][keep]),
                      trial_task = df$task[keep], trial_profile = df$profile[keep],
                      stringsAsFactors = FALSE)
    for (a in attrs) out[[paste0("trial_", a)]] <- df[[a]][keep]
    for (x in others) out[[x]] <- df[[x]][keep]
    out
  })
  out <- do.call(rbind, pieces)
  out <- out[order(out$id, out$trial_task, out$trial_profile, out$item), , drop = FALSE]
  rownames(out) <- NULL
  out
}
