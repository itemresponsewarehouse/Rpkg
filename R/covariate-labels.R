# Covariate value labels (ben-domingue/irw#1775).
#
# Many covariates ship as bare codes (cov_gender in {1, 2}) whose meaning the
# source file recorded as value labels and the IRW table does not, and the
# meaning differs by table: cucchi_2018_rfq codes 1 = female,
# rowe_2016_cfs_strain 1 = male. irw_meta's `covariate_labels` table holds the
# source's own labels, long: table, covariate, code, label, all character.
# Reading them is opt-in -- irw_fetch() never decodes.

.irw_covariate_label_cols <- c("table", "covariate", "code", "label")

# A classed error, so callers (and tests) can tell "this irw_meta has no labels
# table" from a failed read.
.irw_covariate_labels_unavailable <- function(msg) {
  structure(
    class = c("irw_covariate_labels_unavailable", "error", "condition"),
    list(message = msg, call = NULL)
  )
}

#' Fetch the covariate_labels table from irw_meta (cached per irw_meta version)
#'
#' Checks the dataset's table listing first: an irw_meta version without the
#' table (every version before ben-domingue/irw#1775, or a pinned older one) is
#' an expected state, and must not be confused with a read that failed.
#'
#' @return A tibble with character columns table, covariate, code, label.
#' @keywords internal
.fetch_covariate_labels_table <- function() {
  dataset <- .irw_open_meta_dataset()
  version_tag <- dataset$properties$version$tag

  if (!is.null(version_tag) &&
      exists("covariate_labels_tibble", envir = .irw_env) &&
      identical(.irw_env$covariate_labels_version, version_tag)) {
    return(.irw_env$covariate_labels_tibble)
  }

  tables <- .retry_with_backoff(function() dataset$list_tables())
  nms <- vapply(tables, function(t) {
    nm <- t$name
    if (is.null(nm)) nm <- t$properties$name
    if (is.null(nm)) "" else as.character(nm)
  }, character(1))
  if (!"covariate_labels" %in% tolower(nms)) {
    stop(.irw_covariate_labels_unavailable(paste0(
      "This version of irw_meta (",
      if (is.null(version_tag)) "unknown version" else version_tag,
      ") has no `covariate_labels` table, so covariate value labels are not ",
      "available. The table is new (ben-domingue/irw#1775): older irw_meta ",
      "versions do not carry it, so a session pinned to one with ",
      "irw_set_version() cannot decode covariates."
    )))
  }

  out <- .retry_with_backoff(function() dataset$table("covariate_labels")$to_tibble())
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  missing_cols <- setdiff(.irw_covariate_label_cols, names(out))
  if (length(missing_cols) > 0L) {
    stop(.irw_covariate_labels_unavailable(paste0(
      "irw_meta's `covariate_labels` table lacks column(s): ",
      paste(missing_cols, collapse = ", "), "."
    )))
  }
  out <- out[, .irw_covariate_label_cols, drop = FALSE]
  out[] <- lapply(out, as.character)
  out <- tibble::as_tibble(out)

  .irw_env$covariate_labels_version <- version_tag
  .irw_env$covariate_labels_tibble <- out
  out
}

# The shipped value written as `covariate_labels$code` writes it: a shipped
# 1.0 is "1", as is 1L or "1". NA stays NA.
.irw_code_text <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) return(ifelse(is.na(x), NA_character_, as.character(x)))
  if (is.numeric(x)) {
    out <- ifelse(
      is.na(x), NA_character_,
      ifelse(x == round(x), format(round(x), scientific = FALSE, trim = TRUE),
             as.character(x))
    )
    return(out)
  }
  x <- trimws(as.character(x))
  sub("^(-?[0-9]+)\\.0+$", "\\1", x)
}

# Numeric codes in numeric order, then any others alphabetically.
.irw_order_codes <- function(codes) {
  num <- suppressWarnings(as.numeric(codes))
  codes[order(is.na(num), num, codes)]
}

#' Source Value Labels for an IRW Table's Coded Covariates
#'
#' Many covariates ship as bare codes -- \code{cov_gender} in \{1, 2\} -- whose
#' meaning was recorded in the source file (an SPSS or Stata value label) but
#' is not in the IRW table, and the meaning differs from table to table:
#' \code{cucchi_2018_rfq} codes 1 = female, \code{rowe_2016_cfs_strain}
#' 1 = male. This returns the source's own labels, one row per code, from
#' irw_meta's \code{covariate_labels} table. It does not touch the response
#' data, and \code{irw_fetch()} output is unchanged.
#'
#' @param table Character vector of IRW table names (case-insensitive).
#'
#' @return A tibble with character columns \code{table}, \code{covariate},
#'   \code{code}, \code{label}, one row per code, sorted by table, covariate
#'   and code. \code{code} is the shipped value written as text (a shipped
#'   \code{1.0} is \code{"1"}). \code{label} is the source's wording, verbatim
#'   and not harmonised across tables, or the literal
#'   \code{"[institution name withheld]"} where the codes name institutions.
#'   Only codes that occur in the shipped column are listed, and a covariate
#'   may be partly covered. Zero rows, with a message, when IRW has no labels
#'   for the table.
#'
#'   If the irw_meta version in use has no \code{covariate_labels} table (an
#'   older or pinned version), this is an error of class
#'   \code{irw_covariate_labels_unavailable}, not an empty result, so it cannot
#'   be mistaken for "no labelled covariates".
#'
#' @seealso \code{\link{irw_covariates}}, whose \code{labels = TRUE} applies
#'   these labels.
#'
#' @examplesIf requireNamespace("redivis", quietly = TRUE)
#' \donttest{
#'   irw_covariate_labels("cucchi_2018_rfq")
#' }
#'
#' @export
irw_covariate_labels <- function(table) {
  if (missing(table) || !is.character(table) || length(table) == 0L) {
    stop("`table` must be a character vector of IRW table names.", call. = FALSE)
  }
  rows <- .fetch_covariate_labels_table()
  out <- rows[tolower(rows$table) %in% tolower(table), , drop = FALSE]
  if (nrow(out) == 0L) {
    message(
      "No covariate value labels in IRW for: ", paste(table, collapse = ", "),
      ". Either its covariates are not coded, or their source labels could ",
      "not be recovered."
    )
  }
  num <- suppressWarnings(as.numeric(out$code))
  out <- out[order(out$table, out$covariate, is.na(num), num, out$code), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The label rows irw_covariates(labels = ...) should apply, for one table.
.irw_resolve_label_rows <- function(df, labels, table) {
  if (is.null(table) && "source_table" %in% names(df)) {
    tabs <- unique(stats::na.omit(as.character(df$source_table)))
    if (length(tabs) == 1L) {
      table <- tabs
    } else if (length(tabs) > 1L) {
      stop("`df` holds rows from several tables (`source_table`), and the same ",
           "code can mean different things in each. Decode one table at a ",
           "time: subset `df` to one source_table, or pass `table`.",
           call. = FALSE)
    }
  }
  if (is.data.frame(labels)) {
    missing_cols <- setdiff(c("covariate", "code", "label"), names(labels))
    if (length(missing_cols) > 0L) {
      stop("`labels` as a data frame needs the columns covariate, code, label ",
           "(as irw_covariate_labels() returns); missing: ",
           paste(missing_cols, collapse = ", "), ".", call. = FALSE)
    }
    if ("table" %in% names(labels)) {
      if (!is.null(table)) {
        labels <- labels[tolower(labels$table) == tolower(table), , drop = FALSE]
      } else if (length(unique(labels$table)) > 1L) {
        stop("`labels` covers several tables; pass `table` to say which one ",
             "`df` is.", call. = FALSE)
      }
    }
    return(labels)
  }
  if (isTRUE(labels)) {
    if (is.null(table)) {
      stop("labels = TRUE needs to know which IRW table `df` came from: pass ",
           "`table` (e.g. irw_covariates(df, labels = TRUE, ",
           "table = \"cucchi_2018_rfq\")).", call. = FALSE)
    }
    return(irw_covariate_labels(table))
  }
  stop("`labels` must be TRUE, FALSE, or a data frame from ",
       "irw_covariate_labels().", call. = FALSE)
}

# Turn each covariate in `cols` that has label rows into a factor. Returns the
# data frame and the messages to show.
.irw_apply_labels <- function(out, cols, rows) {
  decoded <- character(0)
  kept <- character(0)
  partial <- character(0)
  for (cl in cols) {
    these <- rows[rows$covariate == cl, , drop = FALSE]
    if (nrow(these) == 0L) next
    labs <- stats::setNames(as.character(these$label), .irw_code_text(these$code))
    labs <- labs[!duplicated(names(labs))]
    if (anyDuplicated(unname(labs))) {
      # Several codes share one label -- the institution-withheld rows, where
      # every school reads "[institution name withheld]". Decoding would merge
      # distinct groups into one, so leave the codes.
      kept <- c(kept, cl)
      next
    }
    keys <- .irw_code_text(out[[cl]])
    present <- unique(keys[!is.na(keys)])
    unlabelled <- .irw_order_codes(setdiff(present, names(labs)))
    if (any(unlabelled %in% unname(labs))) {
      kept <- c(kept, cl)
      next
    }
    codes <- .irw_order_codes(union(names(labs), unlabelled))
    lvls <- ifelse(codes %in% names(labs), labs[codes], codes)
    vals <- ifelse(is.na(keys), NA_character_,
                   ifelse(keys %in% names(labs), labs[keys], keys))
    out[[cl]] <- factor(unname(vals), levels = unname(lvls))
    decoded <- c(decoded, cl)
    if (length(unlabelled) > 0L) {
      partial <- c(partial, paste0(cl, " (", paste(unlabelled, collapse = ", "), ")"))
    }
  }

  messages <- character(0)
  if (length(decoded) > 0L) {
    messages <- c(messages, paste0("Decoded with source labels: ",
                                   paste(decoded, collapse = ", ")))
  }
  if (length(partial) > 0L) {
    messages <- c(messages, paste0(
      "Codes with no source label, kept as their code text: ",
      paste(partial, collapse = "; ")))
  }
  if (length(kept) > 0L) {
    messages <- c(messages, paste0(
      "Left as codes (several codes share one label, e.g. institution names ",
      "withheld): ", paste(kept, collapse = ", ")))
  }
  list(out = out, messages = messages)
}
