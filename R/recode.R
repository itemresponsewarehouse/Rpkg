# Default code prefix for a column: 'id' -> "P" (person), 'item' -> "I",
# anything else -> uppercased first letter of the column name.
.irw_recode_prefix <- function(col) {
  if (identical(col, "id")) return("P")
  if (identical(col, "item")) return("I")
  toupper(substr(col, 1L, 1L))
}

#' Recode IRW Identifier Columns to Simple Sequential Codes
#'
#' Replaces the values of identifier columns (by default \code{id} and
#' \code{item}) with short ASCII codes such as \code{P0001} and \code{I0001}.
#' This is useful when identifiers are in a script or use characters that are
#' awkward to type on a US keyboard, which makes subsetting, writing model
#' formulas, and reading wide-format column names difficult.
#'
#' @details
#' Codes are assigned in sorted order of the unique non-missing values of each
#' column, so they are deterministic for a given set of values. They are,
#' however, only meaningful relative to their key: two different subsets of the
#' same table will generally not produce the same codes.
#'
#' The mapping is attached to the result as an attribute and can be retrieved
#' with \code{irw_recode_key()}. Save it if you need it later, because most
#' base R operations on a data frame drop attributes. The original values can
#' be restored with \code{irw_decode()}.
#'
#' Note that recoding \code{item} breaks manual joins against
#' \code{irw_itemtext()} output, which is keyed on the original item
#' identifiers. Use \code{irw_recode_key()} to map back before joining.
#'
#' @param df A data frame in IRW long format.
#' @param cols Character vector of columns to recode. Defaults to
#'   \code{c("id", "item")}.
#' @param prefix Optional named character vector overriding the default code
#'   prefixes, e.g. \code{c(item = "Q")}. Defaults are \code{"P"} for
#'   \code{id}, \code{"I"} for \code{item}, and the uppercased first letter of
#'   the column name otherwise.
#'
#' @return \code{df} with the requested columns replaced by character codes and
#'   an \code{"irw_recode_key"} attribute holding a data frame with columns
#'   \code{column}, \code{original}, and \code{code}.
#'
#' @seealso \code{\link{irw_decode}}, \code{\link{irw_recode_key}}
#'
#' @examples
#' df <- data.frame(
#'   id = c("María-01", "María-01", "René-02"),
#'   item = c("Q_alpha", "Q_beta", "Q_alpha"),
#'   resp = c(1, 0, 1),
#'   stringsAsFactors = FALSE
#' )
#' d <- irw_recode(df)
#' head(d)
#' irw_recode_key(d)
#' identical(irw_decode(d)$item, df$item)
#'
#' @export
irw_recode <- function(df, cols = c("id", "item"), prefix = NULL) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (!is.character(cols) || length(cols) == 0L) {
    stop("`cols` must be a non-empty character vector of column names.",
         call. = FALSE)
  }
  if (!is.null(prefix)) {
    if (!is.character(prefix) || is.null(names(prefix)) || any(names(prefix) == "")) {
      stop("`prefix` must be a named character vector, e.g. c(item = \"Q\").",
           call. = FALSE)
    }
  }

  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L) {
    stop("Missing required IRW columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  messages <- character(0)
  key_new <- NULL

  for (col in cols) {
    # enc2utf8() normalises strings that come back from files or databases
    # marked as "unknown" encoding, which radix sorting rejects.
    x <- enc2utf8(as.character(df[[col]]))
    # Radix sort keeps the ordering (and therefore the codes) independent
    # of the user's locale.
    vals <- sort(unique(x[!is.na(x)]), method = "radix")

    if (length(vals) == 0L) {
      messages <- c(messages,
                    paste0("Column '", col, "' has no non-missing values; left unchanged."))
      next
    }

    pfx <- if (!is.null(prefix) && col %in% names(prefix)) {
      prefix[[col]]
    } else {
      .irw_recode_prefix(col)
    }

    w <- max(4L, nchar(as.character(length(vals))))
    codes <- sprintf("%s%0*d", pfx, w, seq_along(vals))

    df[[col]] <- codes[match(x, vals)]

    key_new <- rbind(
      key_new,
      data.frame(
        column = col,
        original = vals,
        code = codes,
        stringsAsFactors = FALSE
      )
    )

    messages <- c(
      messages,
      paste0("Recoded '", col, "': ", length(vals), " unique values -> ",
             codes[1L], "..", codes[length(codes)])
    )
  }

  # Carry forward any key from a previous irw_recode() call, dropping rows for
  # columns recoded again here.
  key_old <- attr(df, "irw_recode_key", exact = TRUE)
  if (!is.null(key_old) && !is.null(key_new)) {
    key_old <- key_old[!key_old$column %in% key_new$column, , drop = FALSE]
    key_new <- rbind(key_old, key_new)
  } else if (is.null(key_new)) {
    key_new <- key_old
  }

  rownames(key_new) <- NULL
  attr(df, "irw_recode_key") <- key_new

  messages <- c(
    messages,
    paste0("Save the mapping with irw_recode_key() if you need it later; ",
           "most operations on a data frame drop attributes.")
  )
  message(paste(messages, collapse = "\n"))

  df
}

#' Retrieve the Recode Key from an Object Returned by irw_recode()
#'
#' @param x An object returned by \code{irw_recode()}.
#'
#' @return A data frame with columns \code{column}, \code{original}, and
#'   \code{code}.
#'
#' @seealso \code{\link{irw_recode}}, \code{\link{irw_decode}}
#'
#' @export
irw_recode_key <- function(x) {
  key <- attr(x, "irw_recode_key", exact = TRUE)
  if (is.null(key)) {
    stop("No recode key found. `x` should be the result of irw_recode().",
         call. = FALSE)
  }
  key
}

#' Restore Original Identifier Values
#'
#' Inverse of \code{irw_recode()}: maps codes such as \code{I0001} back to the
#' original identifier values using the recode key.
#'
#' @details
#' Works on long-format data (columns named in the key are decoded in place) and
#' on the wide output of \code{irw_long2resp()}, where item codes appear as
#' column names. \code{irw_long2resp()} prefixes item columns with
#' \code{item_}; that prefix is handled and preserved. Values or column names
#' that are not found in the key are left untouched.
#'
#' @param x A data frame returned by \code{irw_recode()}, or derived from one
#'   (including the wide result of \code{irw_long2resp()}).
#' @param key A recode key as returned by \code{irw_recode_key()}. Defaults to
#'   the key attached to \code{x}; supply it explicitly if the attribute was
#'   dropped.
#' @param cols Character vector of columns to decode. Defaults to all columns
#'   present in both \code{x} and the key.
#'
#' @return \code{x} with codes replaced by the original values and the
#'   \code{"irw_recode_key"} attribute removed.
#'
#' @seealso \code{\link{irw_recode}}, \code{\link{irw_recode_key}}
#'
#' @export
irw_decode <- function(x, key = NULL, cols = NULL) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }
  if (is.null(key)) {
    key <- attr(x, "irw_recode_key", exact = TRUE)
  }
  if (is.null(key)) {
    stop("No recode key found. Pass `key = irw_recode_key(<recoded data>)` ",
         "or use an object returned by irw_recode().", call. = FALSE)
  }
  if (!is.data.frame(key) ||
      !all(c("column", "original", "code") %in% names(key))) {
    stop("`key` must be a data frame with columns 'column', 'original', ",
         "and 'code'.", call. = FALSE)
  }

  messages <- character(0)

  cols_arg <- cols
  if (is.null(cols)) {
    cols <- intersect(unique(key$column), names(x))
  } else {
    unknown <- setdiff(cols, names(x))
    if (length(unknown) > 0L) {
      stop("Missing required IRW columns: ", paste(unknown, collapse = ", "),
           call. = FALSE)
    }
  }

  auto_cols <- is.null(cols_arg)

  for (col in cols) {
    k <- key[key$column == col, , drop = FALSE]
    if (nrow(k) == 0L) next
    vals <- as.character(x[[col]])
    idx <- match(vals, k$code)
    n_unmatched <- sum(is.na(idx) & !is.na(vals))
    if (n_unmatched > 0L) {
      messages <- c(
        messages,
        paste0(n_unmatched, " value(s) in '", col,
               "' were not found in the key and were left unchanged.")
      )
    }
    x[[col]] <- ifelse(is.na(idx), vals, k$original[idx])
  }

  # Wide format (e.g. the output of irw_long2resp()): item codes appear as
  # column names, optionally prefixed with "item_".
  decoded_names <- FALSE
  if (auto_cols && !"item" %in% cols) {
    k <- key[key$column == "item", , drop = FALSE]
    if (nrow(k) > 0L) {
      nms <- names(x)
      has_prefix <- grepl("^item_", nms)
      idx <- match(sub("^item_", "", nms), k$code)
      n_matched <- sum(!is.na(idx))
      if (n_matched > 0L) {
        names(x) <- ifelse(
          is.na(idx),
          nms,
          paste0(ifelse(has_prefix, "item_", ""), k$original[idx])
        )
        decoded_names <- TRUE
      }
    }
  }

  if (length(cols) == 0L && !decoded_names) {
    stop("Nothing to decode: no columns of `x` match the key.", call. = FALSE)
  }

  attr(x, "irw_recode_key") <- NULL

  if (length(messages) > 0L) {
    message(paste(messages, collapse = "\n"))
  }

  x
}
