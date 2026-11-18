# The covariance family (source = "cov"): published correlation and covariance
# matrices, one row per cell of the lower triangle plus optional sd/mean rows.
# Standard: ben-domingue/irw data/covariance/README.md.
#
# Two things live here:
#   irw_cov_compute()  a core table (long id/item/resp) -> a matrix in the cov
#                      layout. irw_fetch(name, source = "cov") calls it. Derived
#                      matrices are never stored in the IRW (Ben, 2026-11-17), so
#                      this is the only way to get one for an IRW table.
#   irw_cov2matrix()   the cov layout -> a square matrix plus n, sd and mean,
#                      ready for lavaan(sample.cov =, sample.nobs =).
#   irw_cov2list()     every sample of a multi-group table, padded to common
#                      variables, ready for metaSEM::tssem1(Cov =, n =).

.irw_cov_cols <- c("stat", "var1", "var2", "value", "n")

#' Compute a correlation or covariance matrix from an IRW response table
#'
#' Turns a core table (the long `id`, `item`, `resp` layout from [irw_fetch()])
#' into an item-by-item matrix in the layout of the IRW covariance family: one
#' row per cell of the lower triangle, diagonal included, with columns `stat`
#' (`"cor"` or `"cov"`), `var1`, `var2`, `value` and `n` (the number of people
#' who answered both items). [irw_fetch()] with `source = "cov"` calls this on a
#' core table, and [irw_cov2matrix()] turns the result into a square matrix.
#'
#' The result is computed, not published: its attributes `irw_computed`,
#' `irw_method`, `irw_type`, `irw_use` and `irw_table` say how it was made, and
#' [irw_cov2matrix()] carries them forward. Cite the source table, not the
#' matrix.
#'
#' @param df A core IRW table in long format, with columns `id`, `item` and a
#'   numeric `resp`.
#' @param method The association measure: `"pearson"` (default), `"spearman"`,
#'   `"kendall"`, `"polychoric"` or `"tetrachoric"`. The last two need the
#'   \pkg{psych} package and give correlations only; `"tetrachoric"` needs every
#'   item dichotomous. Or a **function** of your own: it receives the wide
#'   person-by-item matrix (one column per item, `NA` where an item was not
#'   answered, after `use` is applied) and must return a square symmetric
#'   matrix with one row and column per item, in the same order. Say what it
#'   returns with `type`; `n` is still the pairwise count.
#' @param type `"cor"` (default) for a correlation matrix or `"cov"` for a
#'   covariance matrix. `"cov"` is available for `"pearson"`, `"spearman"` and
#'   `"kendall"`.
#' @param use How missing responses are handled: `"pairwise"` (default) uses,
#'   for each pair of items, everyone who answered both, and records that count
#'   in `n`; `"complete"` first drops anyone with a missing response, so `n` is
#'   the same everywhere. For polychoric and tetrachoric correlations,
#'   `"pairwise"` is what \pkg{psych} does internally.
#' @param max_items Refuse tables with more items than this (default 500). An
#'   item bank of 27,000 items has 380 million pairs, almost all answered by
#'   nobody in common.
#' @param table Optional table name, recorded in the `irw_table` attribute.
#'
#' @return A tibble with columns `stat`, `var1`, `var2`, `value` and `n`. A
#'   pair answered by fewer than three people in common has `value` `NA`.
#'
#' @details Tables with repeated measures (more than one response per
#'   `id` and `item`, for example across waves) are refused: filter to one
#'   occasion first, e.g. `df[df$wave == 1, ]`. Items with no variation have no
#'   correlation and are dropped with a warning.
#'
#' @seealso [irw_cov2matrix()], [irw_fetch()]
#' @examples
#' df <- data.frame(id = rep(1:50, each = 3), item = rep(c("a", "b", "c"), 50),
#'                  resp = c(replicate(50, sample(1:5, 3, replace = TRUE))))
#' irw_cov_compute(df)
#' irw_cov_compute(df, method = "spearman", type = "cov")
#' # Your own measure: here, a correlation shrunk towards zero
#' shrunk <- function(x) { r <- stats::cor(x, use = "pairwise.complete.obs"); 0.9 * r + 0.1 * diag(ncol(x)) }
#' irw_cov_compute(df, method = shrunk)
#' @export
irw_cov_compute <- function(df, method = c("pearson", "spearman", "kendall", "polychoric", "tetrachoric"),
                            type = c("cor", "cov"), use = c("pairwise", "complete"),
                            max_items = 500, table = NULL) {
  user_fun <- if (is.function(method)) method else NULL
  method <- if (is.null(user_fun)) match.arg(method) else "user-defined"
  type <- match.arg(type)
  use <- match.arg(use)
  if (type == "cov" && method %in% c("polychoric", "tetrachoric")) {
    stop("type = \"cov\" is not defined for ", method, " correlations; use type = \"cor\".", call. = FALSE)
  }
  miss <- setdiff(c("id", "item", "resp"), names(df))
  if (length(miss)) {
    stop("irw_cov_compute() needs a core table with id, item and resp; missing: ",
         paste(miss, collapse = ", "), ". Other families have no item-by-item matrix.", call. = FALSE)
  }
  df <- as.data.frame(df)[, c("id", "item", "resp")]
  df$resp <- suppressWarnings(as.numeric(df$resp))
  if (anyDuplicated(df[, c("id", "item")])) {
    stop("Some people answered an item more than once (repeated measures, or duplicate rows). ",
         "Filter to one occasion first, e.g. df[df$wave == 1, ], then call irw_cov_compute().", call. = FALSE)
  }
  items <- unique(as.character(df$item))
  if (length(items) > max_items) {
    stop(sprintf("This table has %d items; max_items is %d. A matrix this size is mostly pairs nobody answered together. ",
                 length(items), max_items), "Raise max_items if you mean it.", call. = FALSE)
  }
  ids <- unique(df$id)
  wide <- matrix(NA_real_, length(ids), length(items), dimnames = list(NULL, items))
  wide[cbind(match(df$id, ids), match(as.character(df$item), items))] <- df$resp
  if (use == "complete") wide <- wide[stats::complete.cases(wide), , drop = FALSE]
  sds <- apply(wide, 2, stats::sd, na.rm = TRUE)
  flat <- is.na(sds) | sds == 0
  if (any(flat)) {
    warning(sprintf("Dropped %d item(s) with no variation: %s", sum(flat),
                    paste(utils::head(items[flat], 5), collapse = ", ")), call. = FALSE)
    wide <- wide[, !flat, drop = FALSE]
  }
  if (ncol(wide) < 2) stop("Fewer than two items with variation; no matrix to compute.", call. = FALSE)
  obs <- !is.na(wide)
  npair <- crossprod(obs)
  storage.mode(npair) <- "integer"

  if (!is.null(user_fun)) {
    m <- user_fun(wide)
    k <- ncol(wide)
    if (!is.matrix(m) || !is.numeric(m) || !identical(dim(m), c(k, k))) {
      stop(sprintf("The method function must return a numeric %d x %d matrix (one row and column per item).", k, k),
           call. = FALSE)
    }
    if (!isTRUE(all.equal(unname(m), unname(t(m)), check.attributes = FALSE))) {
      stop("The method function returned a matrix that is not symmetric.", call. = FALSE)
    }
  } else if (method %in% c("polychoric", "tetrachoric")) {
    if (!requireNamespace("psych", quietly = TRUE)) {
      stop("method = \"", method, "\" needs the psych package: install.packages(\"psych\").", call. = FALSE)
    }
    if (method == "tetrachoric") {
      ncat <- apply(wide, 2, function(x) length(unique(x[!is.na(x)])))
      if (any(ncat != 2)) {
        stop("method = \"tetrachoric\" needs every item dichotomous; ", sum(ncat != 2),
             " item(s) are not. Use \"polychoric\".", call. = FALSE)
      }
      m <- suppressWarnings(suppressMessages(psych::tetrachoric(wide)))$rho
    } else {
      m <- suppressWarnings(suppressMessages(psych::polychoric(wide)))$rho
    }
  } else {
    if (type == "cov" && method != "pearson" && use == "pairwise") {
      # stats::cov() cannot do pairwise deletion for rank methods, so each pair
      # is computed on its own complete cases.
      k <- ncol(wide)
      m <- matrix(NA_real_, k, k, dimnames = list(colnames(wide), colnames(wide)))
      for (i in seq_len(k)) for (j in seq_len(i)) {
        ok <- obs[, i] & obs[, j]
        if (sum(ok) >= 3) m[i, j] <- m[j, i] <- stats::cov(wide[ok, i], wide[ok, j], method = method)
      }
    } else {
      fun <- if (type == "cov") stats::cov else stats::cor
      m <- fun(wide, use = if (use == "pairwise") "pairwise.complete.obs" else "everything", method = method)
    }
  }
  m[npair < 3] <- NA_real_
  v <- colnames(wide)
  idx <- which(lower.tri(m, diag = TRUE), arr.ind = TRUE)
  idx <- idx[order(idx[, "row"], idx[, "col"]), , drop = FALSE]
  out <- tibble::tibble(stat = type, var1 = v[idx[, "row"]], var2 = v[idx[, "col"]],
                        value = unname(m[idx]), n = unname(npair[idx]))
  attr(out, "irw_computed") <- TRUE
  attr(out, "irw_method") <- method
  attr(out, "irw_type") <- type
  attr(out, "irw_use") <- use
  attr(out, "irw_table") <- table
  out
}

# Published variable order (ben-domingue/irw#3594, Ben 2026-11-18). Redivis does
# not keep row order, so a published table's order lives in its record:
# cov_metadata's `variables`, names in published order separated by " | ", each
# optionally " = description". This sorts a fetched table's rows into that
# order, sample by sample: each sample's cells as the lower triangle row by row
# (var1 the later variable, var2 the earlier, so irw_cov2matrix() puts the
# variables in published order), then its sd rows, then its mean rows. Values
# are untouched. A table whose record has no `variables`, or whose names do not
# match the table, comes back as it was.
.irw_cov_parse_variables <- function(field) {
  if (is.null(field) || is.na(field) || !nzchar(trimws(field))) return(character(0))
  trimws(sub(" = .*$", "", strsplit(field, " | ", fixed = TRUE)[[1]]))
}

.irw_cov_order_rows <- function(df, meta, table) {
  if (is.null(meta) || !"variables" %in% names(meta) || !nrow(df)) return(df)
  rec <- meta[meta$table == table, , drop = FALSE]
  if (!nrow(rec)) return(df)
  recg <- as.character(rec$group); recg[is.na(recg)] <- ""
  g <- if ("group" %in% names(df)) as.character(df$group) else rep("", nrow(df))
  g[is.na(g)] <- ""
  stat <- as.character(df$stat); v1 <- as.character(df$var1); v2 <- as.character(df$var2)
  cell <- stat %in% c("cor", "cov")
  hi <- lo <- rep(NA_real_, nrow(df))
  for (gg in unique(g)) {
    v <- .irw_cov_parse_variables(rec$variables[match(gg, recg)])
    if (!length(v)) return(df)
    i <- which(g == gg)
    p1 <- match(v1[i], v); p2 <- match(v2[i], v)
    if (anyNA(p1) || anyNA(p2[cell[i]])) return(df)
    hi[i] <- ifelse(cell[i], pmax(p1, p2), p1)
    lo[i] <- ifelse(cell[i], pmin(p1, p2), 0)
  }
  # Orientation: var1 is the later variable of a cell, as the standard writes
  # the lower triangle. A cell is the same either way round.
  grp_rank <- match(g, unique(c(recg, g)))
  pos1 <- pos2 <- rep(NA_real_, nrow(df))
  for (gg in unique(g)) {
    v <- .irw_cov_parse_variables(rec$variables[match(gg, recg)])
    i <- which(g == gg)
    pos1[i] <- match(v1[i], v); pos2[i] <- match(v2[i], v)
  }
  swap <- cell & !is.na(pos2) & pos2 > pos1
  if (any(swap)) {
    tmp <- df$var1[swap]; df$var1[swap] <- df$var2[swap]; df$var2[swap] <- tmp
  }
  stat_rank <- ifelse(cell, 0, ifelse(stat == "sd", 1, 2))
  df[order(grp_rank, stat_rank, hi, lo), , drop = FALSE]
}

# The fewest variables to drop so a matrix has no NA off the diagonal. Exact
# (smallest set) when at most `exact_max` variables touch a blank, else greedy;
# ties keep the variables listed first (candidates are tried latest-first).
# Mirrors complete_drop() in ben-domingue/irw irw_validate/cov.py and
# metadata/22_cov.R, and complete_drop in the Python package: keep in step.
.irw_cov_complete_drop <- function(v, m, exact_max = 20) {
  idx <- which(is.na(m) & lower.tri(m), arr.ind = TRUE)
  if (!nrow(idx)) return(character(0))
  pairs <- lapply(seq_len(nrow(idx)), function(i) v[idx[i, ]])
  involved <- unique(unlist(pairs))
  involved <- involved[order(-match(involved, v))]
  covers <- function(drop) all(vapply(pairs, function(p) any(p %in% drop), logical(1)))
  if (length(involved) <= exact_max) {
    for (size in seq_along(involved)) {
      for (drop in utils::combn(involved, size, simplify = FALSE)) if (covers(drop)) return(v[v %in% drop])
    }
  }
  drop <- character(0); left <- pairs
  while (length(left)) {
    cnt <- vapply(involved, function(x) sum(vapply(left, function(p) x %in% p, logical(1))), numeric(1))
    best <- involved[order(-cnt, -match(involved, v))][1]
    drop <- c(drop, best)
    left <- Filter(function(p) !best %in% p, left)
  }
  v[v %in% drop]
}

#' Turn a covariance-family table into a matrix
#'
#' Rebuilds the square matrix from a table in the IRW covariance layout (from
#' `irw_fetch(..., source = "cov")` or [irw_cov_compute()]), with what an SEM
#' fit needs alongside it.
#'
#' @param df A table with columns `stat`, `var1`, `var2`, `value`, `n` and,
#'   optionally, `group`.
#' @param group For a table holding several samples (a `group` column), which
#'   one to return. Required when there is more than one.
#' @param complete A published matrix can hold cells the source did not
#'   report: rows with an empty `value`, which become `NA` here, with a
#'   warning. Most fitting functions (lavaan among them) refuse a matrix with
#'   `NA`. `complete = TRUE` returns the complete part instead: the matrix
#'   after dropping the fewest variables that leaves no blank cell (ties keep
#'   the variables listed first), with the dropped variables in `dropped`. The
#'   IRW's own record (`irw_metadata(source = "cov")`: `n_missing_cells`, and
#'   `is_pd` judged on the complete part) uses the same rule.
#'
#' @return A list: `matrix` (square, with dimnames), `type` (`"cor"` or
#'   `"cov"`), `n` (the largest cell n, the matrix N when n is the same
#'   everywhere), `n_pairwise` (a matrix of cell n), `sd` and `mean` (named
#'   vectors, empty when the table has none), `dropped` (the variables
#'   `complete = TRUE` left out; empty otherwise), and `computed` (TRUE when
#'   the matrix was computed from an IRW table rather than published, with
#'   `method`, `use` and `table` then set).
#'
#' @examples
#' df <- data.frame(id = rep(1:50, each = 3), item = rep(c("a", "b", "c"), 50),
#'                  resp = c(replicate(50, sample(1:5, 3, replace = TRUE))))
#' m <- irw_cov2matrix(irw_cov_compute(df))
#' m$matrix
#' # lavaan::cfa("f =~ a + b + c", sample.cov = m$matrix, sample.nobs = m$n)
#' @export
irw_cov2matrix <- function(df, group = NULL, complete = FALSE) {
  miss <- setdiff(.irw_cov_cols, names(df))
  if (length(miss)) stop("Not a covariance-family table; missing: ", paste(miss, collapse = ", "), call. = FALSE)
  attrs <- attributes(df)
  df <- as.data.frame(df)
  if ("group" %in% names(df)) {
    groups <- unique(df$group[!is.na(df$group) & df$group != ""])
    if (is.null(group)) {
      if (length(groups) > 1) {
        stop("This table holds ", length(groups), " samples (", paste(groups, collapse = ", "),
             "); choose one with `group =`.", call. = FALSE)
      }
      group <- groups[1]
    }
    if (!group %in% groups) stop("No group ", shQuote(group), " in this table.", call. = FALSE)
    df <- df[df$group %in% group, , drop = FALSE]
  }
  cells <- df[df$stat %in% c("cor", "cov"), , drop = FALSE]
  type <- unique(cells$stat)
  if (length(type) != 1) stop("A matrix is all cor or all cov rows.", call. = FALSE)
  v <- unique(c(cells$var1, cells$var2))
  m <- matrix(NA_real_, length(v), length(v), dimnames = list(v, v))
  np <- matrix(NA_integer_, length(v), length(v), dimnames = list(v, v))
  ij <- cbind(cells$var1, cells$var2); ji <- cbind(cells$var2, cells$var1)
  m[ij] <- cells$value; m[ji] <- cells$value
  np[ij] <- as.integer(cells$n); np[ji] <- as.integer(cells$n)
  pick <- function(s) { r <- df[df$stat == s, , drop = FALSE]; stats::setNames(as.numeric(r$value), r$var1) }
  sd <- pick("sd"); mu <- pick("mean")
  dropped <- character(0)
  n_blank <- sum(is.na(m[lower.tri(m)]))
  if (n_blank) {
    drop <- .irw_cov_complete_drop(v, m)
    if (isTRUE(complete)) {
      keep <- setdiff(v, drop)
      m <- m[keep, keep, drop = FALSE]; np <- np[keep, keep, drop = FALSE]
      sd <- sd[names(sd) %in% keep]; mu <- mu[names(mu) %in% keep]
      dropped <- drop
    } else {
      warning(structure(class = c("irw_cov_blank", "warning", "condition"), list(
        message = paste0(n_blank, if (n_blank == 1) " cell" else " cells",
                         " of this matrix the source did not report ", if (n_blank == 1) "is" else "are",
                         " NA. `complete = TRUE` returns the complete part, dropping ",
                         length(drop), if (length(drop) == 1) " variable" else " variables", " (",
                         paste(drop, collapse = ", "), ")."),
        call = NULL)))
    }
  }
  list(matrix = m, type = type, n = max(np, na.rm = TRUE), n_pairwise = np,
       sd = sd, mean = mu, dropped = dropped,
       computed = isTRUE(attrs$irw_computed), method = attrs$irw_method,
       use = attrs$irw_use, table = attrs$irw_table)
}

# irw_cov2list keeps unreported cells as NA, which is what metaSEM takes, so
# irw_cov2matrix's warning about them does not apply there.
.irw_cov_quiet <- function(expr) withCallingHandlers(expr,
  irw_cov_blank = function(w) invokeRestart("muffleWarning"))

#' Turn every sample of a covariance-family table into a list of matrices
#'
#' A table that holds several samples, such as a meta-analytic SEM corpus
#' with one study per `group`, becomes a named list of matrices on a common set
#' of variables plus a named vector of sample sizes: what
#' `metaSEM::tssem1(Cov =, n =)` wants. Each matrix is built as
#' [irw_cov2matrix()] builds it, then padded to the union of the variables
#' across samples, in order of first appearance. A variable a sample did not
#' measure is `NA` in that sample's matrix, diagonal included, which is
#' metaSEM's convention for a missing variable.
#' A cell a sample measured but did not report (a row with an empty `value`)
#' is `NA` too, off the diagonal, with no warning: metaSEM takes both.
#'
#' @param df A table with columns `stat`, `var1`, `var2`, `value`, `n` and,
#'   optionally, `group`. Without a `group` column, or with one sample, the
#'   result has one element, named after that sample (`"all"` when there is
#'   no `group` column).
#'
#' @return A list: `matrices` (a named list of square matrices, named by
#'   `group` exactly as the table gives it), `n` (a named integer vector, one
#'   N per sample), `type` (`"cor"` or `"cov"`, the same for every sample),
#'   `variables` (the common variable set), and `sd` and `mean` (named lists
#'   of named vectors, empty vectors where a sample reports none).
#'
#' @seealso [irw_cov2matrix()] for one sample.
#' @examples
#' pub <- data.frame(
#'   stat  = "cor",
#'   var1  = c("a", "b", "b", "a", "c", "c"),
#'   var2  = c("a", "a", "b", "a", "a", "c"),
#'   value = c(1, 0.4, 1, 1, 0.3, 1),
#'   n     = c(100, 100, 100, 80, 80, 80),
#'   group = c(rep("study 1", 3), rep("study 2", 3)))
#' x <- irw_cov2list(pub)
#' x$matrices
#' x$n
#' # metaSEM::tssem1(x$matrices, x$n)
#' @export
irw_cov2list <- function(df) {
  miss <- setdiff(.irw_cov_cols, names(df))
  if (length(miss)) stop("Not a covariance-family table; missing: ", paste(miss, collapse = ", "), call. = FALSE)
  if ("group" %in% names(df)) {
    g <- as.character(df$group)
    groups <- unique(g[!is.na(g) & g != ""])
    if (!length(groups)) stop("The group column is empty; no sample to return.", call. = FALSE)
    ms <- lapply(groups, function(x) .irw_cov_quiet(irw_cov2matrix(df, group = x)))
  } else {
    groups <- "all"
    ms <- list(.irw_cov_quiet(irw_cov2matrix(df)))
  }
  names(ms) <- groups
  types <- unique(vapply(ms, `[[`, "", "type"))
  if (length(types) != 1) {
    stop("The samples mix correlation and covariance matrices; take them one at a time with irw_cov2matrix().",
         call. = FALSE)
  }
  v <- unique(unlist(lapply(ms, function(m) rownames(m$matrix)), use.names = FALSE))
  pad <- function(m) {
    out <- matrix(NA_real_, length(v), length(v), dimnames = list(v, v))
    k <- rownames(m)
    out[k, k] <- m
    out
  }
  list(matrices = lapply(ms, function(m) pad(m$matrix)),
       n = vapply(ms, function(m) as.integer(m$n), 1L),
       type = types, variables = v,
       sd = lapply(ms, `[[`, "sd"), mean = lapply(ms, `[[`, "mean"))
}
