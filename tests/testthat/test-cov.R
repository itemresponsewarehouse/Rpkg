# The covariance family (source = "cov"). Published matrices are not reachable
# yet (irw_cov is unreleased); what ships now is computing a core table's item
# matrix on fetch, and turning the layout back into a matrix. Standard:
# ben-domingue/irw data/covariance/README.md.

long_frame <- function(n = 60, seed = 1) {
  set.seed(seed)
  f <- stats::rnorm(n)
  resp <- sapply(1:4, function(j) as.numeric(cut(f + stats::rnorm(n), c(-Inf, -0.5, 0.5, Inf))))
  data.frame(id = rep(seq_len(n), 4), item = rep(paste0("q", 1:4), each = n), resp = c(resp))
}

wide_of <- function(df) {
  w <- stats::reshape(df, idvar = "id", timevar = "item", direction = "wide")
  m <- as.matrix(w[, -1]); colnames(m) <- sub("^resp\\.", "", colnames(m)); m
}

test_that("pearson matches stats::cor and the layout is the lower triangle", {
  df <- long_frame()
  out <- irw_cov_compute(df)
  expect_named(out, c("stat", "var1", "var2", "value", "n"))
  expect_equal(nrow(out), 4 * 5 / 2)
  expect_true(all(out$stat == "cor"))
  m <- irw_cov2matrix(out)
  expect_equal(m$matrix, stats::cor(wide_of(df))[rownames(m$matrix), colnames(m$matrix)])
  expect_identical(m$n, 60L)
  expect_true(m$computed)
  expect_identical(m$method, "pearson")
})

test_that("method, type and use are passed through", {
  df <- long_frame()
  w <- wide_of(df)
  sp <- irw_cov2matrix(irw_cov_compute(df, method = "spearman", type = "cov"))
  expect_identical(sp$type, "cov")
  expect_equal(unname(sp$matrix), unname(stats::cov(w, method = "spearman")[colnames(sp$matrix), colnames(sp$matrix)]))
  df$resp[c(1, 70)] <- NA
  pw <- irw_cov2matrix(irw_cov_compute(df))
  cc <- irw_cov2matrix(irw_cov_compute(df, use = "complete"))
  expect_true(length(unique(c(pw$n_pairwise))) > 1)
  expect_identical(unique(c(cc$n_pairwise)), 58L)
  expect_error(irw_cov_compute(df, method = "polychoric", type = "cov"), "not defined")
})

test_that("a user-defined method gets the wide matrix and is checked", {
  df <- long_frame()
  seen <- NULL
  shrunk <- function(x) { seen <<- dim(x); 0.5 * stats::cor(x) + 0.5 * diag(ncol(x)) }
  m <- irw_cov2matrix(irw_cov_compute(df, method = shrunk))
  expect_identical(seen, c(60L, 4L))
  expect_identical(m$method, "user-defined")
  expect_equal(unname(diag(m$matrix)), rep(1, 4))
  expect_error(irw_cov_compute(df, method = function(x) diag(3)), "4 x 4")
  expect_error(irw_cov_compute(df, method = function(x) { m <- diag(4); m[1, 2] <- 0.3; m }), "not symmetric")
})

test_that("polychoric and tetrachoric go through psych", {
  skip_if_not_installed("psych")
  df <- long_frame()
  pc <- irw_cov2matrix(irw_cov_compute(df, method = "polychoric"))
  expect_identical(pc$method, "polychoric")
  expect_true(all(abs(pc$matrix) <= 1))
  expect_error(irw_cov_compute(df, method = "tetrachoric"), "dichotomous")
  df$resp <- as.numeric(df$resp > 1)
  expect_identical(irw_cov2matrix(irw_cov_compute(df, method = "tetrachoric"))$method, "tetrachoric")
})

test_that("repeated measures, too many items, flat items and non-core tables are handled", {
  df <- long_frame()
  expect_error(irw_cov_compute(rbind(df, df[1, ])), "more than once")
  expect_error(irw_cov_compute(df, max_items = 3), "max_items")
  flat <- rbind(df, data.frame(id = 1:60, item = "q5", resp = 2))
  expect_warning(out <- irw_cov_compute(flat), "no variation")
  expect_false("q5" %in% out$var1)
  expect_error(irw_cov_compute(data.frame(id = 1, task = 1, profile = 1)), "core table")
})

test_that("irw_cov2matrix reads published tables, groups and moments", {
  pub <- data.frame(
    stat = c("cor", "cor", "cor", "sd", "sd", "cor", "cor", "cor"),
    var1 = c("a", "b", "b", "a", "b", "a", "b", "b"),
    var2 = c("a", "a", "b", NA, NA, "a", "a", "b"),
    value = c(1, 0.4, 1, 2, 3, 1, 0.6, 1),
    n = c(rep(100, 5), rep(80, 3)),
    group = c(rep("boys", 5), rep("girls", 3)))
  expect_error(irw_cov2matrix(pub), "choose one")
  b <- irw_cov2matrix(pub, group = "boys")
  expect_equal(b$matrix["a", "b"], 0.4)
  expect_equal(b$sd, c(a = 2, b = 3))
  expect_false(b$computed)
  expect_identical(irw_cov2matrix(pub, group = "girls")$n, 80L)
  expect_error(irw_cov2matrix(data.frame(x = 1)), "missing")
})

test_that("irw_fetch(source = 'cov') fetches id/item/resp from core and computes", {
  df <- long_frame()
  asked <- NULL
  local_mocked_bindings(
    fetch_single_data = function(table_id, source, dedup, max_rows, columns, ...) {
      asked <<- list(table = table_id, source = source, columns = columns); df },
    .irw_source_note = function(...) invisible(NULL))
  out <- irw_fetch("toy_table", source = "cov", cov_args = list(method = "spearman"))
  expect_identical(asked$source, "core")
  expect_identical(asked$columns, c("id", "item", "resp"))
  expect_identical(attr(out, "irw_method"), "spearman")
  expect_identical(attr(out, "irw_table"), "toy_table")
  expect_error(irw_fetch("toy_table", source = "cov", max_rows = 10), "biased")
  expect_error(irw_fetch("toy_table", source = "cov", resp = TRUE), "two different things")
})
