library(testthat)

test_that("irw_check_resp supports custom response columns", {
  x_custom <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    item = c("1", "2", "1", "2", "1", "2"),
    answer = c("A", "A", "A", "B", "A", "C"),
    stringsAsFactors = FALSE
  )
  x_standard <- x_custom
  x_standard$resp <- x_standard$answer

  checks_custom <- irw_check_resp(x_custom, min_count = 2L, resp_col = "answer")
  checks_standard <- irw_check_resp(x_standard, min_count = 2L)

  expect_identical(checks_custom, checks_standard)
  expect_equal(checks_custom$single_category_items, "item_1")
  expect_identical(names(checks_custom$sparse_category_items), "item_2")
})

test_that("irw_check_resp errors when resp_col is missing", {
  x <- data.frame(
    id = 1:2,
    item = c("1", "2"),
    stringsAsFactors = FALSE
  )

  expect_error(
    irw_check_resp(x, resp_col = "answer"),
    "Column specified by `resp_col` not found in `x`"
  )
})

# ---- Occasion selection ----
#
# These four cover the defect that motivated the rewrite: a labelled wave
# column used to be coerced with as.numeric(), the filter was then skipped, and
# with the default agg_method = "mean" every occasion was averaged together and
# returned as data. The first test is the one that matters -- it must never pass
# by returning a value.

labelled_waves <- function() {
  data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    item = c("q1", "q1", "q1", "q1", "q1", "q1"),
    wave = c("pre", "post", "pre", "post", "pre", "post"),
    resp = c(1, 5, 2, 4, 1, 3),
    stringsAsFactors = FALSE
  )
}

test_that("a labelled wave column is filtered, never pooled", {
  out <- irw_long2resp(labelled_waves(), wave = "post", id_density_threshold = NULL)
  expect_equal(nrow(out), 3L)
  # The post responses themselves, not the mean of pre and post.
  expect_equal(sort(out$item_q1), c(3, 4, 5))
})

test_that("the default wave keeps one occasion of a labelled column", {
  d <- labelled_waves()
  d <- d[d$wave != "post" | d$id != 3, ]   # "pre" is now the most frequent
  expect_message(
    out <- irw_long2resp(d, id_density_threshold = NULL),
    "Defaulting to the most frequent wave: pre"
  )
  expect_equal(sort(out$item_q1), c(1, 1, 2))
})

test_that("a wave that is not in the table is an error", {
  expect_error(
    irw_long2resp(labelled_waves(), wave = "followup", id_density_threshold = NULL),
    "not present in this table"
  )
})

test_that("a numeric wave argument matches an integer wave column", {
  d <- labelled_waves()
  d$wave <- rep(c(1L, 2L), 3)
  out <- irw_long2resp(d, wave = 2, id_density_threshold = NULL)
  expect_equal(sort(out$item_q1), c(3, 4, 5))
})

test_that("an all-missing wave column stops rather than pooling", {
  d <- labelled_waves()
  d$wave <- NA
  expect_error(irw_long2resp(d, id_density_threshold = NULL), "missing on every row")
})
