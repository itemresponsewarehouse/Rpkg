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


# Occasion columns other than `wave` (review R-25, ben-domingue/irw#3223).
# The standard defines nine ways a response can repeat; this reshape handled
# one. `rater`, `trial_*` and `session` were dropped from the frame and then
# averaged together by the default agg_method = "mean", with no argument a
# caller could pass -- the same defect as the wave pooling, one layer down.

rater_frame <- function() {
  data.frame(
    id = rep(c("p1", "p2"), each = 4),
    item = rep(c("item_q1", "item_q2"), times = 4),
    rater = rep(c("r1", "r1", "r2", "r2"), times = 2),
    resp = c(1, 1, 5, 5, 2, 2, 4, 4),
    stringsAsFactors = FALSE
  )
}

test_that("a repeated rater returns one rater's responses, never their mean", {
  out <- suppressMessages(irw_long2resp(rater_frame(), id_density_threshold = NULL))
  # r1 and r2 disagree by 4 points; a mean would be 3, which no rater gave.
  expect_false(any(unlist(out[, c("item_q1", "item_q2")]) == 3))
  expect_equal(attr(out, "occasion"), c(rater = "r1"))
})

test_that("the occasion argument chooses, for any occasion column", {
  out <- suppressMessages(irw_long2resp(rater_frame(), occasion = c(rater = "r2"),
                                        id_density_threshold = NULL))
  expect_equal(unname(unlist(out[out$id == "p1", c("item_q1", "item_q2")])), c(5, 5))
  expect_equal(attr(out, "occasion"), c(rater = "r2"))
})

test_that("an occasion that is not in the table is an error", {
  expect_error(
    suppressMessages(irw_long2resp(rater_frame(), occasion = c(rater = "r9"))),
    "is not present in this table"
  )
})

test_that("an occasion column that is empty on every row is an error", {
  df <- rater_frame()
  df$rater <- NA
  expect_error(irw_long2resp(df), "missing on every row")
})

test_that("trial columns count as occasions", {
  df <- data.frame(
    id = rep("p1", 4), item = rep(c("item_q1", "item_q2"), 2),
    trial_number = rep(c(1, 2), each = 2),
    resp = c(0, 0, 1, 1), stringsAsFactors = FALSE
  )
  out <- suppressMessages(irw_long2resp(df, id_density_threshold = NULL))
  expect_equal(attr(out, "occasion"), c(trial_number = "1"))
  expect_equal(unname(unlist(out[, c("item_q1", "item_q2")])), c(0, 0))
  out2 <- suppressMessages(irw_long2resp(df, occasion = c(trial_number = 2),
                                         id_density_threshold = NULL))
  expect_equal(unname(unlist(out2[, c("item_q1", "item_q2")])), c(1, 1))
})

test_that("a column that does not key a repeat is left alone", {
  # `subtest` is in the standard's occasion list because it CAN key a repeat.
  # Here it does not: each id+item appears once, and the subtests hold
  # different items. Filtering to one subtest would throw away items rather
  # than choose an occasion.
  df <- data.frame(
    id = rep(c("p1", "p2"), each = 4),
    item = rep(c("a1", "a2", "b1", "b2"), 2),
    subtest = rep(c("a", "a", "b", "b"), 2),
    resp = c(1, 0, 1, 0, 0, 1, 0, 1), stringsAsFactors = FALSE
  )
  out <- suppressMessages(irw_long2resp(df, id_density_threshold = NULL))
  expect_equal(sort(setdiff(names(out), "id")), c("item_a1", "item_a2", "item_b1", "item_b2"))
  expect_equal(attr(out, "occasion"), character(0))
})

test_that("an explicit occasion is honoured even when nothing repeats", {
  df <- data.frame(
    id = rep(c("p1", "p2"), each = 4),
    item = rep(c("a1", "a2", "b1", "b2"), 2),
    subtest = rep(c("a", "a", "b", "b"), 2),
    resp = c(1, 0, 1, 0, 0, 1, 0, 1), stringsAsFactors = FALSE
  )
  out <- suppressMessages(irw_long2resp(df, occasion = c(subtest = "a"),
                                        id_density_threshold = NULL))
  expect_equal(sort(setdiff(names(out), "id")), c("item_a1", "item_a2"))
})

test_that("occasion and wave cannot contradict each other", {
  df <- data.frame(id = c("p1", "p1"), item = c("q1", "q1"),
                   wave = c(1, 2), resp = c(0, 1), stringsAsFactors = FALSE)
  expect_error(irw_long2resp(df, wave = 1, occasion = list(wave = 2)),
               "disagree")
})

test_that("occasion names must be columns the table has an occasion for", {
  expect_error(irw_long2resp(rater_frame(), occasion = c(session = "1")),
               "no occasion for")
  expect_error(irw_long2resp(rater_frame(), occasion = "r1"),
               "named vector or list")
  expect_error(irw_long2resp(rater_frame(), occasion = list(rater = c("r1", "r2"))),
               "single value")
})
