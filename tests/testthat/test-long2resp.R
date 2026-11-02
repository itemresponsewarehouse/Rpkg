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
  expect_equal(checks_custom$single_category_items, "1")
  expect_identical(names(checks_custom$sparse_category_items), "2")
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
  expect_equal(sort(out$q1), c(3, 4, 5))
})

test_that("the default wave keeps one occasion of a labelled column", {
  d <- labelled_waves()
  d <- d[d$wave != "post" | d$id != 3, ]   # "pre" is now the most frequent
  expect_message(
    out <- irw_long2resp(d, id_density_threshold = NULL),
    "Defaulting to the most frequent wave: pre"
  )
  expect_equal(sort(out$q1), c(1, 1, 2))
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
  expect_equal(sort(out$q1), c(3, 4, 5))
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

test_that("a repeated rater is neither pooled nor chosen for the caller", {
  # r1 and r2 disagree by 4 points, so a mean would be 3, which no rater gave.
  # The first version of this fix kept the most frequent rater instead; on
  # swmd_mokken's 651-rater panel that returned a single respondent, so the
  # answer is to stop and name both ways forward.
  expect_error(
    suppressMessages(irw_long2resp(rater_frame(), id_density_threshold = NULL)),
    "keys repeated id\\+item responses \\(2 values\\)"
  )
  expect_error(
    suppressMessages(irw_long2resp(rater_frame(), id_density_threshold = NULL)),
    'occasion = c\\(rater = "r1"\\)'
  )
})

test_that("an explicit agg_method pools the rater panel and names the column", {
  msgs <- character(0)
  out <- withCallingHandlers(
    irw_long2resp(rater_frame(), agg_method = "mode", id_density_threshold = NULL),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  # Both raters' rows are still present to be aggregated, so both ids survive.
  expect_equal(nrow(out), 2L)
  expect_equal(attr(out, "occasion_pooled"), "rater")
  expect_equal(length(attr(out, "occasion")), 0L)
  expect_match(paste(msgs, collapse = "\n"), "Aggregating across rater \\(2 values\\)")
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
  expect_error(
    suppressMessages(irw_long2resp(df, id_density_threshold = NULL)),
    "Column 'trial_number' keys repeated id\\+item responses"
  )
  out2 <- suppressMessages(irw_long2resp(df, occasion = c(trial_number = 2),
                                         id_density_threshold = NULL))
  expect_equal(unname(unlist(out2[, c("item_q1", "item_q2")])), c(1, 1))
  expect_equal(attr(out2, "occasion"), c(trial_number = "2"))
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
  expect_equal(sort(setdiff(names(out), "id")), c("a1", "a2", "b1", "b2"))
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
  expect_equal(sort(setdiff(names(out), "id")), c("a1", "a2"))
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

# Column order (review R-23, ben-domingue/irw#3221). stats::reshape() ordered
# columns by first appearance in the input rows, which is not a stable property
# of a Redivis table; Python sorted, but lexicographically.

test_that("item columns come back in natural order", {
  d <- data.frame(id = rep("p1", 4),
                  item = c("item10", "item2", "item1", "item20"),
                  resp = 1:4, stringsAsFactors = FALSE)
  out <- suppressMessages(irw_long2resp(d, id_density_threshold = NULL))
  expect_equal(names(out), c("id", "item1", "item2", "item10", "item20"))
})

test_that("the column order does not depend on the row order", {
  d <- data.frame(id = rep("p1", 3),
                  item = c("item10", "item2", "item1"),
                  resp = 1:3, stringsAsFactors = FALSE)
  orders <- lapply(list(c(1, 2, 3), c(3, 1, 2), c(2, 3, 1)), function(ix) {
    names(suppressMessages(irw_long2resp(d[ix, ], id_density_threshold = NULL)))
  })
  expect_equal(orders[[1]], orders[[2]])
  expect_equal(orders[[2]], orders[[3]])
})

test_that("the natural key is the one Python-pkg builds", {
  # Pinned as a string, not as an order: Python's `_natural_key()` must produce
  # the same characters, or the two clients' column orders drift apart.
  expect_equal(.irw_natural_key("item2"), paste0("item", strrep("0", 19), "2"))
  expect_equal(.irw_natural_key("Item2"), .irw_natural_key("item2"))
  expect_equal(
    c("item10", "Item3", "item1", "q2", "q10", "a")[
      .irw_natural_order(c("item10", "Item3", "item1", "q2", "q10", "a"))],
    c("a", "item1", "Item3", "item10", "q2", "q10")
  )
  # A digit run too long to be an item number is left alone rather than padded.
  long_run <- paste0("x", strrep("9", 25))
  expect_equal(.irw_natural_key(long_run), long_run)
  # Case folds for the comparison and the raw name breaks the tie, so the order
  # is total rather than arbitrary between `Item2` and `item2`.
  expect_equal(c("item2", "Item2")[.irw_natural_order(c("item2", "Item2"))],
               c("Item2", "item2"))
})

# Column names (review R-23, ben-domingue/irw#3221). The item id is the key
# that joins a wide matrix back to the item text and the item metadata, so the
# column name is that id and not a decoration of it. Python-pkg returns the
# same names; these cases are the ones that distinguish a map from string
# surgery, and each is also asserted there.

test_that("the item id is the column name, not item_<id>", {
  d <- data.frame(id = rep(c("p1", "p2"), each = 2),
                  item = rep(c("q1", "q2"), 2),
                  resp = c(1, 0, 0, 1), stringsAsFactors = FALSE)
  out <- suppressMessages(irw_long2resp(d, id_density_threshold = NULL))
  expect_equal(names(out), c("id", "q1", "q2"))
})

test_that("a numeric item id survives as the column name it is", {
  d <- data.frame(id = rep(c("p1", "p2"), each = 2),
                  item = rep(c(1L, 2L), 2),
                  resp = c(1, 0, 0, 1))
  out <- suppressMessages(irw_long2resp(d, id_density_threshold = NULL))
  expect_equal(names(out), c("id", "1", "2"))
})

test_that("an item already called item10 is not prefixed twice", {
  d <- data.frame(id = rep(c("p1", "p2"), each = 2),
                  item = rep(c("item10", "item2"), 2),
                  resp = c(1, 0, 0, 1), stringsAsFactors = FALSE)
  out <- suppressMessages(irw_long2resp(d, id_density_threshold = NULL))
  expect_equal(names(out), c("id", "item2", "item10"))
})

test_that("item_a and a stay two items", {
  # The prefix is applied unconditionally for exactly this: skipping items
  # already named `item_*` folded these two into one column and reported it as
  # duplicate (id, item) rows in the caller's data.
  d <- data.frame(id = rep(c("p1", "p2"), each = 2),
                  item = rep(c("a", "item_a"), 2),
                  resp = c(1, 0, 0, 1), stringsAsFactors = FALSE)
  out <- suppressMessages(irw_long2resp(d, id_density_threshold = NULL))
  expect_equal(names(out), c("id", "a", "item_a"))
  expect_equal(out$a, c(1, 0))
  expect_equal(out$item_a, c(0, 1))
})

test_that("an internal underscore is not eaten on the way back", {
  # `sub("item_", "", "item_myitem_x")` is right only by anchoring and
  # `gsub` turned it into `myx`; the map cannot get this wrong.
  d <- data.frame(id = rep(c("p1", "p2"), each = 2),
                  item = rep(c("myitem_x", "q2"), 2),
                  resp = c(1, 0, 0, 1), stringsAsFactors = FALSE)
  out <- suppressMessages(irw_long2resp(d, id_density_threshold = NULL))
  expect_equal(sort(setdiff(names(out), "id")), c("myitem_x", "q2"))
})

test_that("resp2long round-trips the item ids exactly", {
  d <- data.frame(id = rep(c("p1", "p2"), each = 2),
                  item = rep(c(1L, 2L), 2),
                  resp = c(1, 0, 0, 1))
  out <- suppressMessages(irw_long2resp(d, id_density_threshold = NULL))
  back <- irw_resp2long(out)
  expect_setequal(back$item, c("1", "2"))
  expect_equal(nrow(back), 4L)
})

test_that("check_resp names the items the caller will see as columns", {
  d <- data.frame(id = rep(c("p1", "p2", "p3"), each = 2),
                  item = rep(c(1L, 2L), 3),
                  resp = c(1, 1, 1, 0, 1, 1))
  out <- suppressMessages(irw_long2resp(d, check_resp = TRUE,
                                        id_density_threshold = NULL))
  checks <- attr(out, "resp_checks")
  expect_equal(checks$single_category_items, "1")
  expect_true("1" %in% names(out))
})
