library(testthat)

skip_if_no_mirt <- function() {
  skip_if_not_installed("mirt")
}

fit_example <- function(n_id = 150, n_item = 8, seed = 1) {
  set.seed(seed)
  df <- irw_simdata(n_id = n_id, n_item = n_item, model = "2PL", seed = seed)
  wide <- suppressMessages(irw_long2resp(df))
  fit <- mirt::mirt(
    wide[, setdiff(names(wide), "id"), drop = FALSE],
    1,
    itemtype = "Rasch",
    verbose = FALSE
  )
  list(df = df, wide = wide, fit = fit)
}

test_that("irw_predict returns a probability for every response", {
  skip_if_no_mirt()
  ex <- fit_example()

  out <- irw_predict(ex$fit, ex$wide)

  expect_true("p" %in% names(out))
  expect_false(anyNA(out$p))
  expect_true(all(out$p > 0 & out$p < 1))
  expect_equal(nrow(out), sum(!is.na(irw_resp2long(ex$wide)$resp)))
})

test_that("predictions follow ability and item difficulty", {
  skip_if_no_mirt()
  ex <- fit_example()
  out <- irw_predict(ex$fit, ex$wide)

  # Within one item, higher observed score means higher predicted probability.
  one_item <- out[out$item == out$item[1], ]
  total <- rowSums(ex$wide[, setdiff(names(ex$wide), "id"), drop = FALSE], na.rm = TRUE)
  score <- stats::setNames(total, as.character(ex$wide$id))
  expect_gt(
    stats::cor(one_item$p, score[as.character(one_item$id)]),
    0.9
  )
})

test_that("irw_predict scores held-out responses for persons in the fit", {
  skip_if_no_mirt()
  ex <- fit_example()
  held_out <- irw_resp2long(ex$wide)[1:20, c("id", "item")]

  out <- irw_predict(ex$fit, ex$wide, newdata = held_out)

  expect_equal(nrow(out), 20L)
  expect_false(anyNA(out$p))
})

test_that("irw_predict accepts unprefixed item names", {
  skip_if_no_mirt()
  ex <- fit_example()
  nd <- irw_resp2long(ex$wide)[1:10, c("id", "item")]
  nd$item <- sub("^item_", "", nd$item)

  expect_equal(
    irw_predict(ex$fit, ex$wide, newdata = nd)$p,
    irw_predict(ex$fit, ex$wide)$p[1:10]
  )
})

test_that("irw_predict rejects mismatched ids, items and frames", {
  skip_if_no_mirt()
  ex <- fit_example()

  expect_error(
    irw_predict(ex$fit, ex$wide[, setdiff(names(ex$wide), "id"), drop = FALSE]),
    "No `id` column found"
  )

  bad_id <- data.frame(id = "not_a_person", item = names(ex$wide)[2])
  expect_error(irw_predict(ex$fit, ex$wide, newdata = bad_id), "absent from `resp`")

  bad_item <- data.frame(id = ex$wide$id[1], item = "item_999")
  expect_error(irw_predict(ex$fit, ex$wide, newdata = bad_item), "not estimated by `model`")

  short <- ex$wide[1:10, ]
  expect_error(irw_predict(ex$fit, short), "persons but `resp` has")
})

test_that("irw_imv on model predictions ranks a 2PL above a Rasch fit", {
  skip_if_no_mirt()
  set.seed(4)
  df <- irw_simdata(n_id = 400, n_item = 12, model = "2PL", seed = 4)
  wide <- suppressMessages(irw_long2resp(df))
  items <- wide[, setdiff(names(wide), "id"), drop = FALSE]

  m_rasch <- mirt::mirt(items, 1, itemtype = "Rasch", verbose = FALSE)
  m_2pl <- mirt::mirt(items, 1, itemtype = "2PL", verbose = FALSE)

  z <- irw_predict(m_rasch, wide)
  names(z)[names(z) == "p"] <- "p1"
  z$p2 <- irw_predict(m_2pl, wide, newdata = z[, c("id", "item")])$p

  expect_gt(irw_imv(z, "p1", "p2"), 0)
})
