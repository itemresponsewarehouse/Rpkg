library(testthat)

test_that("identical predictions give an IMV of zero", {
  set.seed(1)
  d <- data.frame(resp = rbinom(200, 1, 0.6), p1 = runif(200, 0.2, 0.8))
  d$p2 <- d$p1

  expect_equal(irw_imv(d, "p1", "p2"), 0, tolerance = 1e-6)
})

test_that("a better-calibrated model gets a positive IMV, and the reverse is negative", {
  set.seed(2)
  y <- rbinom(2000, 1, 0.75)
  guess <- rep(0.5, length(y))
  base_rate <- rep(0.75, length(y))

  expect_gt(irw_imv(y, p1 = guess, p2 = base_rate), 0)
  expect_lt(irw_imv(y, p1 = base_rate, p2 = guess), 0)
})

test_that("data frame and vector interfaces agree", {
  set.seed(3)
  d <- data.frame(
    answer = rbinom(300, 1, 0.5),
    a = runif(300, 0.1, 0.9),
    b = runif(300, 0.1, 0.9)
  )

  expect_equal(
    irw_imv(d, "a", "b", resp = "answer"),
    irw_imv(d$answer, p1 = d$a, p2 = d$b)
  )
})

test_that("predictions of 0 and 1 are clamped rather than producing -Inf", {
  y <- c(1, 0, 1, 0)
  out <- irw_imv(y, p1 = c(1, 0, 1, 0), p2 = rep(0.5, 4))

  expect_true(is.finite(out))
  expect_lt(out, 0)
})

test_that("irw_imv rejects malformed input", {
  d <- data.frame(resp = c(0, 1), p1 = c(0.4, 0.6), p2 = c(0.5, 0.5))

  expect_error(irw_imv(d, "p1", "nope"), "Column not found in `data`: nope")
  expect_error(irw_imv(c(0, 2), p1 = c(0.4, 0.6), p2 = c(0.5, 0.5)), "binary outcomes")
  expect_error(irw_imv(c(0, 1), p1 = c(0.4, 1.6), p2 = c(0.5, 0.5)), "probabilities in \\[0, 1\\]")
  expect_error(irw_imv(c(0, 1), p1 = c(0.4, 0.6), p2 = 0.5), "same length")
  expect_error(irw_imv(c(0, NA), p1 = c(0.4, 0.6), p2 = c(0.5, 0.5)), "must not contain NA")
})
