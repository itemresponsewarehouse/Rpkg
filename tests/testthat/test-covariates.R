library(testthat)

make_cov_df <- function() {
  data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    item = rep(c("i1", "i2"), 3),
    resp = c(1, 0, 1, 1, 0, 0),
    cov_group = c("A", "A", "B", "B", "A", "A"),
    cov_age = c(10, 10, 11, 11, 12, 12),
    stringsAsFactors = FALSE
  )
}

test_that("person-level covariates are detected automatically", {
  df <- make_cov_df()
  out <- suppressMessages(irw_covariates(df))
  expect_identical(names(out), c("id", "cov_group", "cov_age"))
  expect_identical(nrow(out), 3L)
  expect_identical(out$cov_group, c("A", "B", "A"))
})

test_that("response-level columns are never returned", {
  df <- make_cov_df()
  out <- suppressMessages(irw_covariates(df))
  expect_false(any(c("item", "resp") %in% names(out)))
})

test_that("columns that vary within id are excluded with a message", {
  df <- make_cov_df()
  df$attempt <- seq_len(nrow(df))
  expect_message(out <- irw_covariates(df), "varies within id")
  expect_false("attempt" %in% names(out))
})

test_that("explicitly naming a varying column is an error", {
  df <- make_cov_df()
  df$attempt <- seq_len(nrow(df))
  expect_error(irw_covariates(df, cols = "attempt"),
               "vary within id")
})

test_that("cols selects a subset", {
  df <- make_cov_df()
  out <- irw_covariates(df, cols = "cov_group")
  expect_identical(names(out), c("id", "cov_group"))
})

test_that("missing columns raise the standard error", {
  df <- make_cov_df()
  expect_error(irw_covariates(df, cols = "cov_nope"),
               "Missing required IRW columns: cov_nope")
  expect_error(irw_covariates(data.frame(item = 1)),
               "Missing required IRW columns: id")
})

test_that("align reorders rows to match a wide response matrix", {
  df <- make_cov_df()
  wide <- suppressMessages(irw_long2resp(df, id_density_threshold = NULL))
  # Force an id order that differs from first-appearance order.
  wide <- wide[c(3, 1, 2), , drop = FALSE]
  out <- suppressMessages(irw_covariates(df, align = wide))
  expect_identical(out$id, wide$id)
  key <- c("1" = "A", "2" = "B", "3" = "A")
  expect_identical(out$cov_group, unname(key[as.character(wide$id)]))
})

test_that("align accepts a matrix with rownames and a bare id vector", {
  df <- make_cov_df()
  m <- matrix(0, nrow = 2, ncol = 1, dimnames = list(c("3", "1"), "i1"))
  out_m <- suppressMessages(irw_covariates(df, align = m))
  expect_identical(out_m$cov_group, c("A", "A"))

  out_v <- suppressMessages(irw_covariates(df, align = c(2, 1)))
  expect_identical(out_v$cov_group, c("B", "A"))
})

test_that("ids in align but not in df give NA rows with a message", {
  df <- make_cov_df()
  expect_message(out <- irw_covariates(df, align = c(1, 99)),
                 "not found in `df`")
  expect_identical(out$id, c(1, 99))
  expect_true(is.na(out$cov_group[2]))
})

test_that("data with no person-level columns returns ids only", {
  df <- data.frame(id = c(1, 1, 2), item = c("i1", "i2", "i1"),
                   resp = c(1, 0, 1), stringsAsFactors = FALSE)
  expect_message(out <- irw_covariates(df), "No person-level covariates")
  expect_identical(names(out), "id")
  expect_identical(nrow(out), 2L)
})

test_that("align without an id column is an error", {
  df <- make_cov_df()
  expect_error(irw_covariates(df, align = data.frame(x = 1)),
               "without an `id` column")
})
