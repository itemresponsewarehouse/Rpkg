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
