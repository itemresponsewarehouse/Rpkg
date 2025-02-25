# tests/testthat/test-irw_list_tables.R

# Load the helper function
source(test_path("helper-redivis.R"))

# Skip tests if redivis is not installed
skip_if_no_redivis()

test_that("irw_list_tables() returns a data frame with expected columns", {
  result <- irw_list_tables()
  expect_s3_class(result, "data.frame")
  expect_true(all(c("name", "numRows", "variableCount") %in% names(result)))
})

test_that("irw_list_tables() returns non-empty data frame", {
  result <- irw_list_tables()
  expect_true(nrow(result) > 0)
})