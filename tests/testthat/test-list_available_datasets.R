test_that("list_available_datasets works correctly", {
  # Ensure the function returns a data frame
  result <- list_available_datasets()
  expect_s3_class(result, "data.frame")

  # Check that essential columns are present
  expect_true(all(c("name", "numRows", "variableCount") %in% names(result)))

  # Check that there is at least one dataset available
  expect_true(nrow(result) > 0)
})
