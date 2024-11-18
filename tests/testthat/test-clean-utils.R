test_that("irw_rename works with data frames", {
  # Create test data frame
  test_df <- data.frame(
    TEST.VAR = 1:3,
    another.VAR = letters[1:3],
    stringsAsFactors = FALSE
  )
  
  # Test renaming
  result <- irw_rename(test_df)
  
  # Check column names were transformed
  expect_true(all(c("test_var", "another_var") %in% colnames(result)))
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 3)
})

test_that("irw_rename works with character vectors", {
  # Test vector input
  test_vec <- c("TEST.VAR", "another.VAR")
  result <- irw_rename(test_vec)
  
  # Check names were transformed
  expect_equal(result, c("test_var", "another_var"))
  expect_type(result, "character")
  expect_length(result, 2)
})

test_that("irw_rename throws error for invalid input", {
  # Test error handling
  expect_error(irw_rename(1:5), "x must be a data frame or character vector")
  expect_error(irw_rename(TRUE), "x must be a data frame or character vector")
  expect_error(irw_rename(NULL), "x must be a data frame or character vector")
})


test_that("downloaded data reformat for base mirt", {
  # Ensure the function returns a data frame
  df=fetch_data(filter_tables(required_columns = "item")[3])
  result <- reformat(df)
  expect_s3_class(result, "data.frame")
  
  # Check that essential columns are present
  expect_true(all(c("name", "numRows", "variableCount") %in% names(result)))
  
  # Check that there is at least one dataset available
  expect_true(nrow(result) > 0)
})
