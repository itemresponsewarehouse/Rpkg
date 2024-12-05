test_that("irw_rename works with data frames", {
  # Create test data frame
  test_df <- data.frame(
    id = rep(1:3,3),
    item = rep(letters[1:3],each=3),
    resp = sample(0:1,9,replace=TRUE)
  )
  
  # Test renaming
  result <- irw_rename(test_df)
  
  # Check column names were transformed
  expect_true(all(c("id", "item","resp") %in% colnames(result)))
  expect_equal(ncol(result), 3)
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

# test_that("irw_rename throws error for invalid input", {
#   # Test error handling
#   expect_error(irw_rename(1:5), "x must be a data frame or character vector")
#   expect_error(irw_rename(TRUE), "x must be a data frame or character vector")
#   expect_error(irw_rename(NULL), "x must be a data frame or character vector")
# })


test_that("downloaded data reformat for base mirt", {
  # Ensure the function returns a data frame
  test_df <- data.frame(
    id = rep(1:3,3),
    item = rep(letters[1:3],each=3),
    resp = sample(0:1,9,replace=TRUE)
  )
  result <- reformat(test_df)
  expect_s3_class(result, "data.frame")
  
  # Check that unique items are now present as column names
  expect_true(all(c("a", "b", "c") %in% colnames(result)))
  
  # Check that there is at least one dataset available
  expect_true(nrow(result) > 0)
})
