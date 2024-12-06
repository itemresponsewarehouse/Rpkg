test_that("irw_rename works with data frames", {
  # Create test data frame
  test_df <- data.frame(
    id = rep(1:3, 3),
    item = rep(letters[1:3], each = 3),
    resp = sample(0:1, 9, replace = TRUE)
  )
  
  # Test renaming
  result <- irw_rename(test_df)
  
  # Check column names were transformed
  expect_true(all(c("id", "item", "resp") %in% colnames(result)))
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
    id = rep(1:3, 3),
    item = rep(letters[1:3], each = 3),
    resp = sample(0:1, 9, replace = TRUE)
  )
  result <- reformat(test_df)
  expect_s3_class(result, "data.frame")
  
  # Check that unique items are now present as column names
  expect_true(all(c("a", "b", "c") %in% colnames(result)))
  
  # Check that there is at least one dataset available
  expect_true(nrow(result) > 0)
})

## check format outputs for different packages



test_that("data reformat for mirt with unsupported inputs", {
  df = data.frame(
    id = rep(rep(1:3, 3), 2),
    item = rep(rep(letters[1:3], each = 3), 2),
    resp = sample(0:1, 9 * 2, replace = TRUE),
    cov_1 = rep(rep(rnorm(3), 3), 2),
    group = rep(c('G1', 'G2', 'G2'), each = 3),
    wave = rep(c(1, 2), each = 9)
  )
  # Ensure the function returns a data frame
  result <- reformat(df, keep_all = TRUE)
  expect_s3_class(result, "data.frame")
  
  # Check that unique items are now present as column names
  expect_true(all(c("a", "b", "c") %in% colnames(result)))
  
  # Check the correct output available
  expect_true(nrow(result) == 6)
})

test_that("reformat for lavaan package", {
  df = data.frame(
    id = rep(rep(1:3, 3), 2),
    item = rep(rep(letters[1:3], each = 3), 2),
    resp = rep(c(0, 1, 0, 1, 0, 1), 3),
    cov_1 = rep(rep(rnorm(3), 3), 2),
    group = rep(c('G1', 'G2', 'G2'), each = 3),
    wave = rep(c(1, 2), each = 9)
  )
  # Ensure the function returns a data frame
  result <- reformat(df, package = "lavaan", timedate = TRUE)
  expect_s3_class(result, "data.frame")
  
  # Check that unique items are now present as column names, dropping last column for no variance
  expect_true(all(c("a_1", "b_1", "c_1", "a_2", "b_2","c_2") %in% colnames(result)))
  
  # Check the correct output available
  expect_true(nrow(result) == 3)
})