# test_that("fetch_data works correctly with valid dataset", {
#   # Replace "abortion" with an actual dataset name you know exists in your IRW database
#   result <- fetch_data("abortion")
#
#   # Ensure the result is a data frame
#   expect_s3_class(result, "data.frame")
#
#   # Check that the data frame has rows and columns (not empty)
#   expect_true(nrow(result) > 0)
#   expect_true(ncol(result) > 0)
#
#   # Verify that expected column(s) are present in the data frame
#   expect_true("id" %in% names(result))  # Customize as needed
# })
#
# # test_that("fetch_data stops with an informative message for nonexistent tables", {
# #   # Check for the phrase "Unable to fetch the dataset" as a partial match
# #   expect_error(
# #     fetch_data("nonexistent_table"),
# #     "Unable to fetch the dataset",
# #     fixed = TRUE
# #   )
# # })
