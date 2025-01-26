# test_that("fetch_data works correctly for valid inputs", {
#   # Mock `fetch_table` to return a simulated Redivis table object
#   mock_table <- list(
#     to_data_frame = function() {
#       data.frame(x = 1:5, y = letters[1:5])
#     }
#   )
#   
#   # Mock the `fetch_table` function
#   mock_fetch_table <- function(dataset_name) {
#     if (dataset_name %in% c("valid_dataset1", "valid_dataset2")) {
#       return(mock_table)
#     } else {
#       stop("Dataset not found.")
#     }
#   }
#   
#   # Temporarily override `fetch_table` with the mock
#   original_fetch_table <- fetch_table
#   assignInNamespace("fetch_table", mock_fetch_table, ns = "irwpkg")
#   
#   # Test 1: Fetch a single valid dataset
#   df <- fetch_data("valid_dataset1")
#   expect_s3_class(df, "data.frame")
#   expect_equal(nrow(df), 5)
#   expect_equal(ncol(df), 2)
#   expect_named(df, c("x", "y"))
#   
#   # Test 2: Fetch multiple valid datasets
#   datasets <- fetch_data(c("valid_dataset1", "valid_dataset2"))
#   expect_type(datasets, "list")
#   expect_length(datasets, 2)
#   expect_true(all(names(datasets) == c("valid_dataset1", "valid_dataset2")))
#   expect_s3_class(datasets[["valid_dataset1"]], "data.frame")
#   expect_s3_class(datasets[["valid_dataset2"]], "data.frame")
#   
#   # Restore original `fetch_table`
#   assignInNamespace("fetch_table", original_fetch_table, ns = "irwpkg")
# })
# 
# test_that("fetch_data handles errors correctly", {
#   # Mock `fetch_table` to simulate errors
#   mock_table <- list(
#     to_data_frame = function() {
#       stop("Conversion error.")
#     }
#   )
#   
#   mock_fetch_table <- function(dataset_name) {
#     if (dataset_name == "valid_dataset") {
#       return(mock_table)
#     } else {
#       stop("Dataset not found.")
#     }
#   }
#   
#   # Temporarily override `fetch_table` with the mock
#   original_fetch_table <- fetch_table
#   assignInNamespace("fetch_table", mock_fetch_table, ns = "irwpkg")
#   
#   # Test 3: Invalid dataset name
#   expect_error(fetch_data("invalid_dataset"), 
#                "Unable to fetch the dataset 'invalid_dataset'")
#   
#   # Test 4: Error during data frame conversion
#   expect_error(fetch_data("valid_dataset"), 
#                "Failed to convert the dataset 'valid_dataset' to a data frame")
#   
#   # Restore original `fetch_table`
#   assignInNamespace("fetch_table", original_fetch_table, ns = "irwpkg")
# })