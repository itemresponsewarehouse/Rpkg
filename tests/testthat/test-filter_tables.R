# # Mock metadata for testing
# mock_metadata <- data.frame(
#   table_name = c("table1", "table2", "table3", "table4"),
#   id_count = c(500, 1500, 300, 800),
#   item_count = c(50, 200, 100, 75),
#   resp_count = c(10000, 20000, 5000, 8000),
#   sparsity = c(0.2, 0.5, 0.1, 0.3),
#   has_date = c(1, 0, 1, 1),
#   has_rt = c(0, 1, 1, 0),
#   has_rater = c(1, 1, 0, 0),
#   stringsAsFactors = FALSE
# )
# 
# # Mock `get_metadata` function
# mock_get_metadata <- function() {
#   return(mock_metadata)
# }
# 
# # Temporarily replace `get_metadata` with the mock
# original_get_metadata <- get_metadata
# assignInNamespace("get_metadata", mock_get_metadata, ns = "irwpkg")
# 
# test_that("filter_tables returns correct results for valid inputs", {
#   # Test 1: Filter by ID count
#   result <- suppressMessages(filter_tables(id_count = c(500, 1000)))
#   expect_equal(result, c("table1", "table4"))
#   
#   # Test 2: Filter by sparsity
#   result <- suppressMessages(filter_tables(sparsity = c(0.1, 0.3)))
#   expect_equal(result, c("table1", "table3", "table4"))
#   
#   # Test 3: Filter by presence of date and RT columns
#   result <- suppressMessages(filter_tables(has_date = TRUE, has_rt = FALSE))
#   expect_equal(result, c("table1", "table4"))
#   
#   # Test 4: Combine numeric and column presence filters
#   result <- suppressMessages(filter_tables(id_count = c(500, 1500), has_rater = TRUE))
#   expect_equal(result, c("table1", "table2"))
# })
# 
# test_that("filter_tables handles no matches gracefully", {
#   # Test 5: No matching tables, check return value
#   result <- suppressMessages(filter_tables(id_count = c(2000, 3000)))
#   expect_equal(result, character(0))
#   
#   # Test 6: Check the message is emitted (but don't print it to the console)
#   expect_message(
#     filter_tables(id_count = c(2000, 3000)),
#     "No tables match the specified criteria."
#   )
# })
# 
# test_that("filter_tables validates input ranges correctly", {
#   # Test 7: Invalid range for ID count
#   expect_error(filter_tables(id_count = c("a", "b")), 
#                "'id_count' must be a numeric vector of length 2")
#   
#   # Test 8: Invalid length for item count
#   expect_error(filter_tables(item_count = c(100)), 
#                "'item_count' must be a numeric vector of length 2")
# })
# 
# test_that("filter_tables handles missing required columns gracefully", {
#   # Mock metadata missing a required column
#   mock_metadata_missing <- mock_metadata
#   mock_metadata_missing$item_count <- NULL
#   
#   # Replace `get_metadata` with modified mock
#   assignInNamespace("get_metadata", function() mock_metadata_missing, ns = "irwpkg")
#   
#   # Test 9: Missing required columns
#   expect_error(filter_tables(item_count = c(50, 100)), 
#                "The following required columns are missing from metadata: item_count")
#   
#   # Restore original mock metadata
#   assignInNamespace("get_metadata", mock_get_metadata, ns = "irwpkg")
# })
# 
# # Restore original `get_metadata` function after tests
# assignInNamespace("get_metadata", original_get_metadata, ns = "irwpkg")