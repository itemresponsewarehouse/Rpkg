# library(testthat)
# 
# # Mock metadata for testing purposes
# mock_metadata <- data.frame(
#   table_name = c("table1", "table2", "table3", "table4"),
#   id_count = c(500, 1500, 300, 800),
#   item_count = c(50, 200, 100, 75),
#   resp_count = c(10000, 20000, 5000, 8000),
#   density = c(0.2, 0.5, 0.1, 0.3),
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
# # Temporarily replace `get_metadata` with mock
# original_get_metadata <- get_metadata
# assignInNamespace("get_metadata", mock_get_metadata, ns = "irwpkg")
# 
# # Test for `show_overall_statistics` function
# test_that("show_overall_statistics computes correct values", {
#   # Capture printed output
#   output <- capture.output(show_overall_statistics())
#   
#   # Check the computed statistics using capture output
#   # Check ID Count: Min, Max, and Mean values
#   expect_true(any(grepl("ID Count:", output)))
#   expect_true(any(grepl("Min: 300", output)))    # Min ID count
#   expect_true(any(grepl("Max: 1500", output)))   # Max ID count
#   expect_true(any(grepl("Mean:", output)))       # Just check for "Mean", ignore decimals
#   
#   # Item Count: Min, Max, and Mean values
#   expect_true(any(grepl("Item Count:", output)))
#   expect_true(any(grepl("Min: 50", output)))     # Min item count
#   expect_true(any(grepl("Max: 200", output)))    # Max item count
#   expect_true(any(grepl("Mean:", output)))       # Just check for "Mean", ignore decimals
#   
#   # Response Count: Min, Max, and Mean values
#   expect_true(any(grepl("Response Count:", output)))
#   expect_true(any(grepl("Min: 5000", output)))    # Min response count
#   expect_true(any(grepl("Max: 20000", output)))   # Max response count
#   expect_true(any(grepl("Mean:", output)))        # Just check for "Mean", ignore decimals
#   
#   # Density: Min, Max, and Mean values
#   expect_true(any(grepl("Density:", output)))
#   expect_true(any(grepl("Min: 0.100", output)))   # Min density
#   expect_true(any(grepl("Max: 0.500", output)))   # Max density
#   expect_true(any(grepl("Mean:", output)))        # Just check for "Mean", ignore decimals
#   
#   # Table count and counts for specific columns (rater, rt, date)
#   expect_true(any(grepl("Number of Tables: 4", output))) 
#   expect_true(any(grepl("Tables with Raters \\(rater\\): 2", output)))  # Tables with rater
#   expect_true(any(grepl("Tables with Response Time \\(rt\\): 2", output)))  # Tables with rt
#   expect_true(any(grepl("Tables with Date \\(date\\): 3", output)))  # Tables with date
# })
# 
# # Restore original functions after tests
# assignInNamespace("get_metadata", original_get_metadata, ns = "irwpkg")