test_that("download_data works correctly", {
  # Mock `fetch_table` to return a simulated Redivis table object
  mock_table <- list(
    name = "mock_table",
    download = function(path, overwrite) {
      # Simulate the download process
      if (file.exists(path) && !overwrite) {
        stop("File already exists and overwrite is set to FALSE.")
      }
      file.create(path)
    }
  )
  
  # Mock the `fetch_table` function
  mock_fetch_table <- function(table_name) {
    if (table_name != "valid_table") {
      stop("Invalid table name.")
    }
    return(mock_table)
  }
  
  # Temporarily override `fetch_table` with the mock
  original_fetch_table <- fetch_table
  assignInNamespace("fetch_table", mock_fetch_table, ns = "irwpkg")
  
  # Test 1: Successful download
  temp_file <- tempfile()
  expect_message(download_data("valid_table", path = temp_file, overwrite = TRUE), 
                 "Dataset downloaded to:")
  expect_true(file.exists(temp_file))
  unlink(temp_file) # Clean up temporary file
  
  # Test 2: File exists and overwrite = FALSE
  temp_file <- tempfile()
  file.create(temp_file)
  expect_error(download_data("valid_table", path = temp_file, overwrite = FALSE), 
               "File already exists and overwrite is set to FALSE.")
  unlink(temp_file) # Clean up temporary file
  
  # Test 3: Invalid table name
  expect_error(download_data("invalid_table"), "Invalid table name.")
  
  # Restore original `fetch_table`
  assignInNamespace("fetch_table", original_fetch_table, ns = "irwpkg")
})