# # tests/testthat/test-irw_info.R
# 
# # Load the helper function
# source(test_path("helper-redivis.R"))
# 
# # Skip tests if redivis is not installed
# skip_if_no_redivis()
# 
# test_that("irw_info() messages something when no table name is provided", {
#   output <- capture.output(irw_info(), type = "message")
#   expect_true(length(output) > 0)
# })
