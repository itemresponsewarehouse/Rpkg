# library(testthat)
# 
# test_that("Basic 2PL simulation works and returns IRW-compliant output", {
#   set.seed(123)
#   a <- matrix(rlnorm(5, 0.2, 0.1))
#   d <- rnorm(5)
#   result <- irw_simdata_logistic(a = a, d = d, n_id = 10, itemtype = "2PL")
#   
#   expect_s3_class(result, "data.frame")
#   expect_true(all(c("id", "item", "resp") %in% colnames(result)))
#   expect_equal(nrow(result), 10 * 5)
#   expect_true(is.numeric(result$resp))
# })
# 
# test_that("Simulation using Theta works correctly", {
#   set.seed(123)
#   a <- matrix(rlnorm(3, 0.2, 0.1))
#   d <- rnorm(3)
#   Theta <- matrix(rnorm(20), ncol = 1)
#   result <- irw_simdata_logistic(a = a, d = d, Theta = Theta)
#   
#   expect_equal(length(unique(result$id)), 20)
#   expect_equal(length(unique(result$item)), 3)
# })
# 
# test_that("1PL and 3PL itemtypes are accepted", {
#   set.seed(42)
#   a <- matrix(1, 4, 1)  # constant slope for 1PL
#   d <- rnorm(4)
#   res_1pl <- irw_simdata_logistic(a = a, d = d, n_id = 15, itemtype = "1PL")
#   expect_equal(nrow(res_1pl), 15 * 4)
#   
#   set.seed(42)
#   a <- matrix(rlnorm(4, 0.2, 0.1))
#   d <- rnorm(4)
#   guess <- rep(0.2, 4)
#   upper <- rep(1.0, 4)
#   res_3pl <- irw_simdata_logistic(a = a, d = d, n_id = 15, itemtype = "3PL",
#                                   guess = guess, upper = upper)
#   expect_equal(nrow(res_3pl), 15 * 4)
# })
# 
# test_that("Invalid itemtype throws error", {
#   a <- matrix(rlnorm(2, 0.2, 0.1))
#   d <- rnorm(2)
#   expect_error(
#     irw_simdata_logistic(a = a, d = d, n_id = 5, itemtype = "nominal"),
#     "Only logistic models supported"
#   )
# })
# 
# test_that("Missing arguments produce errors", {
#   a <- matrix(rlnorm(2, 0.2, 0.1))
#   d <- rnorm(2)
#   
#   expect_error(irw_simdata_logistic(a = a, d = d, itemtype = "2PL"),
#                "must be specified")
#   
#   expect_error(irw_simdata_logistic(d = d, n_id = 10),
#                "argument \"a\" is missing")
# })