# Extracted from test-cov.R:138

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "irw", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
long_frame <- function(n = 60, seed = 1) {
  set.seed(seed)
  f <- stats::rnorm(n)
  resp <- sapply(1:4, function(j) as.numeric(cut(f + stats::rnorm(n), c(-Inf, -0.5, 0.5, Inf))))
  data.frame(id = rep(seq_len(n), 4), item = rep(paste0("q", 1:4), each = n), resp = c(resp))
}
wide_of <- function(df) {
  w <- stats::reshape(df, idvar = "id", timevar = "item", direction = "wide")
  m <- as.matrix(w[, -1]); colnames(m) <- sub("^resp\\.", "", colnames(m)); m
}

# test -------------------------------------------------------------------------
local_irw_pristine("irw_metadata")
expect_error(irw_metadata(source = "cov"), "not available for source = \"cov\"")
