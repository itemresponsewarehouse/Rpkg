# tests/testthat/helper-redivis.R

skip_if_no_redivis <- function() {
  skip_if_not(requireNamespace("redivis", quietly = TRUE), 
              "Redivis package not installed")
}