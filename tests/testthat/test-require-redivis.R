test_that(".irw_require_redivis() passes when the client is installed", {
  skip_if_not_installed("redivis")
  expect_true(irw:::.irw_require_redivis())
})

test_that(".irw_require_redivis() names the install command when it is missing", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(irw:::.irw_require_redivis(), "redivis/redivis-r", fixed = TRUE)
})

test_that("the redivis call sites are guarded", {
  local_mocked_bindings(.irw_require_redivis = function() stop("guard reached", call. = FALSE))
  expect_error(irw:::.irw_query_tibble("SELECT 1"), "guard reached", fixed = TRUE)
  expect_error(
    irw:::.irw_redivis_dataset(list(user = "u", dataset = "d")),
    "guard reached",
    fixed = TRUE
  )
})
