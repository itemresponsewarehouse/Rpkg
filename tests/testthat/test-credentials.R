# Each test points HOME at an empty folder and clears the token, so nothing
# reaches the developer's real login. Older redivis builds fixed the
# credentials path at build time, which no HOME change hides; skip there.
local_no_credentials <- function(env = parent.frame()) {
  withr::local_envvar(
    HOME = withr::local_tempdir(.local_envir = env),
    REDIVIS_API_TOKEN = NA,
    .local_envir = env
  )
  testthat::skip_if(
    irw:::.irw_redivis_credentials_available(),
    "this redivis build has a login path fixed at build time"
  )
}

test_that("the credentials check finds a token in the environment", {
  withr::local_envvar(REDIVIS_API_TOKEN = "x")
  expect_true(irw:::.irw_redivis_credentials_available())
})

test_that("the credentials check finds a cached login under HOME", {
  home <- withr::local_tempdir()
  dir.create(file.path(home, ".redivis"))
  writeLines("{}", file.path(home, ".redivis", "r_credentials"))
  withr::local_envvar(HOME = home, REDIVIS_API_TOKEN = NA)
  expect_true(irw:::.irw_redivis_credentials_available())
})

test_that("without credentials a non-interactive call stops before the client is touched", {
  skip_if_not_installed("redivis")
  skip_if(interactive(), "Requires a non-interactive session")
  local_no_credentials()
  local_irw_pristine(c(".irw_redivis_dataset", ".irw_require_redivis"))
  expect_false(irw_has_credentials())
  expect_error(irw:::.irw_require_redivis(), "needs a Redivis login", fixed = TRUE)
  expect_error(
    irw:::.irw_redivis_dataset(list(user = "u", dataset = "d")),
    "needs a Redivis login",
    fixed = TRUE
  )
  expect_length(list.files(Sys.getenv("HOME"), all.files = TRUE, no.. = TRUE), 0)
})

test_that("irw_has_credentials() is FALSE without the redivis client", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_false(irw_has_credentials())
})
