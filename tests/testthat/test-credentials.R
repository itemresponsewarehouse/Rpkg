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

test_that("without credentials a non-interactive query stops before the client is touched", {
  skip_if_not_installed("redivis")
  skip_if(interactive(), "Requires a non-interactive session")
  local_no_credentials()
  local_irw_pristine(c(".irw_redivis_dataset", ".irw_require_redivis",
                       ".irw_require_redivis_client", ".irw_require_redivis_query"))
  expect_false(irw_has_credentials())
  expect_error(irw:::.irw_require_redivis_query(), "needs a Redivis login", fixed = TRUE)
  expect_error(irw:::.irw_query_tibble("SELECT 1"), "needs a Redivis login", fixed = TRUE)
  expect_length(list.files(Sys.getenv("HOME"), all.files = TRUE, no.. = TRUE), 0)
})

test_that("without credentials a non-interactive read is allowed on a client that can", {
  skip_if_not_installed("redivis")
  skip_if(interactive(), "Requires a non-interactive session")
  skip_if_not(irw:::.irw_redivis_supports_anonymous(),
              "this redivis build authenticates every request")
  local_no_credentials()
  local_irw_pristine(c(".irw_redivis_dataset", ".irw_require_redivis",
                       ".irw_require_redivis_client", ".irw_require_redivis_query"))
  expect_false(irw_has_credentials())
  expect_true(irw:::.irw_require_redivis())
  # Builds the handle only; no request goes out until $get(), so this still
  # touches neither the network nor HOME.
  expect_error(irw:::.irw_redivis_dataset(list(user = "datapages", dataset = "d")), NA)
  expect_length(list.files(Sys.getenv("HOME"), all.files = TRUE, no.. = TRUE), 0)
})

test_that("an older redivis still stops a non-interactive read without credentials", {
  skip_if_not_installed("redivis")
  skip_if(interactive(), "Requires a non-interactive session")
  local_no_credentials()
  local_irw_pristine(c(".irw_redivis_dataset", ".irw_require_redivis",
                       ".irw_require_redivis_client", ".irw_require_redivis_query"))
  local_mocked_bindings(.irw_redivis_supports_anonymous = function() FALSE)
  msg <- tryCatch(irw:::.irw_require_redivis(), error = conditionMessage)
  expect_match(msg, "needs a Redivis login", fixed = TRUE)
  # The way out of an old client is to upgrade it, so say so.
  expect_match(msg, "0.12.14", fixed = TRUE)
  expect_match(msg, "https://redivis.r-universe.dev", fixed = TRUE)
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
