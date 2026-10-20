test_that(".irw_require_redivis() passes when the client is installed", {
  skip_if_not_installed("redivis")
  withr::local_envvar(REDIVIS_API_TOKEN = "x")
  expect_true(irw:::.irw_require_redivis())
})

test_that(".irw_require_redivis() names the install command when it is missing", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(irw:::.irw_require_redivis(), "https://redivis.r-universe.dev", fixed = TRUE)
})

test_that(".irw_redivis_install_cmd() resolves a mirror the paste can use", {
  # Rscript with no profile: the CRAN option is the unresolved placeholder.
  withr::local_options(repos = c(CRAN = "@CRAN@"))
  cmd <- irw:::.irw_redivis_install_cmd()
  expect_false(grepl("@CRAN@", cmd, fixed = TRUE))
  expect_false(grepl("getOption", cmd, fixed = TRUE))
  expect_match(cmd, "https://redivis.r-universe.dev", fixed = TRUE)
  expect_match(cmd, "https://cloud.r-project.org", fixed = TRUE)

  withr::local_options(repos = NULL)
  expect_match(irw:::.irw_redivis_install_cmd(), "https://cloud.r-project.org", fixed = TRUE)

  # A session that has a mirror keeps it.
  withr::local_options(repos = c(CRAN = "https://mirror.example.org"))
  expect_match(irw:::.irw_redivis_install_cmd(), "https://mirror.example.org", fixed = TRUE)
})

test_that("the install command the error prints is runnable as pasted", {
  withr::local_options(repos = c(CRAN = "@CRAN@"))
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  msg <- tryCatch(irw:::.irw_require_redivis(), error = conditionMessage)
  expect_false(grepl("@CRAN@", msg, fixed = TRUE))
  expect_match(msg, "https://redivis.r-universe.dev", fixed = TRUE)
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
