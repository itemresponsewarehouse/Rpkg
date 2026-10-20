test_that(".irw_require_redivis() passes when the client is installed", {
  skip_if_not_installed("redivis")
  withr::local_envvar(REDIVIS_API_TOKEN = "x")
  expect_true(irw:::.irw_require_redivis())
  expect_true(irw:::.irw_require_redivis_query())
})

test_that("the anonymous-read floor is read from the installed client", {
  skip_if_not_installed("redivis")
  expect_equal(
    irw:::.irw_redivis_supports_anonymous(),
    utils::packageVersion("redivis") >= "0.12.14"
  )
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_false(irw:::.irw_redivis_supports_anonymous())
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

test_that("opening a dataset is guarded by the read check", {
  local_mocked_bindings(.irw_require_redivis = function() stop("read guard", call. = FALSE))
  expect_error(
    irw:::.irw_redivis_dataset(list(user = "u", dataset = "d")),
    "read guard",
    fixed = TRUE
  )
})

# The boundary this package depends on: the login gate is on the query, not on
# the dataset handle. A query that reached .irw_require_redivis() instead would
# make `R CMD check` hang again; a dataset handle behind the query gate would
# put anonymous reading back behind a login.
test_that("queries are guarded by the login check, reads are not", {
  local_mocked_bindings(
    .irw_require_redivis_query = function() stop("query guard", call. = FALSE)
  )
  expect_error(irw:::.irw_query_tibble("SELECT 1"), "query guard", fixed = TRUE)
  # source-note swallows its own errors, so the proof that the gate ran before
  # any network call is the empty lookup it falls back to.
  expect_equal(nrow(irw:::.irw_source_note_query()), 0L)
})

# Scanned from the namespace rather than the sources, so it also holds for an
# installed package: every `redivis$query()` call must sit in a function that
# goes through the login gate, or `R CMD check` can hang at a browser login.
test_that("every redivis$query() call site goes through the login gate", {
  ns <- asNamespace("irw")
  calls <- function(fn, what) {
    txt <- paste(deparse(body(fn)), collapse = " ")
    grepl(what, txt, fixed = TRUE)
  }
  fns <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns,
                                 ifnotfound = list(NULL)))
  queriers <- names(Filter(function(f) calls(f, "redivis$query"), fns))
  expect_setequal(queriers, c(".irw_query_tibble", ".irw_source_note_query"))
  for (nm in queriers) {
    expect_true(
      calls(fns[[nm]], ".irw_require_redivis_query()"),
      info = paste(nm, "calls redivis$query() without the login gate")
    )
  }
})
