# Bounded retrieval (review R-20, irw#3221). The assertions that matter are on
# the wire: what was asked of Redivis, and whether a slice was written to the
# disk cache. A test that only checked the returned tibble would pass just as
# well if every bounded fetch exported the whole table first -- which is the
# defect, since 326 IRW tables are over a million rows.

skip_if_not_installed("arrow")

pushdown_frame <- function(n = 10) {
  data.frame(
    id = paste0("p", (seq_len(n) - 1) %/% 2),
    item = paste0("q", (seq_len(n) - 1) %% 2),
    resp = as.numeric(seq_len(n) - 1),
    cov_age = rep(30, n),
    stringsAsFactors = FALSE
  )
}

# A loaded table handle that records the arguments of every download.
recording_table <- function(frame = pushdown_frame(), name = "t_push",
                            hash = "hpush",
                            ref = "datapages.item_response_warehouse:as2e:v5_0") {
  calls <- new.env()
  calls$tibble <- list()
  calls$arrow <- 0L
  list(
    calls = calls,
    properties = list(
      name = name, hash = hash,
      qualifiedReference = paste0(ref, ".", name, ":abcd")
    ),
    get = function() invisible(NULL),
    list_variables = function(...) lapply(names(frame), function(n) list(name = n)),
    to_arrow_table = function(...) {
      calls$arrow <- calls$arrow + 1L
      arrow::arrow_table(frame)
    },
    to_tibble = function(max_results = NULL, variables = NULL, ...) {
      calls$tibble <- c(calls$tibble, list(list(max_results = max_results,
                                                variables = variables)))
      out <- frame
      if (!is.null(variables)) out <- out[, variables, drop = FALSE]
      if (!is.null(max_results)) out <- out[seq_len(min(max_results, nrow(out))), , drop = FALSE]
      tibble::as_tibble(out)
    }
  )
}

# Mocked the way test-disk-cache.R mocks: `.initialize_datasource` through
# local_mocked_bindings with `.env = env`, and anything else through
# local_irw_binding, which restores on exit. testthat 3.3.2 does not restore a
# binding mocked into the namespace itself (helper-redivis.R), so the first
# version of this helper leaked `.irw_shard_first` into every test file that
# happened to run after it.
local_recording_source <- function(tbl, env = parent.frame()) {
  ds <- list(get = function() invisible(NULL), table = function(name) tbl)
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) list(ds),
    .package = "irw",
    .env = env
  )
  local_irw_binding(".irw_source_note", function(...) invisible(NULL), env = env)
}

test_that("max_rows and columns are pushed to Redivis, not applied after", {
  tbl <- recording_table()
  local_recording_source(tbl)
  withr::local_options(irw.cache = FALSE)

  df <- irw_fetch("t_push", max_rows = 4, columns = c("id", "item", "resp"))

  expect_equal(nrow(df), 4)
  expect_equal(names(df), c("id", "item", "resp"))
  expect_length(tbl$calls$tibble, 1)
  expect_equal(tbl$calls$tibble[[1]]$max_results, 4L)
  expect_equal(tbl$calls$tibble[[1]]$variables, c("id", "item", "resp"))
  # Never the whole-table path: that is the export this argument exists to avoid.
  expect_equal(tbl$calls$arrow, 0L)
})

test_that("a slice is never written to the cache, and a cached table serves one", {
  cache <- withr::local_tempdir()
  withr::local_options(irw.cache = TRUE)
  withr::local_envvar(IRW_CACHE_DIR = cache)
  tbl <- recording_table()
  local_recording_source(tbl)

  bounded <- irw_fetch("t_push", max_rows = 3)
  expect_equal(nrow(bounded), 3)
  expect_equal(tbl$calls$arrow, 0L)
  expect_equal(length(list.files(cache, recursive = TRUE, pattern = "parquet$")), 0)

  whole <- irw_fetch("t_push")
  expect_equal(nrow(whole), 10)
  expect_equal(tbl$calls$arrow, 1L)

  # Now the whole table is on disk, a slice costs nothing at all.
  before <- length(tbl$calls$tibble)
  again <- irw_fetch("t_push", max_rows = 2, columns = c("id", "resp"))
  expect_equal(nrow(again), 2)
  expect_equal(names(again), c("id", "resp"))
  expect_equal(tbl$calls$arrow, 1L)
  expect_equal(length(tbl$calls$tibble), before)
})

test_that("a column the table does not have is an error", {
  tbl <- recording_table()
  local_recording_source(tbl)
  withr::local_options(irw.cache = FALSE)
  expect_error(irw_fetch("t_push", columns = c("id", "cov_nonesuch")),
               "no column")
})

test_that("the arguments are validated the way the Python client validates them", {
  expect_error(irw:::.irw_check_pushdown(max_rows = 0), "at least 1")
  expect_error(irw:::.irw_check_pushdown(max_rows = TRUE), "whole number")
  expect_error(irw:::.irw_check_pushdown(max_rows = 1.5), "whole number")
  expect_error(irw:::.irw_check_pushdown(max_rows = c(1, 2)), "whole number")
  expect_error(irw:::.irw_check_pushdown(columns = character(0)), "cannot be empty")
  expect_error(irw:::.irw_check_pushdown(columns = c("id", "")), "non-empty")
  expect_error(irw:::.irw_check_pushdown(columns = 1:2), "character vector")
  expect_equal(irw:::.irw_check_pushdown(max_rows = 5)$max_rows, 5L)
  expect_null(irw:::.irw_check_pushdown()$max_rows)
})

test_that("dedup keeps every column for the download and narrows afterwards", {
  tbl <- recording_table()
  local_recording_source(tbl)
  withr::local_options(irw.cache = FALSE)

  df <- irw_fetch("t_push", dedup = TRUE, columns = c("id", "resp"), max_rows = 6)

  # Deduplication groups on id, item and wave, so the download cannot be
  # narrowed to the caller's columns first.
  expect_null(tbl$calls$tibble[[1]]$variables)
  expect_equal(tbl$calls$tibble[[1]]$max_results, 6L)
  expect_equal(names(df), c("id", "resp"))
})
