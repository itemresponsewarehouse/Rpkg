library(testthat)

## Piece B of ben-domingue/irw#3127: the published metadata is the authority
## for which core tables exist, and the live-table intersection is gone.
##
## Two things are being asserted, and the second is the one the ruling calls
## load-bearing. First, that a published row survives even when no warehouse
## lists that table -- a test that only checked row counts on agreeing data
## would pass against the old intersection too, so the fakes here are built to
## *disagree*. Second, that fetching such a table explains itself: a bare
## not-found on a table the user has just seen listed is the worse half of the
## trade, and leaving it unexplained is how the trade fails to be collected.

# A shard that has no tables and refuses to be enumerated. list_tables() is
# what the removed intersection called, so a call to it here is a regression,
# not a detail: it means the filter came back.
fake_bare_shard <- function(key, counters) {
  list(
    name = key,
    properties = list(version = list(tag = "v1.0")),
    get = function() invisible(NULL),
    list_tables = function() {
      counters$listed <- counters$listed + 1L
      list()
    },
    table = function(table_name) {
      list(
        name = table_name,
        properties = list(version = list(tag = "v1.0")),
        get = function() {
          stop(
            sprintf("Not found: datapages.%s:abcd:v1_0.%s", key, table_name),
            call. = FALSE
          )
        }
      )
    }
  )
}

# An irw_meta stand-in holding one row for a table no warehouse has.
fake_meta_dataset <- function(frame, counters) {
  list(
    properties = list(version = list(tag = "v9.0")),
    get = function() invisible(NULL),
    table = function(name) {
      counters$reads <- c(counters$reads, name)
      list(
        to_tibble = function(...) frame,
        get = function() invisible(NULL)
      )
    }
  )
}

local_meta_frames <- function(counters, env = parent.frame()) {
  cache <- irw:::.irw_env
  for (nm in c("metadata_tibble", "metadata_version", "biblio_tibble",
               "biblio_version", "tags_tibble", "tags_version",
               "core_shard_map")) {
    if (exists(nm, envir = cache)) rm(list = nm, envir = cache)
  }
  withr::defer({
    for (nm in c("metadata_tibble", "metadata_version", "biblio_tibble",
                 "biblio_version", "tags_tibble", "tags_version",
                 "core_shard_map")) {
      if (exists(nm, envir = cache)) rm(list = nm, envir = cache)
    }
  }, envir = env)
  invisible(NULL)
}

test_that("a published metadata row survives when no warehouse lists it", {
  counters <- new.env(parent = emptyenv())
  counters$listed <- 0L
  counters$reads <- character(0)
  frame <- tibble::tibble(
    table = c("live_table", "withdrawn_yesterday"),
    dataset = c("item_response_warehouse", "item_response_warehouse"),
    n_responses = c(10, 20)
  )
  local_meta_frames(counters)
  local_mocked_bindings(
    .irw_open_meta_dataset = function(...) fake_meta_dataset(frame, counters),
    .initialize_datasource = function(source = "core", ...) {
      list(fake_bare_shard("item_response_warehouse", counters))
    },
    .env = asNamespace("irw")
  )

  out <- irw:::.fetch_metadata_table()

  expect_equal(nrow(out), 2L)
  expect_true("withdrawn_yesterday" %in% out$table)
  # The saving that comes with the ruling: no shard was enumerated.
  expect_equal(counters$listed, 0L)
})

test_that("biblio and tags rows are no longer intersected either", {
  counters <- new.env(parent = emptyenv())
  counters$listed <- 0L
  counters$reads <- character(0)
  frame <- tibble::tibble(
    table = c("live_table", "withdrawn_yesterday"),
    citation = c("a", "b")
  )
  local_meta_frames(counters)
  local_mocked_bindings(
    .irw_open_meta_dataset = function(...) fake_meta_dataset(frame, counters),
    .initialize_datasource = function(source = "core", ...) {
      list(fake_bare_shard("item_response_warehouse", counters))
    },
    .env = asNamespace("irw")
  )

  expect_equal(nrow(irw:::.fetch_biblio_table()), 2L)
  expect_equal(nrow(irw:::.fetch_tags_table()), 2L)
  expect_equal(counters$listed, 0L)
})

test_that("fetching a listed-but-absent table explains itself", {
  counters <- new.env(parent = emptyenv())
  counters$listed <- 0L
  counters$reads <- character(0)
  local_meta_frames(counters)
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) {
      list(fake_bare_shard("item_response_warehouse", counters))
    },
    .env = asNamespace("irw")
  )
  local_shard_map(c(withdrawn_yesterday = "item_response_warehouse"))

  msg <- tryCatch(
    irw:::.fetch_redivis_table("withdrawn_yesterday"),
    error = conditionMessage
  )

  expect_match(msg, "is in the IRW metadata but not in the warehouse", fixed = TRUE)
  expect_match(msg, "withdrawn after the metadata was last published", fixed = TRUE)
  expect_match(msg, "catches up on the next pipeline run", fixed = TRUE)
})

test_that("irw_fetch() gives the same explanation and still returns NULL", {
  counters <- new.env(parent = emptyenv())
  counters$listed <- 0L
  counters$reads <- character(0)
  local_meta_frames(counters)
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) {
      list(fake_bare_shard("item_response_warehouse", counters))
    },
    .env = asNamespace("irw")
  )
  local_shard_map(c(withdrawn_yesterday = "item_response_warehouse"))

  expect_message(
    out <- irw:::fetch_single_data("withdrawn_yesterday"),
    "is in the IRW metadata but not in the warehouse"
  )
  expect_null(out)
})

test_that("a table in neither place keeps the plain not-found message", {
  counters <- new.env(parent = emptyenv())
  counters$listed <- 0L
  counters$reads <- character(0)
  local_meta_frames(counters)
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) {
      list(fake_bare_shard("item_response_warehouse", counters))
    },
    .env = asNamespace("irw")
  )
  local_shard_map(c(some_other_table = "item_response_warehouse"))

  msg <- tryCatch(irw:::.fetch_redivis_table("ghost"), error = conditionMessage)

  expect_match(msg, "does not exist in IRW", fixed = TRUE)
  expect_false(grepl("metadata but not in the warehouse", msg, fixed = TRUE))
})

test_that("an unreadable map leaves the plain message standing", {
  # Choosing between two messages must never turn into a second failure. The
  # failure is injected at the metadata read rather than by mocking
  # .irw_core_shard_map(): that binding does not get restored for the files
  # that run after this one (see helper-redivis.R).
  counters <- new.env(parent = emptyenv())
  counters$listed <- 0L
  counters$reads <- character(0)
  local_meta_frames(counters)
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) {
      list(fake_bare_shard("item_response_warehouse", counters))
    },
    .irw_open_meta_dataset = function(...) stop("metadata unreadable"),
    .env = asNamespace("irw")
  )

  msg <- tryCatch(irw:::.fetch_redivis_table("ghost"), error = conditionMessage)

  expect_match(msg, "does not exist in IRW", fixed = TRUE)
})

test_that("a non-core source never claims the metadata lists the table", {
  expect_false(irw:::.irw_table_listed_in_metadata("anything", source = "sim"))
  expect_false(irw:::.irw_table_listed_in_metadata("anything", source = "comp"))
})
