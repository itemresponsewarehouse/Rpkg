library(testthat)

# The session cache is a package-level environment rather than a binding, so it
# is set directly and removed on exit: `local_mocked_bindings()` would not
# restore a mocked `.irw_core_shard_map()` for the files that run after this one
# (see helper-redivis.R). Setting the cache also exercises the real lookup.
local_shard_map <- function(map, env = parent.frame()) {
  cache <- irw:::.irw_env
  had <- exists("core_shard_map", envir = cache)
  old <- if (had) get("core_shard_map", envir = cache) else NULL
  assign("core_shard_map", map, envir = cache)
  withr::defer(
    if (had) {
      assign("core_shard_map", old, envir = cache)
    } else {
      rm("core_shard_map", envir = cache)
    },
    envir = env
  )
  invisible(NULL)
}

# A stand-in for an opened Redivis dataset that records every round trip it is
# asked to make: one for the dataset `get()`, one for the table `get()`.
fake_shard <- function(key, tables, calls) {
  list(
    name = key,
    properties = list(version = list(tag = "v1.0")),
    get = function() {
      calls$n <- calls$n + 1L
      invisible(NULL)
    },
    table = function(table_name) {
      list(
        name = table_name,
        properties = list(version = list(tag = "v1.0")),
        get = function() {
          calls$n <- calls$n + 1L
          if (!table_name %in% tables) {
            stop(
              sprintf("Not found: datapages.%s:abcd:v1_0.%s", key, table_name),
              call. = FALSE
            )
          }
          invisible(NULL)
        }
      )
    }
  )
}

test_that(".irw_shard_index orders shards numerically, not lexically", {
  keys <- c("item_response_warehouse", "item_response_warehouse_2",
            "item_response_warehouse_10")
  expect_equal(irw:::.irw_shard_index(keys), c(1L, 2L, 10L))
  expect_equal(irw:::.irw_shard_index("irw_text"), 1L)
})

test_that(".irw_shard_map_from_metadata maps table names to their shard", {
  meta <- data.frame(
    table = c("alpha", "BETA", "gamma"),
    dataset = c("item_response_warehouse", "item_response_warehouse_3",
                "item_response_warehouse_2"),
    stringsAsFactors = FALSE
  )
  map <- irw:::.irw_shard_map_from_metadata(meta)
  # Keyed lowercase, because every lookup is on a lowercased name.
  expect_equal(map[["alpha"]], "item_response_warehouse")
  expect_equal(map[["beta"]], "item_response_warehouse_3")
  expect_equal(map[["gamma"]], "item_response_warehouse_2")
})

test_that(".irw_shard_map_from_metadata keeps the newest copy of a duplicate", {
  # A name published in two shards resolves to its newest copy today, silently
  # (ARCHITECTURE.md section 2). The map has to agree, and _10 is newer than _2.
  meta <- data.frame(
    table = c("twice", "twice", "once"),
    dataset = c("item_response_warehouse_10", "item_response_warehouse_2",
                "item_response_warehouse"),
    stringsAsFactors = FALSE
  )
  map <- irw:::.irw_shard_map_from_metadata(meta)
  expect_equal(map[["twice"]], "item_response_warehouse_10")
  expect_equal(map[["once"]], "item_response_warehouse")
})

test_that(".irw_shard_map_from_metadata ignores a reference id on the dataset", {
  meta <- data.frame(
    table = "alpha",
    dataset = "item_response_warehouse_3:5xaj",
    stringsAsFactors = FALSE
  )
  expect_equal(
    irw:::.irw_shard_map_from_metadata(meta)[["alpha"]],
    "item_response_warehouse_3"
  )
})

test_that(".irw_shard_map_from_metadata gives up rather than guessing", {
  expect_null(irw:::.irw_shard_map_from_metadata(NULL))
  expect_null(irw:::.irw_shard_map_from_metadata(
    data.frame(table = character(0), dataset = character(0))
  ))
  # An older irw_meta without the column, or rows with nothing in it.
  expect_null(irw:::.irw_shard_map_from_metadata(
    data.frame(table = "alpha", n_items = 3L)
  ))
  expect_null(irw:::.irw_shard_map_from_metadata(
    data.frame(table = "alpha", dataset = NA_character_)
  ))
})

test_that(".irw_shard_first moves the resolved shard to the front", {
  local_shard_map(c(alpha = "item_response_warehouse"))
  ds <- list(
    list(name = "item_response_warehouse_3"),
    list(name = "item_response_warehouse_2"),
    list(name = "item_response_warehouse")
  )
  out <- irw:::.irw_shard_first(ds, "alpha", "core")
  expect_equal(vapply(out, function(d) d$name, character(1)),
               c("item_response_warehouse", "item_response_warehouse_3",
                 "item_response_warehouse_2"))
  # Reordering, never truncating: the rest of the list is still searched, which
  # is what keeps the not-found path and `.irw_handle_datasource_error()`'s
  # `length(ds_list)` test behaving as they did.
  expect_length(out, 3L)
})

test_that(".irw_shard_first is case-insensitive about the table name", {
  local_shard_map(c(alpha = "item_response_warehouse"))
  ds <- list(list(name = "item_response_warehouse_2"),
             list(name = "item_response_warehouse"))
  out <- irw:::.irw_shard_first(ds, "ALPHA", "core")
  expect_equal(out[[1]]$name, "item_response_warehouse")
})

test_that(".irw_shard_first leaves the order alone when it cannot resolve", {
  ds <- list(list(name = "item_response_warehouse_2"),
             list(name = "item_response_warehouse"))

  # No map at all (irw_meta unreachable, or an older release without the column).
  local_shard_map(NULL)
  expect_identical(irw:::.irw_shard_first(ds, "alpha", "core"), ds)

  # A table the map has never heard of: uploaded since irw_meta was published.
  local_shard_map(c(other = "item_response_warehouse"))
  expect_identical(irw:::.irw_shard_first(ds, "alpha", "core"), ds)

  # A shard that is not in this session's list, e.g. one dropped because it had
  # no release at the pinned IRW version.
  local_shard_map(c(alpha = "item_response_warehouse_99"))
  expect_identical(irw:::.irw_shard_first(ds, "alpha", "core"), ds)
})

test_that(".irw_shard_first only acts on core, and only on a real shard list", {
  local_shard_map(c(alpha = "item_response_warehouse"))
  ds <- list(list(name = "item_response_warehouse_2"),
             list(name = "item_response_warehouse"))
  # conj and rank have no published table-to-shard column.
  expect_identical(irw:::.irw_shard_first(ds, "alpha", "conj"), ds)
  # A single-dataset source has nothing to reorder.
  single <- list(list(name = "irw_simsyn"))
  expect_identical(irw:::.irw_shard_first(single, "alpha", "core"), single)
})

test_that("shard resolution can be switched off", {
  local_shard_map(c(alpha = "item_response_warehouse"))
  ds <- list(list(name = "item_response_warehouse_2"),
             list(name = "item_response_warehouse"))
  withr::local_options(irw.shard_resolution = FALSE)
  expect_identical(irw:::.irw_shard_first(ds, "alpha", "core"), ds)
})

# Three shards, newest last in config order, so the search order is 3, 2, 1.
fake_core <- function(calls, holder = "item_response_warehouse") {
  list(
    fake_shard("item_response_warehouse",
               if (identical(holder, "item_response_warehouse")) "alpha" else character(0), calls),
    fake_shard("item_response_warehouse_2",
               if (identical(holder, "item_response_warehouse_2")) "alpha" else character(0), calls),
    fake_shard("item_response_warehouse_3",
               if (identical(holder, "item_response_warehouse_3")) "alpha" else character(0), calls)
  )
}

test_that("a fetch with the map in hand makes one shard's worth of calls", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  ds_list <- fake_core(calls)
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) ds_list,
    .env = asNamespace("irw")
  )

  # Today's behaviour: searched newest-first, so a table in the oldest shard
  # costs a round trip pair for every shard ahead of it.
  local_shard_map(NULL)
  expect_equal(irw:::.fetch_redivis_table("alpha")$name, "alpha")
  expect_equal(calls$n, 6L)

  calls$n <- 0L
  local_shard_map(c(alpha = "item_response_warehouse"))
  tbl <- irw:::.fetch_redivis_table("alpha")
  expect_equal(tbl$name, "alpha")
  expect_equal(attr(tbl, "dataset_version"), "v1.0")
  expect_equal(calls$n, 2L)
})

test_that("a fetch from the newest shard never reads the map", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  ds_list <- fake_core(calls, holder = "item_response_warehouse_3")
  reads <- 0L
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) ds_list,
    .irw_read_core_shard_map = function() {
      reads <<- reads + 1L
      c(alpha = "item_response_warehouse_3")
    },
    .env = asNamespace("irw")
  )
  # No map cached: this is the first fetch of a session.
  cache <- irw:::.irw_env
  if (exists("core_shard_map", envir = cache)) {
    rm("core_shard_map", envir = cache)
  }
  withr::defer(
    if (exists("core_shard_map", envir = cache)) rm("core_shard_map", envir = cache)
  )

  expect_equal(irw:::.fetch_redivis_table("alpha")$name, "alpha")
  # Exactly what it cost before this change, and nothing was read to get it.
  expect_equal(calls$n, 2L)
  expect_equal(reads, 0L)
})

test_that("a miss on the newest shard resolves the rest instead of walking it", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  ds_list <- fake_core(calls, holder = "item_response_warehouse")
  reads <- 0L
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) ds_list,
    .irw_read_core_shard_map = function() {
      reads <<- reads + 1L
      c(alpha = "item_response_warehouse")
    },
    .env = asNamespace("irw")
  )
  cache <- irw:::.irw_env
  if (exists("core_shard_map", envir = cache)) {
    rm("core_shard_map", envir = cache)
  }
  withr::defer(
    if (exists("core_shard_map", envir = cache)) rm("core_shard_map", envir = cache)
  )

  expect_equal(irw:::.fetch_redivis_table("alpha")$name, "alpha")
  # The newest shard, then straight to the one that has it: 4 rather than 6,
  # and the saving grows with the number of shards (seven in production).
  expect_equal(calls$n, 4L)
  expect_equal(reads, 1L)
})

test_that("a table that exists nowhere fails exactly as it did before", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  ds_list <- list(
    fake_shard("item_response_warehouse", character(0), calls),
    fake_shard("item_response_warehouse_2", character(0), calls)
  )
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) ds_list,
    .env = asNamespace("irw")
  )

  local_shard_map(NULL)
  before <- tryCatch(irw:::.fetch_redivis_table("ghost"), error = conditionMessage)

  # Resolution pointing at a shard the table has since left: the remaining
  # shards are still searched and the same message comes back.
  local_shard_map(c(ghost = "item_response_warehouse"))
  after <- tryCatch(irw:::.fetch_redivis_table("ghost"), error = conditionMessage)

  expect_type(before, "character")
  expect_identical(after, before)
})
