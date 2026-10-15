# The rankings source (source = "rank"): respondent x task x marked position,
# resp a rank. Same shard-list semantics as conj, from day one. Standard:
# ben-domingue/irw data/rankings/README.md.

test_that("rank is a known source", {
  expect_identical(irw:::.irw_resolve_source("rank"), "rank")
  expect_identical(irw:::.irw_single_datasource_cache_key("rank"), "rank_datasource")
  expect_true(grepl("^irw_rank:", irw:::.irw_datasource_specs$rank[[1]]$dataset))
  expect_identical(irw:::.irw_source_noun("rank"), "rankings shard")
})

test_that("the rank metadata fetcher reads rank_metadata, filtered to live tables", {
  local_irw_binding(".irw_env", new.env())
  read <- character()
  local_mocked_bindings(
    .irw_open_meta_dataset = function() list(
      properties = list(version = list(tag = "v60.0")),
      table = function(name) { read <<- c(read, name)
        list(to_tibble = function() tibble::tibble(table = c("eurovision_2016_final", "gone_2020")))}),
    .irw_filter_rows_to_live_tables = function(df, source) {
      expect_identical(source, "rank"); df[df$table != "gone_2020", ] })
  m <- irw:::.fetch_rank_metadata_table()
  expect_identical(read, "rank_metadata")
  expect_identical(m$table, "eurovision_2016_final")
  expect_identical(irw:::.fetch_rank_metadata_table()$table, "eurovision_2016_final")  # cached
  expect_identical(read, "rank_metadata")
})

# irw_metadata(source = "rank") is a one-line dispatch to the fetcher above; it is
# not tested through irw_metadata() because test-collections.R's mock of that
# function outlives its file (Rpkg#187), as test-conjoint.R notes.

test_that("irw_save_bibtex(source = \"rank\") reads rank_biblio", {
  local_mocked_bindings(
    .fetch_rank_biblio_table = function() tibble::tibble(
      table = "eurovision_2016_final", BibTex = "@misc{Eurovision2016, title={Jurors}}",
      DOI__for_paper_ = NA_character_),
    .fetch_biblio_table = function() stop("read the core biblio"),
    .fetch_redivis_table = function(...) TRUE)
  f <- withr::local_tempfile(fileext = ".bib")
  out <- suppressMessages(irw_save_bibtex("eurovision_2016_final", output_file = f, source = "rank"))
  expect_identical(out, "@misc{eurovision_2016_final, title={Jurors}}")
})

test_that("irw_info() on a rank table reads rank_biblio", {
  local_mocked_bindings(
    .fetch_redivis_table = function(...) structure(
      list(properties = list(numRows = 10, numBytes = 2048, variableCount = 5, url = "u")),
      dataset_version = "v1.0"),
    .irw_table_variable_names = function(...) c("id", "task", "item", "resp"),
    .fetch_rank_biblio_table = function() tibble::tibble(
      table = "eurovision_2016_final", DOI__for_paper_ = "10.1/x", URL__for_data_ = "d",
      Derived_License = "CC BY 4.0", Custom_License_Terms = NA_character_,
      Description = "Eurovision jury rankings", Reference_x = "EBU (2016)"),
    .fetch_biblio_table = function() stop("read the core biblio"))
  msgs <- capture_messages(irw_info("eurovision_2016_final", source = "rank"))
  expect_true(any(grepl("Eurovision jury rankings", msgs)))
  expect_true(any(grepl("\\(rankings\\)", msgs)))
  expect_false(any(grepl("No bibliography row", msgs)))
})

test_that("irw_filter(source = \"rank\") takes license and names the filters it cannot", {
  local_irw_pristine("irw_metadata")   # test-collections.R's mock of it outlives that file (Rpkg#187)
  local_mocked_bindings(
    .fetch_rank_metadata_table = function() tibble::tibble(
      table = c("a_rank", "b_rank"), n_respondents = c(200, 5000), method = c("irv", "stv")),
    .fetch_rank_biblio_table = function() tibble::tibble(
      table = c("a_rank", "b_rank"), Derived_License = c("CC0 1.0", "CC BY 4.0")))
  expect_identical(suppressMessages(irw_filter(source = "rank")), c("a_rank", "b_rank"))
  expect_identical(suppressMessages(irw_filter(source = "rank", license = "CC BY 4.0")), "b_rank")
  expect_error(irw_filter(source = "rank", n_items = c(5, 10)), "no column for them: n_items")
  expect_error(irw_filter(source = "rank", construct_type = "x"), "Tag filters")
  expect_error(irw_filter(source = "rank", outcome = "choice"), "only available when `source = \"conj\"`")
  lo <- irw_license_options(source = "rank")
  expect_setequal(lo$license, c("CC0 1.0", "CC BY 4.0"))
})

# irw_rank_compress(): the helper data/rankings/README.md promises. Its
# fixture and expected outputs are shared byte-for-byte with the Python
# package's tests (tests/data/rank_fixture*.csv there), so the two helpers are
# held to the same answer.

read_fixture <- function(name) {
  read.csv(test_path("fixtures", name), stringsAsFactors = FALSE)
}

test_that("irw_rank_compress drops later repeats, closes gaps and renumbers 1..k", {
  out <- irw_rank_compress(read_fixture("rank_fixture.csv"))
  expected <- read_fixture("rank_fixture_compressed.csv")
  expect_identical(names(out), names(expected))
  expect_equal(out, expected, ignore_attr = TRUE)
  expect_type(out$resp, "integer")
})

test_that("irw_rank_compress(drop_repeats = FALSE) keeps every mark and only renumbers", {
  out <- irw_rank_compress(read_fixture("rank_fixture.csv"), drop_repeats = FALSE)
  expected <- read_fixture("rank_fixture_keep_repeats.csv")
  expect_equal(out, expected, ignore_attr = TRUE)
})

test_that("irw_rank_compress keeps every row of a clean ballot and works without task", {
  d <- data.frame(id = c(2, 1, 1), item = c("c01", "c02", "c01"), resp = c(1, 2, 1))
  out <- irw_rank_compress(d)
  expect_equal(out$id, c(1, 1, 2))
  expect_equal(out$resp, c(1L, 2L, 1L))
  expect_false("task" %in% names(out))
})

test_that("irw_rank_compress refuses what is not a rankings table", {
  expect_error(irw_rank_compress(data.frame(id = 1, item = "a")), "missing resp")
  expect_error(irw_rank_compress(data.frame(id = 1, item = "a", resp = 0)), "positive integer")
  expect_error(irw_rank_compress(data.frame(id = 1, item = "a", resp = 1.5)), "positive integer")
  expect_error(irw_rank_compress(data.frame(id = 1, item = "a", resp = 1), drop_repeats = NA), "TRUE or FALSE")
  empty <- irw_rank_compress(data.frame(id = numeric(0), item = character(0), resp = numeric(0)))
  expect_equal(nrow(empty), 0L)
})

# rank is a shard list from day one (the family will pass Redivis' 1000-table
# cap), with the same semantics as core and conj.

local_two_rank_shards <- function(env = parent.frame()) {
  specs <- irw:::.irw_datasource_specs
  specs$rank <- list(
    list(user = "datapages", dataset = "irw_rank:93f5"),
    list(user = "datapages", dataset = "irw_rank_2:zzzz")
  )
  local_irw_binding(".irw_datasource_specs", specs, env = env)
  # Earlier files' mocks of these outlive them (helper-redivis.R); the shard
  # tests need the real ones.
  local_irw_pristine(c(".initialize_datasource", ".irw_open_dataset", ".irw_env"), env = env)
  withr::defer(suppressWarnings(rm(
    list = c("rank_datasource", "rank_datasource_fingerprint"), envir = irw:::.irw_env
  )), envir = env)
}

test_that(".initialize_datasource(\"rank\") opens every rank shard, cached, searched newest-first", {
  local_two_rank_shards()
  opened <- character(0)
  local_mocked_bindings(
    .irw_open_dataset = function(spec) { opened <<- c(opened, spec$dataset); spec$dataset },
    .env = asNamespace("irw")
  )
  ds <- irw:::.initialize_datasource("rank")
  expect_equal(ds, list("irw_rank:93f5", "irw_rank_2:zzzz"))
  expect_equal(irw:::.irw_order_datasources(ds, "rank"),
               list("irw_rank_2:zzzz", "irw_rank:93f5"))
  irw:::.initialize_datasource("rank")
  expect_length(opened, 2L)
})

test_that("an unreleased rank shard is skipped with a warning rather than failing every lookup", {
  local_two_rank_shards()
  local_mocked_bindings(
    .irw_open_dataset = function(spec) {
      if (grepl("_2", spec$dataset, fixed = TRUE)) {
        stop("[403 insufficient_scope] missing the required scope(s): data.edit")
      }
      spec$dataset
    },
    .env = asNamespace("irw")
  )
  expect_warning(ds <- irw:::.initialize_datasource("rank"), "unavailable IRW datasource")
  expect_equal(ds, list("irw_rank:93f5"))
})

test_that("irw_list_tables(source = \"rank\") flags a table name present in two shards", {
  fake_ds <- function(names) {
    list(list_tables = function() lapply(names, function(n)
      list(name = n, properties = list(numRows = 10, variableCount = 5))))
  }
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) {
      list(fake_ds(c("alpha_2024", "shared_2025")), fake_ds(c("shared_2025", "zeta_2026")))
    },
    .env = asNamespace("irw")
  )
  expect_warning(out <- irw_list_tables(source = "rank"),
                 "appear in more than one rank shard: shared_2025")
  expect_equal(out$name, c("alpha_2024", "shared_2025", "zeta_2026"))
})

test_that("rank shards are pinnable", {
  expect_true("irw_rank" %in% names(irw:::.irw_pinnable_specs()))
})

# Last in the file: local_mocked_bindings() does not restore a mocked
# `.initialize_datasource` (helper-redivis.R), and the shard tests above need
# the real one.
test_that("irw_fetch(dedup = TRUE) leaves a rankings table alone", {
  d <- tibble::tibble(id = c(1, 1), task = 1, item = c("c01", "c01"), resp = c(1, 2))
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) list(list(get = function() NULL,
      table = function(name) list(get = function() NULL))),
    .irw_cached_download = function(...) d,
    .env = asNamespace("irw"))
  expect_message(out <- fetch_single_data("x_rank", source = "rank", dedup = TRUE),
                 "duplicate marks as recorded")
  expect_equal(nrow(out), 2L)
})
