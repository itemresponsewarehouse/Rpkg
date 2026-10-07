## The fetch-time credit note for tables found via openESM and the like
## (ben-domingue/irw#2421). Offline: the biblio query is replaced, so nothing
## here reaches Redivis. The rules under test are the ones settled on the issue
## -- once per session per source, an off switch, and silence (never an error)
## when the lookup fails.

lookup_fixture <- function() {
  data.frame(
    table = c("bailon_2020_covidaffect", "other_esm", "gilbert_meta_1"),
    via = c("openESM", "openESM", "gilbert_meta"),
    url = c("https://zenodo.org/records/22763396", "", ""),
    stringsAsFactors = FALSE
  )
}

fresh_session <- function(env = parent.frame()) {
  withr::local_envvar(IRW_SOURCE_NOTE = NA, IRW_CACHE_DIR = tempfile("irw-sn-"),
                      .local_envir = env)
  withr::local_options(irw.source_note = NULL, .local_envir = env)
  .irw_env$source_note_lookup <- NULL
  .irw_env$source_note_shown <- NULL
  withr::defer({
    .irw_env$source_note_lookup <- NULL
    .irw_env$source_note_shown <- NULL
  }, envir = env)
}

new_session <- function() {
  .irw_env$source_note_lookup <- NULL
  .irw_env$source_note_shown <- NULL
}

notes <- function(expr) {
  out <- character(0)
  withCallingHandlers(expr, message = function(m) {
    out <<- c(out, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  out
}

test_that("the note names the source, the table's page and the citation", {
  fresh_session()
  local_mocked_bindings(.irw_source_note_query = function() lookup_fixture())
  n <- notes(.irw_source_note("Bailon_2020_CovidAffect"))
  expect_length(n, 1)
  expect_match(n, "found via openESM", fixed = TRUE)
  expect_match(n, "https://zenodo.org/records/22763396", fixed = TRUE)
  expect_match(n, "10.3758/s13428-026-03112-y", fixed = TRUE)
  expect_match(n, "Kloft, M., B\u00fcchner, A., Zhang, Y.", fixed = TRUE)
})

test_that("once per session per source, not per table", {
  fresh_session()
  local_mocked_bindings(.irw_source_note_query = function() lookup_fixture())
  n <- notes({
    .irw_source_note("bailon_2020_covidaffect")
    .irw_source_note(c("other_esm", "gilbert_meta_1"))
  })
  expect_length(n, 2)
  expect_match(n[2], "found via gilbert_meta.", fixed = TRUE)
  expect_no_match(n[2], "cite")
})

test_that("tables with no Source_via, and non-core sources, are silent", {
  fresh_session()
  local_mocked_bindings(.irw_source_note_query = function() lookup_fixture())
  expect_length(notes(.irw_source_note("environment_ltm")), 0)
  expect_length(notes(.irw_source_note("bailon_2020_covidaffect", source = "nom")), 0)
})

test_that("the option and the environment variable switch it off", {
  fresh_session()
  local_mocked_bindings(.irw_source_note_query = function() lookup_fixture())
  withr::with_options(list(irw.source_note = FALSE),
                      expect_length(notes(.irw_source_note("bailon_2020_covidaffect")), 0))
  withr::with_envvar(c(IRW_SOURCE_NOTE = "0"),
                     expect_length(notes(.irw_source_note("bailon_2020_covidaffect")), 0))
  expect_length(notes(.irw_source_note("bailon_2020_covidaffect")), 1)
})

test_that("suppressMessages() silences it", {
  fresh_session()
  local_mocked_bindings(.irw_source_note_query = function() lookup_fixture())
  expect_silent(suppressMessages(.irw_source_note("bailon_2020_covidaffect")))
})

test_that("a failed lookup is silent, and cached so the next session does not pay", {
  fresh_session()
  calls <- 0
  local_mocked_bindings(
    # Pass the redivis guard (client installed, login available) so the
    # failure comes from the dataset itself, as on a real failed lookup.
    .irw_require_redivis = function() invisible(TRUE),
    .irw_open_meta_dataset = function() {
      calls <<- calls + 1
      stop("no Source_via column in this release")
    }
  )
  expect_length(notes(.irw_source_note("bailon_2020_covidaffect")), 0)
  new_session()
  expect_length(notes(.irw_source_note("bailon_2020_covidaffect")), 0)
  expect_equal(calls, 1)
})

test_that("the lookup is kept on disk for a week, then refreshed", {
  fresh_session()
  calls <- 0
  local_mocked_bindings(.irw_source_note_query = function() {
    calls <<- calls + 1
    lookup_fixture()
  })
  expect_length(notes(.irw_source_note("bailon_2020_covidaffect")), 1)
  new_session()
  expect_length(notes(.irw_source_note("bailon_2020_covidaffect")), 1)
  expect_equal(calls, 1)

  path <- .irw_source_note_path()
  Sys.setFileTime(path, Sys.time() - .irw_source_note_ttl_seconds - 60)
  new_session()
  notes(.irw_source_note("bailon_2020_covidaffect"))
  expect_equal(calls, 2)
})

test_that("the cache file is the one the Python package writes", {
  fresh_session()
  path <- .irw_source_note_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # Exactly what src/irw/utils/redivis/source_note.py writes.
  writeLines(c("table,via,url",
               "bailon_2020_covidaffect,openESM,https://zenodo.org/records/22763396",
               "other_esm,openESM,"), path, useBytes = TRUE)
  local_mocked_bindings(.irw_source_note_query = function() stop("should read the file"))
  n <- notes(.irw_source_note("other_esm"))
  expect_length(n, 1)
  expect_no_match(n, "See ", fixed = TRUE)
})

test_that("irw_fetch() prints it for a table it returned, not for one it did not", {
  fresh_session()
  local_mocked_bindings(
    .irw_source_note_query = function() lookup_fixture(),
    fetch_single_data = function(table_id, ...) {
      if (table_id == "missing") NULL else tibble::tibble(id = 1, item = "a", resp = 1)
    }
  )
  expect_length(notes(irw_fetch("missing")), 0)
  expect_length(notes(irw_fetch(c("missing", "bailon_2020_covidaffect"))), 1)
})

## ---- irw_save_bibtex(): the intermediary's own entry (#2421) ---------------

fake_biblio <- function(with_column = TRUE) {
  b <- tibble::tibble(
    table = c("bailon_2020_covidaffect", "other_esm", "environment_ltm"),
    BibTex = c("@article{Bailon2020, title={CoVidAffect}}",
               "@article{Other2021, title={Other}}",
               "@article{Env2019, title={Env}}"),
    DOI__for_paper_ = NA_character_
  )
  if (with_column) b$Source_via <- c("openESM", "openESM", NA)
  b
}

save_quietly <- function(...) {
  f <- withr::local_tempfile(fileext = ".bib", .local_envir = parent.frame())
  out <- suppressMessages(irw_save_bibtex(..., output_file = f))
  list(entries = out, file = readLines(f, encoding = "UTF-8"))
}

test_that("irw_save_bibtex() adds the openESM entry once, after the tables", {
  local_mocked_bindings(.fetch_biblio_table = function() fake_biblio(),
                        .fetch_redivis_table = function(...) TRUE)
  res <- save_quietly(c("bailon_2020_covidaffect", "other_esm", "environment_ltm"))
  keys <- sub(",.*", "", res$entries)
  expect_equal(keys, c("@article{bailon_2020_covidaffect", "@article{other_esm",
                       "@article{environment_ltm", "@article{siepe2026openesm"))
  expect_match(res$entries[4], "B{\\\"u}chner, Anabel", fixed = TRUE)
  expect_true(any(grepl("@article{siepe2026openesm", res$file, fixed = TRUE)))
})

test_that("it says so when it adds the entry", {
  local_mocked_bindings(.fetch_biblio_table = function() fake_biblio(),
                        .fetch_redivis_table = function(...) TRUE)
  f <- withr::local_tempfile(fileext = ".bib")
  expect_message(irw_save_bibtex("bailon_2020_covidaffect", output_file = f),
                 "Also added the citation for openESM")
})

test_that("tables not found via openESM, and a biblio without the column, are unchanged", {
  local_mocked_bindings(.fetch_biblio_table = function() fake_biblio(),
                        .fetch_redivis_table = function(...) TRUE)
  expect_length(save_quietly("environment_ltm")$entries, 1)
  local_mocked_bindings(.fetch_biblio_table = function() fake_biblio(FALSE))
  expect_length(save_quietly(c("bailon_2020_covidaffect", "environment_ltm"))$entries, 2)
})

test_that("the openESM BibTeX is balanced and keyed", {
  b <- .irw_aggregators$openESM$bibtex
  expect_equal(lengths(regmatches(b, gregexpr("\\{", b))),
               lengths(regmatches(b, gregexpr("\\}", b))))
  expect_true(startsWith(b, "@article{siepe2026openesm,"))
})
