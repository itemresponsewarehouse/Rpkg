library(testthat)

# Replace bindings inside the irw namespace for the duration of the calling
# frame. testthat's local_mocked_bindings() does not reliably restore namespace
# bindings here, and a leaked mock would silently corrupt later test files.
local_ns_bindings <- function(..., .env = parent.frame()) {
  ns <- asNamespace("irw")
  mocks <- list(...)
  originals <- mget(names(mocks), envir = ns)

  set_bindings <- function(values) {
    for (nm in names(values)) {
      locked <- bindingIsLocked(nm, ns)
      if (locked) unlockBinding(nm, ns)
      assign(nm, values[[nm]], envir = ns)
      if (locked) lockBinding(nm, ns)
    }
  }

  set_bindings(mocks)
  withr::defer(set_bindings(originals), envir = .env)
  invisible(NULL)
}

# Small biblio fixture: one shared DOI, one shared BibTex entry, and several
# rows whose citation is missing in the ways the live biblio table records it
# ("NA", empty string, real NA).
make_biblio <- function() {
  data.frame(
    table = c(
      "doi_a", "doi_b", "doi_c",
      "bib_a", "bib_b",
      "none_a", "none_b", "none_c", "none_d"
    ),
    DOI__for_paper_ = c(
      "10.1/shared", "10.1/shared", "10.1/shared",
      "NA", "NA",
      "NA", "", NA, "NA"
    ),
    BibTex = c(
      "@article{one}", "@article{one}", "@article{one}",
      "@article{two}", "@article{two}",
      "NA", "", NA, "  "
    ),
    stringsAsFactors = FALSE
  )
}

test_that(".is_present_biblio_value rejects the biblio missing-value sentinels", {
  expect_equal(
    irw:::.is_present_biblio_value(c("10.1/x", "NA", "", "  ", NA, "@article{k}")),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("generate_doi_bibtex_mapping ignores rows with no citation", {
  maps <- irw:::generate_doi_bibtex_mapping(make_biblio())

  expect_equal(names(maps$doi_map), "10.1/shared")
  expect_equal(maps$doi_map[["10.1/shared"]], c("doi_a", "doi_b", "doi_c"))

  # "NA"/""/NA must never become a grouping key: tables that merely share a
  # *missing* citation are not related.
  expect_equal(sort(names(maps$bibtex_map)), c("@article{one}", "@article{two}"))
  expect_false(any(c("NA", "", "  ") %in% names(maps$bibtex_map)))
  expect_false(any(startsWith(unlist(maps$bibtex_map), "none_")))
})

test_that("generate_doi_bibtex_mapping drops singleton groups", {
  bib <- data.frame(
    table = c("solo", "pair_a", "pair_b"),
    DOI__for_paper_ = c("10.1/solo", "10.1/pair", "10.1/pair"),
    BibTex = c("@a{1}", "@a{2}", "@a{2}"),
    stringsAsFactors = FALSE
  )
  maps <- irw:::generate_doi_bibtex_mapping(bib)

  expect_equal(names(maps$doi_map), "10.1/pair")
  expect_equal(names(maps$bibtex_map), "@a{2}")
})

test_that("find_merge_candidates returns the query table first", {
  maps <- irw:::generate_doi_bibtex_mapping(make_biblio())

  expect_equal(irw:::find_merge_candidates("doi_b", maps), c("doi_b", "doi_a", "doi_c"))
  expect_equal(irw:::find_merge_candidates("bib_b", maps), c("bib_b", "bib_a"))
})

test_that("find_merge_candidates returns NULL when there is nothing to merge", {
  maps <- irw:::generate_doi_bibtex_mapping(make_biblio())

  for (tbl in c("none_a", "none_b", "none_c", "none_d")) {
    expect_null(irw:::find_merge_candidates(tbl, maps))
  }
  expect_null(irw:::find_merge_candidates("not_in_biblio", maps))
})

test_that("find_merge_candidates prefers the DOI group over the BibTex group", {
  bib <- data.frame(
    table = c("x", "y", "z"),
    DOI__for_paper_ = c("10.1/d", "10.1/d", "NA"),
    BibTex = c("@a{k}", "NA", "@a{k}"),
    stringsAsFactors = FALSE
  )
  maps <- irw:::generate_doi_bibtex_mapping(bib)

  expect_equal(irw:::find_merge_candidates("x", maps), c("x", "y"))
})

test_that("check_ids_and_items passes when IDs match and items are disjoint", {
  tables <- list(
    t1 = data.frame(id = c(5, 7), item = c("i1", "i2"), stringsAsFactors = FALSE),
    t2 = data.frame(id = c(5, 7), item = c("i3", "i4"), stringsAsFactors = FALSE)
  )
  expect_true(suppressMessages(irw:::check_ids_and_items(tables)))
})

test_that("check_ids_and_items flags mismatched IDs and overlapping items", {
  mismatched <- list(
    t1 = data.frame(id = c(5, 7), item = c("i1", "i2"), stringsAsFactors = FALSE),
    t2 = data.frame(id = c(5, 9), item = c("i3", "i4"), stringsAsFactors = FALSE)
  )
  expect_false(suppressMessages(irw:::check_ids_and_items(mismatched)))

  overlapping <- list(
    t1 = data.frame(id = c(5, 7), item = c("i1", "i2"), stringsAsFactors = FALSE),
    t2 = data.frame(id = c(5, 7), item = c("i2", "i3"), stringsAsFactors = FALSE)
  )
  expect_message(
    expect_false(irw:::check_ids_and_items(overlapping)),
    "items that overlap"
  )
})

test_that("check_ids_and_items handles a single table without erroring", {
  one <- list(t1 = data.frame(id = 1:3, item = c("i1", "i2", "i3"), stringsAsFactors = FALSE))
  expect_no_error(suppressMessages(irw:::check_ids_and_items(one)))
})

test_that(".prompt_yes_no returns the default without looping when non-interactive", {
  skip_if(interactive(), "Requires a non-interactive session")

  expect_message(irw:::.prompt_yes_no("go? ", default = TRUE), "not interactive")
  expect_true(suppressMessages(irw:::.prompt_yes_no("go? ", default = TRUE)))
  expect_false(suppressMessages(irw:::.prompt_yes_no("go? ", default = FALSE)))
})

# --- irw_merge() end to end, with Redivis access mocked out -------------------

# Two-column-compatible tables plus one with an extra column, so the rbind
# failure path is exercised alongside the happy path.
merge_fixture_tables <- list(
  m1 = data.frame(id = c(1, 2), item = c("a1", "a2"), resp = c(1, 0), stringsAsFactors = FALSE),
  m2 = data.frame(id = c(1, 2), item = c("b1", "b2"), resp = c(0, 1), stringsAsFactors = FALSE),
  m3 = data.frame(id = c(1, 2), item = c("c1", "c2"), resp = c(1, 1), wave = c(1, 2), stringsAsFactors = FALSE)
)

mock_merge_env <- function(tables = merge_fixture_tables, n_participants = c(m1 = 2, m2 = 2, m3 = 2)) {
  bib <- data.frame(
    table = names(tables),
    DOI__for_paper_ = rep("10.1/shared", length(tables)),
    BibTex = rep("@article{shared}", length(tables)),
    stringsAsFactors = FALSE
  )

  list(
    .fetch_biblio_table = function() bib,
    irw_metadata = function(...) {
      data.frame(
        table = names(n_participants),
        n_participants = unname(n_participants),
        stringsAsFactors = FALSE
      )
    },
    irw_fetch = function(name, ...) tables[[name]]
  )
}

test_that("irw_merge stacks compatible tables and skips mismatched ones", {
  mocks <- mock_merge_env()
  local_ns_bindings(
    .fetch_biblio_table = mocks$.fetch_biblio_table,
    irw_metadata = mocks$irw_metadata,
    irw_fetch = mocks$irw_fetch
  )

  out <- suppressMessages(irw_merge("m1"))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4L)
  expect_equal(sort(unique(out$source_table)), c("m1", "m2"))
  expect_false("m3" %in% out$source_table) # extra `wave` column -> rbind fails
})

test_that("irw_merge omits source_table when add_source_column = FALSE", {
  mocks <- mock_merge_env()
  local_ns_bindings(
    .fetch_biblio_table = mocks$.fetch_biblio_table,
    irw_metadata = mocks$irw_metadata,
    irw_fetch = mocks$irw_fetch
  )

  out <- suppressMessages(irw_merge("m1", add_source_column = FALSE))

  expect_false("source_table" %in% names(out))
  expect_equal(nrow(out), 4L)
})

test_that("irw_merge skips tables that cannot be fetched", {
  # m2 is listed in the bibliography but absent from every datasource.
  mocks <- mock_merge_env()
  local_ns_bindings(
    .fetch_biblio_table = mocks$.fetch_biblio_table,
    irw_metadata = mocks$irw_metadata,
    irw_fetch = function(name, ...) if (identical(name, "m2")) NULL else merge_fixture_tables[[name]]
  )

  # m1 merges, m2 returns NULL, m3 mismatches -> only one usable table left.
  expect_message(
    expect_null(irw_merge("m1", add_source_column = FALSE)),
    "Merging failed for all tables"
  )
})

test_that("irw_merge returns NULL when the table has no merge candidates", {
  local_ns_bindings(
    .fetch_biblio_table = function() make_biblio()
  )

  expect_message(expect_null(irw_merge("none_a")), "No mergeable tables found")
})

test_that("irw_merge returns NULL when the user declines", {
  mocks <- mock_merge_env()
  local_ns_bindings(
    .fetch_biblio_table = mocks$.fetch_biblio_table,
    irw_metadata = mocks$irw_metadata,
    irw_fetch = mocks$irw_fetch,
    .prompt_yes_no = function(...) FALSE
  )

  expect_message(expect_null(irw_merge("m1")), "Merge operation canceled")
})

test_that("irw_merge reports inconsistent respondent counts", {
  mocks <- mock_merge_env(n_participants = c(m1 = 2, m2 = 99, m3 = 2))
  local_ns_bindings(
    .fetch_biblio_table = mocks$.fetch_biblio_table,
    irw_metadata = mocks$irw_metadata,
    irw_fetch = mocks$irw_fetch
  )

  out <- NULL
  expect_message(out <- irw_merge("m1"), "not consistent across tables")
  expect_equal(nrow(out), 4L) # warning is advisory; the merge still proceeds
})
