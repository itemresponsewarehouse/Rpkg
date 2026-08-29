## Collections (issue #1633). Entirely mocked -- no Redivis access, no
## credentials, no network. Same local_mocked_bindings pattern as test-filter.R.

fake_registry <- function() {
  tibble::tibble(
    collection = c("rct", "big_five", "continuous_response"),
    label      = c("RCT", "Big Five / FFM", "Continuous response"),
    kind       = c("design", "instrument", "design"),
    definition = c("Carries a treat column.",
                   "Five-factor inventories. (searched the 2,251 tagged tables of 3,650; coverage incomplete)",
                   "Curated."),
    rule       = c("var:treat", "cname:big.?five", "curated"),
    coverage   = c("metadata-complete", "tagged-subset-only", "curated-only"),
    basis      = c("rule", "rule", "curated"),
    n_tables   = c(2L, 2L, 1L),
    maintainer = "bd",
    added      = "2026-08-29"
  )
}

fake_members <- function() {
  tibble::tibble(
    table      = c("tab_rct_a", "tab_rct_b", "tab_bf_a", "tab_rct_a", "tab_cont"),
    collection = c("rct", "rct", "big_five", "big_five", "continuous_response"),
    basis      = c("rule:var:treat", "rule:var:treat", "rule:cname:big.?five",
                   "rule:cname:big.?five", "curated:bd")
  )
}

fake_metadata <- function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
  data.frame(
    table = c("tab_rct_a", "tab_rct_b", "tab_bf_a", "tab_cont", "tab_other"),
    density = 0.8,
    n_participants = c(1000, 100, 1000, 1000, 1000),
    stringsAsFactors = FALSE
  )
}

mock_all <- function() {
  local_mocked_bindings(
    irw_metadata = fake_metadata,
    .fetch_collections_table = fake_registry,
    .fetch_collection_members_table = fake_members,
    .env = asNamespace("irw")
  )
}

test_that("irw_collection returns member tables and reports coverage", {
  mock_all()
  expect_equal(suppressMessages(irw_collection("rct")), c("tab_rct_a", "tab_rct_b"))

  ## A collection that does not search the whole warehouse must say so --
  ## that warning is the entire point of the coverage field.
  expect_message(irw_collection("big_five"), "tagged-subset-only")
  expect_message(irw_collection("big_five"), "does not search the whole warehouse")

  ## ...and one that does must not cry wolf.
  msgs <- capture_messages(irw_collection("rct"))
  expect_false(any(grepl("does not search the whole warehouse", msgs)))

  expect_silent(irw_collection("rct", quiet = TRUE))
})

test_that("irw_collection rejects unknown names and suggests near matches", {
  mock_all()
  expect_error(irw_collection("bigfive"), 'Did you mean: "big_five"')
  expect_error(irw_collection("zzzz"), "No collection named")
  expect_error(irw_collection(c("rct", "big_five")), "single collection name")
})

test_that("irw_collections recomputes n_tables from live membership", {
  mock_all()
  reg <- irw_collections()
  expect_equal(nrow(reg), 3L)
  ## Registry claims 2 for big_five; membership has 2. Registry claims 2 for
  ## rct; membership has 2. The point is that the number comes from members,
  ## not from the published column, since members are live-filtered.
  expect_equal(reg$n_tables[reg$collection == "rct"], 2L)
  expect_equal(irw_collections(kind = "design")$collection,
               c("continuous_response", "rct"))
  expect_error(irw_collections(kind = "nope"), "Unknown kind")
})

test_that("irw_filter(collection=) filters, unions, and composes", {
  mock_all()
  expect_equal(irw_filter(collection = "rct", density = NULL),
               c("tab_rct_a", "tab_rct_b"))

  ## OR within the argument -- documented behaviour, worth pinning.
  expect_equal(irw_filter(collection = c("rct", "big_five"), density = NULL),
               c("tab_bf_a", "tab_rct_a", "tab_rct_b"))

  ## Composes with the numeric filters.
  expect_equal(irw_filter(collection = "rct", n_participants = c(500, Inf), density = NULL),
               "tab_rct_a")
})

test_that("an unknown collection ERRORS rather than silently returning everything", {
  ## Regression test for the specific way this ships broken: if `collection`
  ## were routed through `tag_filters`, filter.R's loop would find no such
  ## column, warn "not found in tags table. Ignored.", and return the full
  ## unfiltered set. Assert loudly that it does not.
  mock_all()
  expect_error(irw_filter(collection = "not_a_collection", density = NULL),
               "Unknown collection")
  expect_failure(expect_warning(
    tryCatch(irw_filter(collection = "not_a_collection", density = NULL),
             error = function(e) NULL),
    "Ignored"
  ))
})

test_that("collection filters are rejected for sources that have none", {
  mock_all()
  expect_error(irw_filter(collection = "rct", source = "nom"), "only available for")
  expect_error(irw_filter(collection = "rct", source = "sim"), "only available for")
  expect_error(irw_filter(collection = "rct", source = "comp"), "not available")
})

test_that("irw_collection_members answers the inverse question", {
  mock_all()
  expect_equal(irw_collection_members(tables = "tab_rct_a")$collection,
               c("big_five", "rct"))
  expect_equal(nrow(irw_collection_members(collection = "rct")), 2L)
  expect_equal(irw_collection_members(tables = "TAB_RCT_A")$collection,
               c("big_five", "rct"))   # case-insensitive, like the rest of the package
})
