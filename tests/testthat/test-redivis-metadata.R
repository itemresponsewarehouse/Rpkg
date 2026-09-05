library(testthat)

test_that(".irw_filter_rows_to_live_tables keeps only listed tables", {
  local_mocked_bindings(
    .irw_live_table_names = function(source = "core") {
      expect_identical(source, "core")
      c("keep_me", "also_keep")
    },
    .env = asNamespace("irw")
  )

  meta <- data.frame(
    table = c("keep_me", "ghost", "also_keep"),
    n_items = c(10L, 5L, 20L),
    stringsAsFactors = FALSE
  )

  out <- irw:::.irw_filter_rows_to_live_tables(meta, source = "core")
  expect_equal(out$table, c("keep_me", "also_keep"))
})

test_that(".irw_is_not_found_error recognizes BigQuery not-found messages", {
  msg <- "Not found: datapages.item_response_warehouse:as2e:v43_0.eammi_grahe_2018_american_dream"
  expect_true(irw:::.irw_is_not_found_error(msg))
})

test_that(".resolve_itemtext_table_name prefers an exact match", {
  available <- c("gilbert_meta_49", "heard_roch_2022_k6")
  expect_identical(
    irw:::.resolve_itemtext_table_name("gilbert_meta_49", available),
    "gilbert_meta_49"
  )
})

test_that(".resolve_itemtext_table_name falls back to a case-insensitive match", {
  # Item text table names are lower-cased on upload to Redivis, while many IRW
  # response tables have mixed case; without this fallback their item text is
  # unreachable under the response table's own name.
  available <- c("heard_roch_2022_k6", "alsecypiamh_wu_2022_cps")

  expect_identical(
    irw:::.resolve_itemtext_table_name("HEARD_Roch_2022_K6", available),
    "heard_roch_2022_k6"
  )
  expect_identical(
    irw:::.resolve_itemtext_table_name("ALSECYPIAMH_WU_2022_CPS", available),
    "alsecypiamh_wu_2022_cps"
  )
})

test_that(".resolve_itemtext_table_name returns NULL when nothing matches", {
  expect_null(
    irw:::.resolve_itemtext_table_name("no_such_table", c("gilbert_meta_49"))
  )
})

test_that(".resolve_itemtext_table_name refuses an ambiguous case-insensitive match", {
  available <- c("Some_Table", "some_table")
  expect_message(
    result <- irw:::.resolve_itemtext_table_name("SOME_TABLE", available),
    "Multiple item text tables match"
  )
  expect_null(result)
  # An exact match must still win even when a case-insensitive rival exists.
  expect_identical(
    irw:::.resolve_itemtext_table_name("some_table", available),
    "some_table"
  )
})

test_that(".resolve_itemtext_table_name rejects unusable input without matching", {
  # Subsetting by an NA comparison would otherwise return a vector of NAs and
  # produce a nonsense "multiple matches" message.
  available <- c("gilbert_meta_49", "heard_roch_2022_k6")
  expect_null(irw:::.resolve_itemtext_table_name(NA_character_, available))
  expect_null(irw:::.resolve_itemtext_table_name("", available))
  expect_null(irw:::.resolve_itemtext_table_name(character(0), available))
})

# ---------------------------------------------------------------------------
# Item text sharding. Redivis caps a dataset at 1000 tables, so `irw_text` will
# become `irw_text`, `irw_text_2`, ... These tests drive the two-shard path
# before a second shard exists, so the cutover is a config edit rather than a
# code change made under pressure.
# ---------------------------------------------------------------------------

# A stand-in for a Redivis dataset holding the named item text tables.
fake_text_shard <- function(id, bases) {
  tables <- lapply(paste0(bases, "__items"),
                   function(nm) list(properties = list(name = nm)))
  list(
    id = id,
    list_tables = function() tables,
    table = function(nm) list(shard = id, name = nm)
  )
}

# The session env is shared across test files, and the item text fingerprint is
# deliberately config-only -- two tests using the same shard *names* with
# different contents look identical to it, which is correct in production and
# wrong in a test. So clear the caches explicitly.
clear_itemtext_caches <- function() {
  e <- irw:::.irw_env
  for (nm in c("itemtext_fingerprint", "itemtext_datasource_list",
               "itemtext_table_names", "itemtext_table_index")) {
    if (exists(nm, envir = e)) rm(list = nm, envir = e)
  }
}

local_text_shards <- function(shards, env = parent.frame()) {
  local_irw_pristine(".irw_env", env = env)
  clear_itemtext_caches()
  withr::defer(clear_itemtext_caches(), envir = env)
  # Two specs so the ordering helper actually has something to reverse.
  local_irw_binding(
    ".irw_itemtext_specs",
    lapply(names(shards), function(id) list(user = "u", dataset = id)),
    env = env
  )
  local_mocked_bindings(
    .irw_open_dataset = function(spec) shards[[spec$dataset]],
    .env = asNamespace("irw")
  )
}

test_that("item text lists the union across shards, deduplicated", {
  local_text_shards(list(
    "irw_text:1"   = fake_text_shard("old", c("alpha", "shared")),
    "irw_text_2:2" = fake_text_shard("new", c("shared", "beta"))
  ))
  expect_equal(irw:::.irw_itemtext_table_index() |> names() |> sort(),
               c("alpha", "beta", "shared"))
  expect_equal(irw_list_itemtext_tables(), c("alpha", "beta", "shared"))
})

test_that("a table in two shards routes to the newest", {
  # This is the whole point. Clients resolve newest-first, so the copy in
  # irw_text_2 is the live one; serving the older copy would be silently wrong.
  local_text_shards(list(
    "irw_text:1"   = fake_text_shard("old", c("shared")),
    "irw_text_2:2" = fake_text_shard("new", c("shared"))
  ))
  expect_equal(irw:::.irw_itemtext_table_index()[["shared"]]$id, "new")
})

test_that("a table only in the older shard still resolves there", {
  local_text_shards(list(
    "irw_text:1"   = fake_text_shard("old", c("only_old")),
    "irw_text_2:2" = fake_text_shard("new", c("something_else"))
  ))
  expect_equal(irw:::.irw_itemtext_table_index()[["only_old"]]$id, "old")
})

test_that("item text shards are stored newest-first", {
  local_text_shards(list(
    "irw_text:1"   = fake_text_shard("old", c("a")),
    "irw_text_2:2" = fake_text_shard("new", c("b"))
  ))
  ids <- vapply(irw:::.irw_itemtext_datasources(), function(d) d$id, character(1))
  expect_equal(ids, c("new", "old"))
})

test_that("an unavailable shard does not hide the other one's tables", {
  local_irw_pristine(".irw_env")
  clear_itemtext_caches()
  withr::defer(clear_itemtext_caches())
  local_irw_binding(".irw_itemtext_specs", list(
    list(user = "u", dataset = "irw_text:1"),
    list(user = "u", dataset = "irw_text_2:2")
  ))
  local_mocked_bindings(
    .irw_open_dataset = function(spec) {
      if (grepl("_2:", spec$dataset)) stop("[403 insufficient_scope] no release")
      fake_text_shard("old", c("alpha"))
    },
    .env = asNamespace("irw")
  )
  expect_warning(nms <- irw_list_itemtext_tables(), "unavailable")
  expect_equal(nms, "alpha")
})

test_that("changing the shard list invalidates the cached union", {
  # A package upgrade or devtools::load_all() can add a shard mid-session.
  # Without the fingerprint the old union stays cached and the new shard is
  # invisible until restart.
  local_irw_pristine(".irw_env")
  clear_itemtext_caches()
  withr::defer(clear_itemtext_caches())
  one <- fake_text_shard("old", c("alpha"))
  two <- fake_text_shard("new", c("beta"))
  local_mocked_bindings(
    .irw_open_dataset = function(spec) if (grepl("_2:", spec$dataset)) two else one,
    .env = asNamespace("irw")
  )
  local_irw_binding(".irw_itemtext_specs", list(list(user = "u", dataset = "irw_text:1")))
  expect_equal(irw_list_itemtext_tables(), "alpha")

  local_irw_binding(".irw_itemtext_specs", list(
    list(user = "u", dataset = "irw_text:1"),
    list(user = "u", dataset = "irw_text_2:2")
  ))
  expect_equal(irw_list_itemtext_tables(), c("alpha", "beta"))
})
