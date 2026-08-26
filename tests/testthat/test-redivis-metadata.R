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
