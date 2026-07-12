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
