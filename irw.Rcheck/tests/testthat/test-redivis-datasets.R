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

test_that(".irw_redivis_error_type classifies common errors", {
  expect_equal(
    irw:::.irw_redivis_error_type("invalid_request_error: bad table name"),
    "invalid"
  )
  expect_equal(
    irw:::.irw_redivis_error_type("Not found: datapages.item_response_warehouse_3:5xaj:v1_0.foo"),
    "not_found"
  )
  expect_equal(
    irw:::.irw_redivis_error_type("Error: User is not authenticated"),
    "auth"
  )
  expect_equal(
    irw:::.irw_redivis_error_type("temporary upstream failure"),
    "other"
  )
})

test_that(".irw_sanitize_redivis_error strips internal dataset paths", {
  msg <- "Not found: datapages.item_response_warehouse_3:5xaj:v43_0.some_table"
  clean <- irw:::.irw_sanitize_redivis_error(msg)
  expect_false(grepl("item_response_warehouse_3", clean, fixed = TRUE))
  expect_false(grepl("5xaj", clean, fixed = TRUE))
})

test_that(".irw_order_datasources reverses core warehouses only", {
  ds <- list("wh1", "wh2", "wh3")
  expect_equal(irw:::.irw_order_datasources(ds, "core"), list("wh3", "wh2", "wh1"))
  expect_equal(irw:::.irw_order_datasources(ds, "sim"), ds)
})

test_that(".irw_dedup_table_info keeps the first row per table name", {
  info <- data.frame(
    name = c("alpha", "beta", "alpha"),
    numRows = c(1, 2, 99),
    variableCount = c(3, 3, 3),
    stringsAsFactors = FALSE
  )
  out <- irw:::.irw_dedup_table_info(info)
  expect_equal(out$name, c("alpha", "beta"))
  expect_equal(out$numRows, c(1, 2))
})

test_that(".irw_core_warehouse_fingerprint changes when specs change", {
  fp <- irw:::.irw_core_warehouse_fingerprint()
  expect_type(fp, "character")
  expect_true(nzchar(fp))

  local_mocked_bindings(
    .irw_datasource_specs = list(
      core = list(
        list(user = "datapages", dataset = "item_response_warehouse:as2e"),
        list(user = "datapages", dataset = "item_response_warehouse_99:zzzz")
      ),
      sim = irw:::.irw_datasource_specs$sim,
      comp = irw:::.irw_datasource_specs$comp,
      nom = irw:::.irw_datasource_specs$nom
    ),
    .env = asNamespace("irw")
  )

  expect_false(identical(fp, irw:::.irw_core_warehouse_fingerprint()))
})

test_that(".irw_sync_core_warehouse_caches clears live-table caches when specs change", {
  local({
    env <- new.env(parent = emptyenv())
    env$datasource_list <- list("cached")
    env$metadata_tibble <- data.frame(table = "x")
    env$core_warehouse_fingerprint <- "old"

    local_mocked_bindings(
      .irw_env = env,
      .irw_core_warehouse_fingerprint = function() "new",
      .env = asNamespace("irw")
    )

    irw:::.irw_sync_core_warehouse_caches()
    expect_false(exists("datasource_list", envir = env))
    expect_false(exists("metadata_tibble", envir = env))
    expect_equal(env$core_warehouse_fingerprint, "new")
  })
})

test_that(".irw_table_not_found_message does not mention warehouse ids", {
  msg <- irw:::.irw_table_not_found_message("frac20")
  expect_match(msg, "does not exist in IRW")
  expect_false(grepl("warehouse", msg, ignore.case = TRUE))
})
