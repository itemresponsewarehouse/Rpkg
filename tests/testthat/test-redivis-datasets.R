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
  # The message Redivis actually returns for a bad/expired token.
  expect_equal(
    irw:::.irw_redivis_error_type("[401 invalid_token] Must be logged in"),
    "auth"
  )
  # A warehouse with no released version yet: skippable, not an auth failure.
  expect_equal(
    irw:::.irw_redivis_error_type(
      paste("[403 insufficient_scope] You have access to the underlying resource,",
            "but your current credentials are missing the required scope(s): data.edit")
    ),
    "other"
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

  local_irw_core_specs(list(
        list(user = "datapages", dataset = "item_response_warehouse:as2e"),
        list(user = "datapages", dataset = "item_response_warehouse_99:zzzz")
      ))

  expect_false(identical(fp, irw:::.irw_core_warehouse_fingerprint()))
})

test_that(".irw_sync_core_warehouse_caches clears live-table caches when specs change", {
  local({
    env <- new.env(parent = emptyenv())
    env$datasource_list <- list("cached")
    env$metadata_tibble <- data.frame(table = "x")
    env$core_warehouse_fingerprint <- "old"

    local_irw_binding(".irw_env", env)
    local_mocked_bindings(
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

test_that(".irw_redivis_error_type classifies export quota and rate limits", {
  quota <- paste(
    "[400 invalid_request] Cannot export more than 200GB within a 30 day period.",
    "You have exported 204GB in the past 30 days"
  )
  expect_equal(irw:::.irw_redivis_error_type(quota), "quota")
  expect_equal(irw:::.irw_redivis_error_type("429 Too Many Requests"), "quota")
  expect_equal(irw:::.irw_redivis_error_type("RESOURCE_EXHAUSTED: rate limit exceeded"), "quota")
})

test_that(".irw_handle_datasource_error stops on quota errors instead of falling through", {
  quota <- "[400 invalid_request] Cannot export more than 200GB within a 30 day period."
  expect_error(
    irw:::.irw_handle_datasource_error(quota, "some_table", ds_list = list("a", "b", "c", "d")),
    "export quota"
  )
})

test_that(".irw_handle_datasource_error collects unclassified errors across datasources", {
  errors <- irw:::.irw_new_error_collector()
  expect_null(irw:::.irw_handle_datasource_error(
    "temporary upstream failure", "some_table",
    ds_list = list("a", "b"), errors = errors
  ))
  expect_null(irw:::.irw_handle_datasource_error(
    "Not found: some_table", "some_table",
    ds_list = list("a", "b"), errors = errors
  ))
  expect_length(errors$msgs, 1L)
  expect_match(irw:::.irw_collected_error_message(errors), "temporary upstream failure")
})

test_that(".irw_collected_error_message is NULL when every datasource said not-found", {
  errors <- irw:::.irw_new_error_collector()
  irw:::.irw_handle_datasource_error(
    "Not found: some_table", "some_table",
    ds_list = list("a", "b"), errors = errors
  )
  expect_null(irw:::.irw_collected_error_message(errors))
})

test_that("fetch_single_data reports the real error rather than a missing table", {
  fake_ds <- function(msg) {
    list(
      get = function() invisible(NULL),
      table = function(name) list(get = function() stop(msg, call. = FALSE))
    )
  }
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) {
      list(fake_ds("temporary upstream failure"), fake_ds("temporary upstream failure"))
    },
    .env = asNamespace("irw")
  )

  expect_message(out <- irw:::fetch_single_data("some_table"), "temporary upstream failure")
  expect_null(out)
})

test_that("fetch_single_data still reports a genuinely missing table", {
  fake_ds <- function(msg) {
    list(
      get = function() invisible(NULL),
      table = function(name) list(get = function() stop(msg, call. = FALSE))
    )
  }
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) {
      list(fake_ds("Not found: some_table"), fake_ds("Not found: some_table"))
    },
    .env = asNamespace("irw")
  )

  expect_message(out <- irw:::fetch_single_data("some_table"), "does not exist in IRW")
  expect_null(out)
})

test_that(".irw_coerce_resp_set keeps nominal responses as character", {
  expect_identical(irw:::.irw_coerce_resp_set(c("2", "1"), source = "core"), c(1, 2))
  expect_identical(irw:::.irw_coerce_resp_set(c("b", "a"), source = "core"), c("a", "b"))
  expect_identical(irw:::.irw_coerce_resp_set(c("2", "1"), source = "nom"), c("2", "1"))
})

# A warehouse that exists but has no released version yet errors for read-only
# tokens; it must not take down every IRW lookup.

test_that(".irw_open_core_datasources skips an unreleased warehouse with a warning", {
  local_irw_core_specs(list(
        list(user = "datapages", dataset = "item_response_warehouse:as2e"),
        list(user = "datapages", dataset = "item_response_warehouse_5:3ykx")
      ))
  local_mocked_bindings(
    .irw_open_dataset = function(spec) {
      if (grepl("_5", spec$dataset, fixed = TRUE)) {
        stop("[403 insufficient_scope] missing the required scope(s): data.edit")
      }
      spec$dataset
    },
    .env = asNamespace("irw")
  )

  expect_warning(out <- irw:::.irw_open_core_datasources(), "unavailable IRW datasource")
  expect_equal(out, list("item_response_warehouse:as2e"))
})

test_that(".irw_open_core_datasources warning does not leak warehouse ids", {
  local_irw_core_specs(list(
        list(user = "datapages", dataset = "item_response_warehouse:as2e"),
        list(user = "datapages", dataset = "item_response_warehouse_5:3ykx")
      ))
  local_mocked_bindings(
    .irw_open_dataset = function(spec) {
      if (grepl("_5", spec$dataset, fixed = TRUE)) {
        stop("Boom on item_response_warehouse_5:3ykx")
      }
      spec$dataset
    },
    .env = asNamespace("irw")
  )

  w <- tryCatch(irw:::.irw_open_core_datasources(), warning = function(e) conditionMessage(e))
  expect_false(grepl("3ykx", w, fixed = TRUE))
})

test_that(".irw_open_core_datasources errors when every warehouse is unavailable", {
  local_irw_core_specs(list(
        list(user = "datapages", dataset = "item_response_warehouse:as2e"),
        list(user = "datapages", dataset = "item_response_warehouse_5:3ykx")
      ))
  local_mocked_bindings(
    .irw_open_dataset = function(spec) stop("[500] upstream is down"),
    .env = asNamespace("irw")
  )

  expect_error(irw:::.irw_open_core_datasources(), "An error occurred while accessing IRW")
})

test_that(".irw_open_core_datasources stops immediately on an auth failure", {
  local_irw_core_specs(list(
        list(user = "datapages", dataset = "item_response_warehouse:as2e"),
        list(user = "datapages", dataset = "item_response_warehouse_5:3ykx")
      ))
  local_mocked_bindings(
    .irw_open_dataset = function(spec) stop("unauthorized: bad token"),
    .env = asNamespace("irw")
  )

  expect_error(irw:::.irw_open_core_datasources(), "Redivis authentication failed")
})

test_that(".irw_open_core_datasources returns config order when all are available", {
  local_irw_core_specs(list(
        list(user = "datapages", dataset = "wh_a"),
        list(user = "datapages", dataset = "wh_b"),
        list(user = "datapages", dataset = "wh_c")
      ))
  local_mocked_bindings(
    .irw_open_dataset = function(spec) spec$dataset,
    .env = asNamespace("irw")
  )

  expect_silent(out <- irw:::.irw_open_core_datasources())
  expect_equal(out, list("wh_a", "wh_b", "wh_c"))
  # .irw_order_datasources is what flips this to newest-first at point of use.
  expect_equal(irw:::.irw_order_datasources(out, "core"), list("wh_c", "wh_b", "wh_a"))
})
