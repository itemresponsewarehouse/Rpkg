library(testthat)

test_that(".irw_normalize_version accepts released tags and adds the 'v'", {
  expect_identical(irw:::.irw_normalize_version("v32.0"), "v32.0")
  expect_identical(irw:::.irw_normalize_version("32.0"), "v32.0")
  expect_identical(irw:::.irw_normalize_version(" 1.8 "), "v1.8")
})

test_that(".irw_normalize_version rejects anything Redivis would silently resolve", {
  # Redivis returns the current release for an unrecognized version string, so
  # these must be caught before the request is made.
  expect_null(irw:::.irw_normalize_version("banana"))
  expect_null(irw:::.irw_normalize_version("next"))
  expect_null(irw:::.irw_normalize_version("v32"))
  expect_null(irw:::.irw_normalize_version(32.0))
  expect_null(irw:::.irw_normalize_version(c("v1.0", "v2.0")))
  expect_null(irw:::.irw_normalize_version(NA_character_))
})

test_that(".irw_dataset_key strips the Redivis hash", {
  expect_identical(irw:::.irw_dataset_key("item_response_warehouse:as2e"), "item_response_warehouse")
  expect_identical(irw:::.irw_dataset_key("irw_meta:bdxt"), "irw_meta")
})

test_that("every configured datasource is pinnable, including metadata and item text", {
  local_irw_pristine(".irw_datasource_specs")
  keys <- names(irw:::.irw_pinnable_specs())
  expect_true(all(c(
    "item_response_warehouse", "item_response_warehouse_2",
    "irw_simsyn", "irw_competitions", "irw_nominal",
    "irw_meta", "irw_text"
  ) %in% keys))
  expect_false(anyDuplicated(keys) > 0)
})

test_that("irw_set_version rejects an unknown dataset before any network call", {
  local_no_pins()
  expect_error(irw_set_version("not_a_warehouse", "v1.0"), "Unknown IRW dataset")
  expect_length(irw:::.irw_pins(), 0)
})

test_that("irw_set_version rejects a malformed version tag", {
  local_no_pins()
  expect_error(irw_set_version("item_response_warehouse", "banana"), "released version tag")
  expect_length(irw:::.irw_pins(), 0)
})

test_that("irw_set_version stores the pin and clears cached datasources", {
  local_no_pins()
  local_irw_binding(".irw_open_dataset_at_version", function(spec, version) {
    list(properties = list(version = list(tag = version)))
  })
  e <- irw:::.irw_env
  e$datasource_list <- "stale"

  expect_message(irw_set_version("item_response_warehouse", "40.0"), "v40.0")

  expect_identical(irw:::.irw_pinned_version("item_response_warehouse"), "v40.0")
  expect_false(exists("datasource_list", envir = irw:::.irw_env))
})

test_that("irw_set_version fails when Redivis resolves a different version", {
  local_no_pins()
  local_irw_binding(".irw_open_dataset_at_version", function(spec, version) {
    list(properties = list(version = list(tag = "v45.1")))
  })

  expect_error(
    suppressMessages(irw_set_version("item_response_warehouse", "v40.0")),
    "could not be resolved"
  )
  expect_length(irw:::.irw_pins(), 0)
})

test_that("irw_reset_version unpins one dataset or all of them", {
  local_no_pins()
  e <- irw:::.irw_env
  e$pinned_versions <- c(
    item_response_warehouse = "v40.0",
    irw_simsyn = "v1.0"
  )

  expect_message(irw_reset_version("item_response_warehouse"), "Unpinned item_response_warehouse")
  expect_identical(names(irw:::.irw_pins()), "irw_simsyn")

  expect_message(irw_reset_version(), "Unpinned irw_simsyn")
  expect_length(irw:::.irw_pins(), 0)

  expect_message(irw_reset_version(), "No IRW version pins were set")
})

test_that(".irw_clear_all_datasource_caches keeps pins and drops caches", {
  local_no_pins()
  e <- irw:::.irw_env
  e$pinned_versions <- c(item_response_warehouse = "v40.0")
  e$metadata_tibble <- "stale"
  e$sim_datasource <- "stale"

  irw:::.irw_clear_all_datasource_caches()

  expect_identical(irw:::.irw_pins(), c(item_response_warehouse = "v40.0"))
  expect_false(exists("metadata_tibble", envir = irw:::.irw_env))
  expect_false(exists("sim_datasource", envir = irw:::.irw_env))
})

test_that("a missing table reports the pinned version rather than 'not in IRW'", {
  local_no_pins()
  e <- irw:::.irw_env
  e$pinned_versions <- c(item_response_warehouse = "v40.0")

  msg <- irw:::.irw_pinned_not_found_message("enem_2023_1mil_ch", "core")
  expect_match(msg, "does not exist in the pinned version")
  expect_match(msg, "item_response_warehouse v40.0", fixed = TRUE)

  # A pin on a core warehouse says nothing about the simulation datasource.
  expect_null(irw:::.irw_pinned_not_found_message("anything", "sim"))
})

test_that("no pin means the ordinary not-found message is used", {
  local_no_pins()
  expect_null(irw:::.irw_pinned_not_found_message("anything", "core"))
})

test_that(".irw_open_dataset passes a pinned version through to Redivis", {
  local_no_pins()
  e <- irw:::.irw_env
  e$pinned_versions <- c(irw_simsyn = "v1.0")

  local_irw_pristine(".irw_open_dataset")

  seen <- list()
  local_irw_binding(".irw_redivis_dataset", function(spec, version = NULL) {
    seen[[spec$dataset]] <<- version
    list(get = function() invisible(NULL))
  })

  irw:::.irw_open_dataset(list(user = "datapages", dataset = "irw_simsyn:0btg"))
  irw:::.irw_open_dataset(list(user = "datapages", dataset = "item_response_warehouse:as2e"))

  expect_identical(seen[["irw_simsyn:0btg"]], "v1.0")
  expect_false("item_response_warehouse:as2e" %in% names(seen))
})
