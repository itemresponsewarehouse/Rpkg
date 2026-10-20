# Shard discovery (R/shard-discovery.R). The fixture is the live datapages
# catalog as captured on 2026-10-20, trimmed to the fields discovery reads,
# plus two rows for cases the live catalog does not currently contain:
# `item_response_warehouse_9` with no published version, and a published but
# non-public `irw_rank_3`.

catalog_fixture <- function() {
  irw:::.irw_parse_dataset_catalog(
    paste(readLines(test_path("fixtures", "datapages_catalog.json"), warn = FALSE),
          collapse = "\n")
  )
}

local_discovery_on <- function(env = parent.frame()) {
  withr::local_envvar(IRW_SHARD_DISCOVERY = NA, .local_envir = env)
  withr::local_options(irw.shard_discovery = TRUE, .local_envir = env)
}

spec_keys <- function(specs) {
  vapply(specs, function(s) irw:::.irw_dataset_key(s$dataset), character(1))
}

test_that("the catalog parses the reference name out of scopedReference", {
  catalog <- catalog_fixture()
  expect_gt(nrow(catalog), 20)
  expect_setequal(
    names(catalog),
    c("reference", "reference_id", "version", "public", "table_count")
  )
  # The reference name, not the display name: `name` is "Item Response
  # Warehouse 7" and does not resolve.
  row <- catalog[catalog$reference == "item_response_warehouse_7", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$reference_id, "ej0d")
  expect_equal(row$version, "v1_3")
  expect_true(row$public)
})

test_that("a dataset with no published version is not in the parsed catalog", {
  # Two-field scopedReference. Adopting it would make every open warn.
  expect_false("item_response_warehouse_9" %in% catalog_fixture()$reference)
})

test_that("a malformed or empty response yields no catalog rows", {
  expect_equal(nrow(irw:::.irw_parse_dataset_catalog("not json at all")), 0L)
  expect_equal(nrow(irw:::.irw_parse_dataset_catalog('{"results":[]}')), 0L)
  expect_equal(nrow(irw:::.irw_parse_dataset_catalog("")), 0L)
})

test_that("discovery appends a core shard the config has never heard of", {
  local_discovery_on()
  local_irw_core_specs(list(
    list(user = "datapages", dataset = "item_response_warehouse:as2e"),
    list(user = "datapages", dataset = "item_response_warehouse_2:epbx")
  ))
  local_mocked_bindings(.irw_dataset_catalog = function() catalog_fixture())

  specs <- suppressMessages(irw:::.irw_effective_specs("core"))
  expect_equal(
    spec_keys(specs),
    c("item_response_warehouse", "item_response_warehouse_2",
      "item_response_warehouse_3", "item_response_warehouse_4",
      "item_response_warehouse_5", "item_response_warehouse_6",
      "item_response_warehouse_7")
  )
  # Reference ids come from the catalog, so the appended specs resolve.
  expect_equal(specs[[7]]$dataset, "item_response_warehouse_7:ej0d")
  expect_equal(specs[[7]]$user, "datapages")
})

test_that("discovery is a no-op when the config is already current", {
  local_discovery_on()
  local_mocked_bindings(.irw_dataset_catalog = function() catalog_fixture())
  for (source in names(irw:::.irw_datasource_specs)) {
    expect_identical(
      irw:::.irw_effective_specs(source),
      irw:::.irw_datasource_specs[[source]],
      info = source
    )
  }
  expect_identical(irw:::.irw_effective_specs("text"), irw:::.irw_itemtext_specs)
})

test_that("appended shards are ordered oldest-first, not lexically", {
  local_discovery_on()
  # _10 sorts before _2 as text. The spec lists are declared oldest-first and
  # `.irw_order_datasources()` reverses them to resolve a duplicate name to its
  # newest copy, so getting this order wrong resolves to the wrong shard.
  catalog <- data.frame(
    reference = c("item_response_warehouse_10", "item_response_warehouse_2",
                  "item_response_warehouse_9"),
    reference_id = c("aa10", "bb02", "cc09"),
    version = "v1_0", public = TRUE, table_count = 1L,
    stringsAsFactors = FALSE
  )
  extra <- irw:::.irw_extra_shard_specs(
    list(list(user = "datapages", dataset = "item_response_warehouse:as2e")),
    catalog
  )
  expect_equal(
    spec_keys(extra),
    c("item_response_warehouse_2", "item_response_warehouse_9",
      "item_response_warehouse_10")
  )
})

test_that("discovery extends a family but never invents one", {
  local_discovery_on()
  catalog <- catalog_fixture()
  # Everything in the catalog that is not a continuation of a configured
  # family must be ignored -- the organization also holds wordbank, peekbank,
  # palmerpenguins and so on, and a genuinely new IRW family needs a `source`
  # value and filters, so it has to keep failing loudly instead.
  all_sources <- c(names(irw:::.irw_datasource_specs), "text")
  configured <- unlist(lapply(all_sources, function(s) {
    specs <- if (s == "text") irw:::.irw_itemtext_specs else irw:::.irw_datasource_specs[[s]]
    spec_keys(specs)
  }))
  discovered <- unlist(lapply(all_sources, function(s) {
    specs <- if (s == "text") irw:::.irw_itemtext_specs else irw:::.irw_datasource_specs[[s]]
    spec_keys(irw:::.irw_extra_shard_specs(specs, catalog))
  }))
  expect_length(discovered, 0L)
  expect_false(any(c("wordbank", "peekbank", "palmerpenguins", "childes_db", "erw") %in%
                     c(configured, discovered)))
})

test_that("a non-public shard is not adopted", {
  local_discovery_on()
  # irw_rank_3 is published in the fixture but publicAccessLevel is "metadata",
  # so opening it anonymously would fail.
  extra <- irw:::.irw_extra_shard_specs(
    list(list(user = "datapages", dataset = "irw_rank:93f5")),
    catalog_fixture()
  )
  expect_length(extra, 0L)
})

test_that("a family whose base name prefixes another is matched exactly", {
  local_discovery_on()
  catalog <- data.frame(
    reference = c("irw_text_2", "irw_text_extra", "irw_textual", "irw_text_3"),
    reference_id = c("aa02", "bb00", "cc00", "dd03"),
    version = "v1_0", public = TRUE, table_count = 1L,
    stringsAsFactors = FALSE
  )
  extra <- irw:::.irw_extra_shard_specs(
    list(list(user = "datapages", dataset = "irw_text:07b6")),
    catalog
  )
  expect_equal(spec_keys(extra), c("irw_text_2", "irw_text_3"))
})

test_that("an unreadable catalog leaves the configured list untouched", {
  local_discovery_on()
  local_mocked_bindings(.irw_dataset_catalog = function() NULL)
  expect_identical(
    irw:::.irw_effective_specs("core"),
    irw:::.irw_datasource_specs$core
  )
})

test_that("discovery can be switched off by option and by environment", {
  local_irw_core_specs(list(list(user = "datapages", dataset = "item_response_warehouse:as2e")))
  local_mocked_bindings(.irw_dataset_catalog = function() catalog_fixture())

  withr::with_options(list(irw.shard_discovery = FALSE), {
    expect_false(irw:::.irw_shard_discovery_enabled())
    expect_length(irw:::.irw_effective_specs("core"), 1L)
  })
  withr::with_envvar(c(IRW_SHARD_DISCOVERY = "0"), {
    withr::with_options(list(irw.shard_discovery = NULL), {
      expect_false(irw:::.irw_shard_discovery_enabled())
      expect_length(irw:::.irw_effective_specs("core"), 1L)
    })
  })
  # And on by default, which is what makes a released package self-correcting.
  withr::with_envvar(c(IRW_SHARD_DISCOVERY = NA), {
    withr::with_options(list(irw.shard_discovery = NULL), {
      expect_true(irw:::.irw_shard_discovery_enabled())
    })
  })
})

test_that("a discovered shard is said once, not on every call", {
  local_discovery_on()
  local_irw_pristine(".irw_env")
  local_irw_core_specs(list(list(user = "datapages", dataset = "item_response_warehouse:as2e")))
  local_mocked_bindings(.irw_dataset_catalog = function() catalog_fixture())

  expect_message(irw:::.irw_effective_specs("core"), "newer than this version of irw")
  expect_silent(irw:::.irw_effective_specs("core"))
})

test_that("a discovered shard is pinnable like any configured one", {
  local_discovery_on()
  local_irw_pristine(".irw_env")
  local_irw_core_specs(list(list(user = "datapages", dataset = "item_response_warehouse:as2e")))
  local_mocked_bindings(.irw_dataset_catalog = function() catalog_fixture())

  keys <- names(suppressMessages(irw:::.irw_pinnable_specs()))
  # Without this, `irw_use_version()` would honour the pin for every
  # configured shard and read a discovered one at its current release.
  expect_true("item_response_warehouse_7" %in% keys)
  expect_true("irw_meta" %in% keys)
})

test_that("the catalog is fetched once per session, even when it fails", {
  local_irw_pristine(".irw_env")
  calls <- 0
  local_mocked_bindings(
    .irw_parse_dataset_catalog = function(text) {
      calls <<- calls + 1
      irw:::.irw_parse_dataset_catalog(text)
    }
  )
  local_mocked_bindings(
    GET = function(...) {
      calls <<- calls + 1
      stop("no network")
    },
    .package = "httr"
  )
  expect_null(irw:::.irw_dataset_catalog())
  expect_null(irw:::.irw_dataset_catalog())
  expect_equal(calls, 1)
})
