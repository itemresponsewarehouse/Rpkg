library(testthat)

test_that("irw_version() with no argument reports the newest version", {
  local_manifest()
  out <- suppressMessages(irw_version())
  expect_identical(attr(out, "irw_version"), 3L)
  expect_setequal(out$dataset, c("item_response_warehouse", "irw_meta"))
  expect_identical(out$version[out$dataset == "irw_meta"], "v2.0")
})

test_that("irw_version(date) reports the version live on that date", {
  local_manifest()
  out <- suppressWarnings(suppressMessages(irw_version("2026-08-01")))
  expect_identical(attr(out, "irw_version"), 2L)
  # v2.0 of the shard, but irw_meta was still on v1.0 that day.
  expect_identical(out$version[out$dataset == "irw_meta"], "v1.0")
})

test_that("a date lands on the version in force, not the next one", {
  local_manifest()
  # One second before v3 was released is still v2.
  out <- suppressWarnings(suppressMessages(irw_version("2026-08-14 23:59:59")))
  expect_identical(attr(out, "irw_version"), 2L)
  out <- suppressWarnings(suppressMessages(irw_version("2026-08-15")))
  expect_identical(attr(out, "irw_version"), 3L)
})

test_that("a date before the corpus existed is an error, not a guess", {
  local_manifest()
  expect_error(irw_version("2020-01-01"), "did not exist yet")
})

test_that("a date lookup resting on an overwritten timestamp warns", {
  # This is the case the warning exists for: the tag itself may be wrong,
  # because a later version could already have been released in the bracket.
  local_manifest()
  expect_warning(
    suppressMessages(irw_version("2026-08-01")),
    "approximate"
  )
})

test_that("asking for the newest version does not warn about dates", {
  # Same bracketed row, weaker claim: v3's pins are exactly what v3 held, so a
  # warning here would be wrong. Reported as a message instead.
  local_manifest()
  expect_no_warning(suppressMessages(irw_version()))
})

test_that("irw_version reports which pins are approximate", {
  local_manifest()
  out <- suppressMessages(irw_version())
  expect_true(out$approximate[out$dataset == "item_response_warehouse"])
  expect_false(out$approximate[out$dataset == "irw_meta"])
})

test_that("a manifest with unexpected columns is rejected rather than parsed", {
  # Guards against a schema change in the irw repo silently producing wrong
  # pins in an old package install.
  broken <- MANIFEST_FIXTURE
  names(broken)[names(broken) == "redivis_tag"] <- "tag"
  e <- irw:::.irw_env
  withr::defer(suppressWarnings(rm(list = "manifest", envir = e)))
  expect_error(
    irw:::.irw_manifest_check_columns(broken),
    "expected columns"
  )
})

test_that(".irw_as_utc reads the forms a person would actually type", {
  expect_identical(
    format(irw:::.irw_as_utc("2026-08-01"), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2026-08-01 00:00:00"
  )
  expect_identical(
    format(irw:::.irw_as_utc("2026-08-01 12:30"), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2026-08-01 12:30:00"
  )
  expect_identical(
    format(irw:::.irw_as_utc(as.Date("2026-08-01")), "%Y-%m-%d", tz = "UTC"),
    "2026-08-01"
  )
})

test_that(".irw_as_utc rejects what it cannot read rather than returning NA", {
  expect_error(irw:::.irw_as_utc("last tuesday"), "Could not read")
  expect_error(irw:::.irw_as_utc(c("2026-08-01", "2026-08-02")), "single date")
  expect_error(irw:::.irw_as_utc(20260801), "single date")
})

# ---- looking a version up by number -----------------------------------------

test_that("irw_version(version =) reports exactly what that version held", {
  local_manifest()
  out <- suppressMessages(irw_version(version = 2))
  expect_identical(attr(out, "irw_version"), 2L)
  expect_identical(out$version[out$dataset == "irw_meta"], "v1.0")
})

test_that("a version number never warns, however bracketed its dates are", {
  # The whole point of citing a number: v2's pins are what v2 held, full stop.
  local_manifest()
  expect_no_warning(suppressMessages(irw_version(version = 2)))
})

test_that("an unreleased version number is an error naming the range", {
  local_manifest()
  expect_error(irw_version(version = 99), "no version 99")
  expect_error(irw_version(version = 99), "1 to 3")
})

test_that("naming both a version and a date is refused rather than ranked", {
  local_manifest()
  expect_error(irw_version(date = "2026-08-01", version = 2), "not both")
})

test_that(".irw_as_irw_version takes what a person would type", {
  expect_identical(irw:::.irw_as_irw_version(332), 332L)
  expect_identical(irw:::.irw_as_irw_version(332L), 332L)
  expect_identical(irw:::.irw_as_irw_version(" 332 "), 332L)
})

test_that(".irw_as_irw_version refuses a date rather than coercing one", {
  # A date silently read as a version number would pin the wrong corpus, so the
  # error points at the argument that does accept dates.
  expect_error(irw:::.irw_as_irw_version("2026-08-01"), "date = ")
  expect_error(irw:::.irw_as_irw_version("v332"), "version number")
  expect_error(irw:::.irw_as_irw_version(332.5), "version number")
  expect_error(irw:::.irw_as_irw_version(c(1, 2)), "version number")
  expect_error(irw:::.irw_as_irw_version(0), "version number")
})

# ---- pinning the whole corpus at once ---------------------------------------

# `irw_use_version()` opens every dataset to confirm its tag; this stands in for
# Redivis and echoes back whatever tag it is asked for.
local_version_echo <- function(env = parent.frame()) {
  local_irw_binding(
    ".irw_open_dataset_at_version",
    function(spec, version) list(properties = list(version = list(tag = version))),
    env = env
  )
}

test_that("irw_use_version pins every dataset the version held", {
  local_no_pins()
  local_manifest()
  local_version_echo()

  out <- suppressMessages(irw_use_version(2))

  expect_identical(attr(out, "irw_version"), 2L)
  expect_identical(irw:::.irw_pinned_version("item_response_warehouse"), "v2.0")
  expect_identical(irw:::.irw_pinned_version("irw_meta"), "v1.0")
})

test_that("a dataset with no release then is pinned to nothing, not to current", {
  # Leaving it unpinned would read today's data into a run reproducing v2.
  local_no_pins()
  local_manifest()
  local_version_echo()

  out <- suppressMessages(irw_use_version(2))

  expect_identical(
    irw:::.irw_pinned_version("irw_simsyn"),
    irw:::.irw_absent_version
  )
  expect_true(is.na(out$version[out$dataset == "irw_simsyn"]))
})

test_that("reading a dataset that did not exist then is an error", {
  local_no_pins()
  local_irw_pristine(c(".irw_open_dataset", ".irw_redivis_dataset"))
  e <- irw:::.irw_env
  e$pinned_versions <- c(irw_simsyn = irw:::.irw_absent_version)

  expect_error(
    irw:::.irw_open_dataset(list(user = "datapages", dataset = "irw_simsyn:0btg")),
    "had no released version"
  )
})

test_that("irw_get_version reports an absent pin without asking Redivis", {
  local_no_pins()
  local_irw_binding(".irw_open_dataset", function(spec) {
    stop("irw_get_version should not have opened this dataset")
  })
  e <- irw:::.irw_env
  e$pinned_versions <- c(irw_simsyn = irw:::.irw_absent_version)

  out <- irw_get_version("irw_simsyn")
  expect_true(is.na(out$version))
  expect_true(out$pinned)
})

test_that("irw_use_version with no argument pins the newest version", {
  local_no_pins()
  local_manifest()
  local_version_echo()

  out <- suppressMessages(irw_use_version())
  expect_identical(attr(out, "irw_version"), 3L)
  expect_identical(irw:::.irw_pinned_version("irw_meta"), "v2.0")
})

test_that("irw_use_version by date carries the date warning", {
  local_no_pins()
  local_manifest()
  local_version_echo()

  expect_warning(
    suppressMessages(irw_use_version(date = "2026-08-01")),
    "approximate"
  )
  expect_identical(irw:::.irw_pinned_version("irw_meta"), "v1.0")
})

test_that("irw_use_version clears caches so nothing is served from the old version", {
  local_no_pins()
  local_manifest()
  local_version_echo()
  e <- irw:::.irw_env
  e$datasource_list <- "stale"
  e$metadata_tibble <- "stale"

  suppressMessages(irw_use_version(2))

  expect_false(exists("datasource_list", envir = irw:::.irw_env))
  expect_false(exists("metadata_tibble", envir = irw:::.irw_env))
  # The manifest is a download, not a version-dependent cache; keeping it saves
  # a request and it is what we just read.
  expect_false(is.null(irw:::.irw_env$manifest))
})

test_that("a Redivis tag that resolves to something else stops the whole pin", {
  local_no_pins()
  local_manifest()
  local_irw_binding(
    ".irw_open_dataset_at_version",
    function(spec, version) list(properties = list(version = list(tag = "v99.9")))
  )

  expect_error(suppressMessages(irw_use_version(2)), "could not be resolved")
})
