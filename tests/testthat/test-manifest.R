library(testthat)

# The manifest is downloaded once per session and cached in the package
# environment. Every test here installs a fixture in that cache instead, so
# nothing touches the network.
FIXTURE <- data.frame(
  irw_version = c(1L, 1L, 2L, 2L, 3L, 3L),
  irw_released_at = c(
    "2024-01-01T00:00:00Z", "2024-01-01T00:00:00Z",
    "2026-07-01T00:00:00Z", "2026-07-01T00:00:00Z",
    "2026-08-15T00:00:00Z", "2026-08-15T00:00:00Z"
  ),
  dataset = c("item_response_warehouse", "irw_meta",
              "item_response_warehouse", "irw_meta",
              "item_response_warehouse", "irw_meta"),
  redivis_tag = c("v1.0", "v1.0", "v2.0", "v1.0", "v2.0", "v2.0"),
  redivis_released_at = c(
    "2024-01-01T00:00:00Z", "2024-01-01T00:00:00Z",
    "2026-07-01T00:00:00Z", "2024-01-01T00:00:00Z",
    "2026-07-01T00:00:00Z", "2026-08-15T00:00:00Z"
  ),
  # The first shard's dates are the overwritten kind; irw_meta's are genuine.
  precision = c("bracketed", "exact", "bracketed", "exact", "bracketed", "exact"),
  redivis_released_before = c(
    "2026-07-01T00:00:00Z", "", "2026-09-01T00:00:00Z", "",
    "2026-09-01T00:00:00Z", ""
  ),
  stringsAsFactors = FALSE
)

local_manifest <- function(manifest = FIXTURE, env = parent.frame()) {
  e <- irw:::.irw_env
  had <- !is.null(e$manifest)
  old <- e$manifest
  withr::defer(
    {
      if (had) e$manifest <- old else suppressWarnings(rm(list = "manifest", envir = e))
    },
    envir = env
  )
  manifest$released <- irw:::.irw_parse_utc(manifest$irw_released_at)
  e$manifest <- manifest
}

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
  broken <- FIXTURE
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
