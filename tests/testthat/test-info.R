fake_ds <- function(tables, bytes = 1024^3, created = 1e12, updated = 2e12) {
  list(properties = list(tableCount = tables, totalNumBytes = bytes,
                         createdAt = created, updatedAt = updated,
                         version = list(tag = "1.0"), url = "u"))
}

fake_datasources <- list(
  core = list(fake_ds(10), fake_ds(5, created = 5e11)),
  nom = list(fake_ds(3)), sim = list(fake_ds(2)), comp = list(fake_ds(4)),
  conj = list(fake_ds(6), fake_ds(1)), rank = list(fake_ds(7, updated = 3e12))
)

test_that("irw_info() with no arguments reports every family and a grand total", {
  local_mocked_bindings(.initialize_datasource = function(source = "core", ...) {
    fake_datasources[[source]]
  })
  msgs <- capture_messages(irw_info())
  for (src in .irw_sources) {
    expect_true(any(grepl(paste0("\\s", src, "\\s"), msgs)), info = src)
  }
  expect_true(any(grepl("Core\\s+core\\s+15\\s+2\\.00 GB", msgs)))
  expect_true(any(grepl("Total Table Count:\\s+38\\b", msgs)))
  expect_true(any(grepl("Total Data Size:\\s+8\\.00 GB", msgs)))
  expect_true(any(grepl(.irw_format_utc(5e8), msgs, fixed = TRUE)))
  expect_true(any(grepl(.irw_format_utc(3e9), msgs, fixed = TRUE)))
})

test_that("naming a source reports that family alone", {
  opened <- character()
  local_mocked_bindings(.initialize_datasource = function(source = "core", ...) {
    opened <<- c(opened, source)
    fake_datasources[[source]]
  })
  msgs <- capture_messages(irw_info(source = "core"))
  expect_identical(opened, "core")
  expect_true(any(grepl("Total Table Count:\\s+15\\b", msgs)))

  opened <- character()
  msgs <- capture_messages(irw_info(source = "rank"))
  expect_identical(opened, "rank")
  expect_true(any(grepl("IRW Rankings Database Information", msgs)))
  expect_true(any(grepl("Total Table Count:\\s+7\\b", msgs)))
})

test_that("the deprecated family flags still report one family", {
  opened <- character()
  local_mocked_bindings(.initialize_datasource = function(source = "core", ...) {
    opened <<- c(opened, source)
    fake_datasources[[source]]
  })
  suppressWarnings(capture_messages(irw_info(sim = TRUE)))
  expect_identical(opened, "sim")
})

test_that("details = TRUE adds the core breakdown to the all-family report", {
  local_mocked_bindings(.initialize_datasource = function(source = "core", ...) {
    fake_datasources[[source]]
  })
  msgs <- capture_messages(irw_info(details = TRUE))
  expect_true(any(grepl("All Families", msgs)))
  expect_length(grep("^Dataset [0-9]+:", msgs), 2L)
})

test_that("an unavailable family is shown and not silently left out of the total", {
  local_mocked_bindings(.initialize_datasource = function(source = "core", ...) {
    if (source == "conj") stop("boom")
    fake_datasources[[source]]
  })
  expect_warning(msgs <- capture_messages(irw_info()), "'conj' family: boom")
  expect_true(any(grepl("Conjoint\\s+conj\\s+unavailable", msgs)))
  expect_true(any(grepl("Total \\(available only\\):\\s+31\\b", msgs)))
})
