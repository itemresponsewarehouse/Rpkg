# Extracted from test-info.R:22

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "irw", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
local_mocked_bindings(.initialize_datasource = function(source = "core", ...) {
    fake_datasources[[source]]
  })
msgs <- capture_messages(irw_info())
for (src in .irw_sources) {
    expect_true(any(grepl(paste0("\\s", src, "\\s"), msgs)), info = src)
  }
expect_true(any(grepl("Core\\s+core\\s+15\\s+2\\.00 GB", msgs)))
expect_true(any(grepl("Total Table Count:\\s+38\\b", msgs)))
