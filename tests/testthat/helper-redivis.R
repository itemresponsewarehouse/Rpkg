# testthat's local_mocked_bindings() does not restore non-function bindings
# (checked against testthat 3.3.2): a mocked `.irw_datasource_specs` or
# `.irw_env` stays mocked for the rest of the run, which makes later test files
# fail depending on the order they happen to run in. Mock package data through
# this helper instead, which restores the original value on exit.
local_irw_binding <- function(name, value, env = parent.frame()) {
  ns <- asNamespace("irw")
  old <- get(name, envir = ns)
  was_locked <- bindingIsLocked(name, ns)

  set <- function(v) {
    if (was_locked) unlockBinding(name, ns)
    assign(name, v, envir = ns)
    if (was_locked) lockBinding(name, ns)
  }

  set(value)
  withr::defer(set(old), envir = env)
  invisible(old)
}

# The core warehouse list used by most datasource tests: two warehouses, with
# the other sources left as configured.
local_irw_core_specs <- function(core, env = parent.frame()) {
  local_irw_binding(
    ".irw_datasource_specs",
    list(
      core = core,
      sim = irw:::.irw_datasource_specs$sim,
      comp = irw:::.irw_datasource_specs$comp,
      nom = irw:::.irw_datasource_specs$nom
    ),
    env = env
  )
}

# testthat 3.3.2 also fails to restore mocked *function* bindings in this
# package's setup, so mocks from one test file are still in place when the next
# runs. Snapshot the bindings that tests mock before any test file has run, so
# a test that depends on the real implementation can ask for it explicitly.
.irw_pristine_bindings <- local({
  ns <- asNamespace("irw")
  nms <- c(
    ".irw_datasource_specs",
    ".irw_env",
    ".irw_open_dataset",
    ".irw_redivis_dataset",
    ".irw_open_dataset_at_version",
    ".irw_live_table_names"
  )
  stats::setNames(lapply(nms, get, envir = ns), nms)
})

# Restore the real implementations of `names` for the duration of a test.
local_irw_pristine <- function(names, env = parent.frame()) {
  for (nm in names) {
    local_irw_binding(nm, .irw_pristine_bindings[[nm]], env = env)
  }
  invisible(NULL)
}

# The manifest is downloaded once per session and cached in the package
# environment. Tests install this fixture in that cache instead, so nothing
# touches the network. Two datasets only: the nine others in
# `.irw_pinnable_specs()` are then absent from every version, which is exactly
# the case `irw_use_version()` has to handle.
MANIFEST_FIXTURE <- data.frame(
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

local_manifest <- function(manifest = MANIFEST_FIXTURE, env = parent.frame()) {
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

# Version pins live in the package session environment, so every test that sets
# one must put the environment back the way it found it.
local_no_pins <- function(env = parent.frame()) {
  # Other test files leave mocked bindings behind (see helper-redivis.R), so
  # start from the real session environment and warehouse config every time.
  local_irw_pristine(c(".irw_env", ".irw_datasource_specs"), env = env)
  e <- irw:::.irw_env
  had <- exists("pinned_versions", envir = e)
  old <- if (had) e$pinned_versions else NULL
  withr::defer(
    {
      if (had) e$pinned_versions <- old else suppressWarnings(rm(list = "pinned_versions", envir = e))
    },
    envir = env
  )
  e$pinned_versions <- stats::setNames(character(0), character(0))
}
