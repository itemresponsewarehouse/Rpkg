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
