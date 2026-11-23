# Replace a binding in the irw namespace until `env` exits, then put the
# original back. Used for package data such as `.irw_datasource_specs` and
# `.irw_env`. For functions, `local_mocked_bindings()` does the same job, as
# long as it is left to restore on the caller's exit: never pass it
# `.env = asNamespace("irw")`, which ties the restore to the namespace and so
# never undoes the mock (Rpkg#172).
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
      nom = irw:::.irw_datasource_specs$nom,
      conj = irw:::.irw_datasource_specs$conj,
      rank = irw:::.irw_datasource_specs$rank
    ),
    env = env
  )
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

# The session cache is a package-level environment rather than a binding, so it
# is set directly and removed on exit. Setting the cache, rather than mocking
# `.irw_core_shard_map()`, also exercises the real lookup.
local_shard_map <- function(map, env = parent.frame()) {
  cache <- irw:::.irw_env
  had <- exists("core_shard_map", envir = cache)
  old <- if (had) get("core_shard_map", envir = cache) else NULL
  assign("core_shard_map", map, envir = cache)
  withr::defer(
    if (had) {
      assign("core_shard_map", old, envir = cache)
    } else {
      rm("core_shard_map", envir = cache)
    },
    envir = env
  )
  invisible(NULL)
}
