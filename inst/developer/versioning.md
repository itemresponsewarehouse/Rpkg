# Version pinning

This note is for package maintainers only (not included in the user-facing README).

## What it does

IRW data on Redivis is versioned, and the warehouses are re-released as datasets
are added or corrected. `irw_set_version()` fixes one Redivis dataset at a
released version for the rest of the R session, so an analysis script re-run
next year reads the same data. See Rpkg#105 and Rpkg#114 for the motivating
cases, and ben-domingue/irw#1705 for the roadmap item.

- `irw_set_version(dataset, version)` — pin
- `irw_get_version(dataset = NULL)` — report the version in use per dataset, and
  whether it is pinned; this is how a user finds the tags to write into a script
- `irw_reset_version(dataset = NULL)` — unpin

## Where it hooks in

Pins live in `.irw_env$pinned_versions`, a named character vector keyed by
dataset name (the part of the spec before the colon, e.g.
`item_response_warehouse`).

Everything goes through one seam: `.irw_open_dataset()` in `R/redivis-config.R`
looks up the pin and hands the version to `.irw_redivis_dataset()`. Because every
IRW dataset — the six core warehouses, sim/comp/nom, `irw_meta` and `irw_text` —
is opened through that function, a pin applies uniformly to fetches, table
listings, and metadata. Adding a datasource to `.irw_datasource_specs` makes it
pinnable with no further work.

## Two things that are easy to get wrong

**Redivis does not reject an unrecognized version string.** Asking for
`version = "banana"` returns the *current* release rather than an error, which
would silently defeat the pin. `irw_set_version()` therefore validates the tag
against `^v?[0-9]+\.[0-9]+$` first, and then checks that the version Redivis
resolved to is the one that was requested.

**Pinning invalidates every cache.** The opened dataset objects, the table
listings derived from them, and the metadata filtered against those listings are
all version-specific. `.irw_clear_all_datasource_caches()` clears
`.irw_env` by exclusion (keeping only the pins themselves and the item text
disclaimer flag) rather than by an enumerated list, so a cache added later is not
left stale behind a pin.

## Missing tables under a pin

A table added after the pinned release is an error, not a silent fetch from the
current version: quietly mixing versions would break the reproducibility the pin
exists to provide. `.irw_pinned_not_found_message()` produces that message, and
both `.fetch_redivis_table()` and `fetch_single_data()` fall back to it before
reporting "does not exist in IRW", so the user can tell "never existed" from
"not in the release you pinned".

## Tests

`tests/testthat/test-version.R`. Note the mocking helpers in
`helper-redivis.R`: testthat 3.3.2 does not restore bindings mocked with
`local_mocked_bindings()` in this package, so mocks leak across test files. Use
`local_irw_binding()` / `local_irw_pristine()` for anything in the package
namespace that another file might have mocked.
