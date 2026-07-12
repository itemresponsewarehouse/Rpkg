# Adding a main IRW Redivis warehouse

This note is for package maintainers only (not included in the user-facing README).

## Where to edit

All Redivis dataset identifiers live in one place:

- `R/redivis-config.R` — `.irw_datasource_specs`

Main production warehouses are listed under `$core` in **oldest-to-newest** order. Each entry is a list with `user` and `dataset` fields:

```r
.irw_datasource_specs <- list(
  core = list(
    list(user = "datapages", dataset = "item_response_warehouse:as2e"),
    list(user = "datapages", dataset = "item_response_warehouse_2:epbx"),
    list(user = "datapages", dataset = "item_response_warehouse_3:5xaj"),
    list(user = "datapages", dataset = "item_response_warehouse_4:XXXX")  # example
  ),
  ...
)
```

No other files need hard-coded warehouse IDs for fetch/list/filter/download to work.

## Runtime behavior

- **Search order:** core warehouses are queried **newest first** so duplicate table names resolve to the latest copy.
- **Listing:** `irw_list_tables()` unions tables across all core warehouses and deduplicates by name (newest wins).
- **Metadata:** rows in `irw_meta` are filtered to tables that exist in **any** core warehouse.
- **Caching:** session caches invalidate automatically when:
  - the configured warehouse list changes (package upgrade or `devtools::load_all()`), or
  - any core warehouse version tag changes, or
  - the `irw_meta` dataset version changes.

## After adding a warehouse

1. Append the new `list(user = ..., dataset = ...)` to `.irw_datasource_specs$core`.
2. Run `devtools::test()` (or at least `tests/testthat/test-redivis-datasets.R`).
3. Optionally verify live access with Redivis authenticated:
   - `irw_list_tables()` shows tables from the new warehouse
   - `irw_fetch("<known_table>")` succeeds for a table only in the new warehouse

Simulation, competition, and nominal sources each use a single dataset spec under their respective keys in the same config object.
