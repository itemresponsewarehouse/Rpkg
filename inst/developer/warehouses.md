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
    list(user = "datapages", dataset = "item_response_warehouse_4:980f"),
    list(user = "datapages", dataset = "item_response_warehouse_5:3ykx"),
    list(user = "datapages", dataset = "item_response_warehouse_6:XXXX")  # example
  ),
  ...
)
```

No other files need hard-coded warehouse IDs for fetch/list/filter/download to work.

## Runtime behavior

- **Search order:** core warehouses are queried **newest first** so duplicate table names resolve to the latest copy.
- **Unavailable warehouses are skipped.** A warehouse that exists but has no
  released version yet returns an error for read-only tokens, so
  `.irw_open_core_datasources()` drops it with a warning rather than failing
  every lookup. If *no* warehouse opens (e.g. a bad token, which fails for all
  of them), it errors; an authentication failure stops immediately.
- **Listing:** `irw_list_tables()` unions tables across all core warehouses and deduplicates by name (newest wins).
- **Metadata:** rows in `irw_meta` are filtered to tables that exist in **any** core warehouse.
- **Caching:** session caches invalidate automatically when:
  - the configured warehouse list changes (package upgrade or `devtools::load_all()`), or
  - any core warehouse version tag changes, or
  - the `irw_meta` dataset version changes.

## After adding a warehouse

1. Append the new `list(user = ..., dataset = ...)` to `.irw_datasource_specs$core`.
2. **Publish a release of the new dataset on Redivis before shipping this.**
   An unreleased warehouse is skipped with a warning for read-only users, so its
   tables are simply missing until it is released.
3. Run `devtools::test()` (or at least `tests/testthat/test-redivis-datasets.R`).
4. Optionally verify live access with Redivis authenticated:
   - `irw_list_tables()` shows tables from the new warehouse
   - `irw_fetch("<known_table>")` succeeds for a table only in the new warehouse

Simulation, competition, and nominal sources each use a single dataset spec under their respective keys in the same config object.

## Why warehouses are a list at all

**Redivis caps any dataset at 1000 tables.** That is the only reason there is
more than one warehouse, and it applies to every dataset — so item text is a
shard list too (below), and any other dataset would become one if it grew that
far.

## Non-source datasets

Two things are not table sources and so live beside `.irw_datasource_specs`
rather than inside it, in the same `R/redivis-config.R`:

- `.irw_meta_spec` — the `irw_meta` metadata/biblio/tags backbone. Every
  fetcher in `R/redivis-metadata.R` opens it through `.irw_open_meta_dataset()`;
  none of them names the owner or dataset directly. A single spec: `irw_meta` is
  nowhere near the cap.
- `.irw_itemtext_specs` — the item text **shard list** (`irw_text`,
  `irw_text_2`, …), oldest to newest, with the same semantics as `$core`:
  opened by `.irw_open_datasources()`, ordered newest-first by
  `.irw_order_datasources(..., "text")`, and reached through
  `.irw_itemtext_datasources()`.

They are deliberately *not* keyed under `.irw_datasource_specs`, because that
object is indexed by the user-facing `source` argument (`.irw_sources` in
`R/redivis-datasets.R`), and neither is a valid `source`.

### Adding an item text shard

`irw_text` holds ~590 tables against a 1000 cap, and the extraction queue has
~1,000 still pending, so a second shard is a matter of when. The code is already
shard-aware; the cutover is config plus a Redivis click.

1. Create `irw_text_2` on Redivis under `datapages`, upload at least one table,
   and **publish a release before touching any config**. An unreleased dataset
   errors for read-only tokens — both client packages will drop it with a
   warning and its tables will simply be missing for every non-owner user, with
   nobody seeing an error. This is the one trap.
2. Append `"irw_text_2"` to `IRW_TEXT_DATASETS` in
   `src/metadata/redivis_config.R`. `red_up` parses that file, so new
   `*__items.csv` uploads default to the new shard immediately.
3. Append `list(user = "datapages", dataset = "irw_text_2:<refid>")` to
   `.irw_itemtext_specs` here, and the matching tuple to `ITEMTEXT_REFS` in
   `Python-pkg/src/irw/config.py`. **Ship steps 2 and 3 together** — a config
   naming a shard the other two do not is the drift of ben-domingue/irw#1733.
   Reference ids rotate when a release is cut, so read the refid off Redivis at
   that moment rather than from an older document.
4. Run `python3 -m red_up.manifest` and commit `metadata/version_manifest.tsv`;
   the daily `--check` fails until the new dataset's release rows are appended.
5. Re-run `src/metadata/08_itemtext.R` (it reads the union through this package,
   so no edit is needed) and upload `irw_meta` as usual.
6. Update the browse link in `irw_site/itemtext.qmd`, which points at shard 1's
   Redivis page and would otherwise show a reader only part of the item text.
7. **Do not move existing tables.** Newest-first resolution keeps every table
   reachable where it already is; moving one *creates* the shadowing problem
   rather than solving it.

## Changing the Redivis owner

All five auxiliary datasets (`irw_meta`, `irw_text`, `irw_simsyn`,
`irw_competitions`, `irw_nominal`) moved from the `bdomingu` personal account to
`datapages` in August 2026, joining the six core warehouses already there. The
short dataset IDs were unchanged by the transfer. Redivis auto-resolves
references to a previous owner, so a future move is again a config-only change:
edit the `user` fields in `R/redivis-config.R` and nothing else.
