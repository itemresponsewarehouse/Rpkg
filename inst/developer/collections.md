# Collections: what they are, and why adding one is a data change

Maintainer note (not user-facing). Companion to `tags.md` and `warehouses.md`.
Issue [#1633](https://github.com/ben-domingue/irw/issues/1633).

## Current state

| source | collections? | registry | CSVs | Redivis tables |
|---|---|---|---|---|
| `core` | yes | `src/collections/registry.csv` | `src/metadata/collections.csv`, `collection_members.csv` | `irw_meta` / `collections`, `collection_members` |
| `nom` / `comp` / `sim` | no | — | — | — |

The list of sources with collections lives in **one place**:
`.irw_collection_sources` in `R/redivis-config.R`. `filter.R` and `explore.R`
read it rather than testing `source == "core"` — same pattern as
`.irw_tag_sources`.

## The shape, and why

A collection is a labelled group of tables. Tables belong to **many**
collections at once (610 of them do), so membership is stored long — one row per
`(table, collection)` — not as a column per collection.

That is the load-bearing decision. `irw_filter()` hardcodes its argument list
three times, the Python package has three separate constants, and the site needs
four coordinated edits. With a column-per-collection design every new collection
would be an ~8-file change across three repos. With long format plus one generic
`collection=` argument, **adding a collection is a data change**: one line in
`src/collections/registry.csv`, no code anywhere.

So: do not add per-collection branches, arguments, or constants. If you find
yourself writing `if (collection == "big_five")`, stop.

## Two tables, handled differently

- `collections` — the registry, one row per collection. **No `table` column**,
  so `.fetch_collections_table()` must NOT call
  `.irw_filter_rows_to_live_tables()`; that would error or silently empty it.
- `collection_members` — long. Live-filtered like every other per-table product.

Consequence: after live filtering, membership can be smaller than the `n_tables`
the registry published at build time. `irw_collections()` **recomputes**
`n_tables` from the filtered members rather than passing the published column
through. If you change that, a user can be told 63 tables exist and then fetch
61.

## Coverage is not decoration

`coverage` records how much of the warehouse a collection's rule actually
searched:

- `metadata-complete` — read `irw_metadata()`, so it saw every documented table.
  Still not the whole warehouse: `irw_metadata()` itself has gaps (no w5/w6
  rows, ~403 tables missing).
- `tagged-subset-only` — read the tags table, which covers ~62% of documented
  tables, and far less in the newer warehouses (w1 99.6%, w2 77.4%, **w3 27.2%,
  w4 34.7%**). Biased toward older tables. Not "all" of anything.
- `curated-only` — chosen by hand.

`irw_collection()` prints this when it is not `metadata-complete`. That message
is the feature, not noise — someone assembling a meta-analytic corpus from
`irw_collection("depression")` needs to know it searched 2,251 of 3,650 tables.
Do not quiet it by default.

## Three invariants that break silently

**1. `collection` must not go in `tag_filters`.** It has its own table keyed on
`table`, so `irw_filter()` handles it in a separate block modelled on `license`.
Routing it through `tag_filters` sends it into the loop testing
`colname %in% colnames(tags)`, which finds no such column, warns "Ignored", and
**returns unfiltered results**. `test-collections.R` has a regression test
asserting an unknown collection errors rather than warns. Keep it.

**2. Join table names case-insensitively.** `metadata.csv` preserves original
case (307 rows are not lower-case); the tags CSV is lower-cased. A
case-sensitive join silently drops those 307 tables. Every comparison in the
package uses `tolower()` on both sides.

**3. Never write `##'` at the start of a comment line.** roxygen reads the `#'`
inside it as a documentation directive, and the result is garbage entries in
NAMESPACE. Likewise, do not insert code between an existing roxygen block and
the function it documents — that silently reassigns the documentation to your
new function.

## Adding a collection

1. Add a line to `src/collections/registry.csv` (rule grammar in
   `src/collections/README`).
2. `cd src/metadata && Rscript 10_collections.R` — needs no credentials.
3. Check `collections_report.txt`; for a `cname:` rule, spot-check the matched
   construct names, since a broad regex is the easy mistake.
4. Upload with the site-update skill's `upload_meta.py`.

Nothing in this package changes.
