# Tags: which sources have them, and why

Maintainer note (not user-facing). Companion to `warehouses.md`.

## Current state

| source | tags? | sheet | CSV | Redivis table |
|---|---|---|---|---|
| `core` | yes | "IRW Tags" `1V3ef0sa...`, gid `126134123` | `src/metadata/tags.csv` | `irw_meta` / `tags:7nkh` |
| `nom`  | yes | "IRW Tags Nominal" `1v3toO6O...`, gid `126134123` | `src/metadata/nominal_tags.csv` | `irw_meta` / `nominal_tags` |
| `comp` | **no, by design** | — | — | — |
| `sim`  | **no, by design** | — | — | — |

The list of tagged sources lives in **one place**: `.irw_tag_sources` in
`R/redivis-config.R`. `filter.R`, `explore.R` and `fetch.R` all read it rather
than testing `source == "core"`. Fetching goes through
`.irw_tags_for_source()` in `R/redivis-metadata.R`.

## Why comp and sim have no tags

Decided in [#1689](https://github.com/ben-domingue/irw/issues/1689). Please
don't re-litigate without new evidence.

- **`comp`** (`irw_competitions`) — pairwise-comparison / arena data. Most of the
  13-column tag schema is inapplicable: there is no construct being measured, no
  item format, no respondent age. `n_actors` already exists as the meaningful
  filter, and `irw_filter(source = "comp")` supports it directly.
- **`sim`** (`irw_simsyn`) — simulated/synthetic. Tags are largely meaningless by
  construction; the useful provenance is the generating script, not a construct
  name or a sample description.

`irw_filter()` rejects tag filters for both, and `.irw_tags_for_source()` errors
rather than returning an empty tibble — an empty tibble would silently filter
every table away and read as "no matches" instead of "wrong question".

## Adding tags for a new source

1. Add a tab with the standard 13 columns (see below), including the
   instruction row as row 2.
2. Add an entry to `dbs` in `src/metadata/03_tags.R`.
3. Create the target table in the `irw_meta` Redivis dataset and add it to
   `FILE_TABLE_MAP` in
   `src/.claude/skills/irw-site-update/scripts/upload_meta.py`.
4. Add a fetcher in `R/redivis-metadata.R`, a branch in
   `.irw_tags_for_source()`, and the source to `.irw_tag_sources`.
5. Add the CSV to `tags_csvs` in `audit_tables.R`.

## Two invariants that break silently

**1. `Context Text` must never be published.** `03_tags.R` selects columns by
position — `c(1, 6:12, 3)` — which omits column 4, `Context Text`: verbatim
excerpts from source papers. That positional selection is the *only* thing
keeping raw source text out of the public CSVs and Redivis tables. Reordering
or inserting a sheet column changes what gets published, with no error. If you
touch that selection, verify the output has no `context text` column.

**2. Row 1 of the data is an instruction row, not data.** Every tags sheet
carries a template row directly under the header, and `03_tags.R` drops it.
A sheet missing that row would lose its first real table silently, so the
script asserts the row is present (sentinel: `should match what is on redivis`)
and stops rather than guessing.

The 13 sheet columns, in order:

```
table, Rater, Construct Name, Context Text, Item text available?, Age Range,
Child Age (for child-focused studies), Sample, Construct type,
Measurement tool, Item format, Primary Language(s), Notes
```

## Known data issue (not fixed here)

Four core table names carry a trailing space on the sheet, e.g.
`'himmelstein-berlin_numeracy-2025 '`. `03_tags.R` does not trim, so these
never match a live table and `.irw_filter_rows_to_live_tables()` drops their
tags. Fixing it means trimming in `03_tags.R`, which changes the published
core `tags.csv` — a deliberate, separately reviewed change, not a drive-by.
