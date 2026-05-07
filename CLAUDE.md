# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**irw** is an R package providing programmatic access to the [Item Response Warehouse (IRW)](https://itemresponsewarehouse.github.io/Rpkg) — an open repository of harmonized item response data hosted on Redivis. All data access flows through the `redivis` R client, which requires browser-based OAuth authentication on first use per session.

## Common Commands

```bash
# Generate documentation from roxygen comments
Rscript -e "roxygen2::roxygenise()"

# Run full package check (matches CI flags)
R CMD check --no-manual --compact-vignettes=gs+qpdf .

# Run tests only
Rscript -e "testthat::test_dir('tests/testthat')"

# Run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-irw_simdata.R')"

# Build pkgdown documentation site
Rscript -e "pkgdown::build_site()"
```

Note: Most tests that exercise `irw_fetch()` or metadata functions are skipped in CI due to Redivis authentication requirements. `irw_simdata()` tests run without authentication.

## Architecture

### Data Sources

All public functions accept a `source` parameter routing requests to different Redivis datasets:

| source | Redivis dataset | Purpose |
|--------|----------------|---------|
| `"core"` | `item_response_warehouse:as2e`, `item_response_warehouse_2:epbx` | Main production datasets |
| `"sim"` | `irw_simsyn:0btg` | Synthetic/simulation datasets |
| `"comp"` | `irw_competitions:cmd7` | Competition datasets |
| `"nom"` | `irw_nominal:614n` | Nominal response datasets |

The old boolean arguments `sim=`, `comp=`, `nom=` are deprecated in favor of `source=`.

### Caching Layer (`R/zzz.R`, `R/redivis-metadata.R`, `R/redivis-datasets.R`)

An internal environment `.irw_env` caches Redivis dataset objects and metadata tables across calls within a session. Caching is version-aware — Redivis dataset version tags are compared to detect stale data and trigger refreshes. This minimizes API calls without serving stale metadata.

### Data Format Contract

IRW long format always has columns: `id`, `item`, `resp` (required) plus optional `wave`, `date`, `rater`, and covariates. The `resp` column is coerced from character to numeric (with warnings) when non-numeric values are encountered; `"NA"` strings and empty strings become `NA`.

- `irw_long2resp()` converts long → wide response matrix (respondents × items)
- `irw_resp2long()` converts wide → long
- `irw_check_resp()` validates: flags single-category items and sparse categories (< 5 obs or < 1% within-item proportion)

### Key Files

- `R/fetch.R` — `irw_fetch()`, the primary data retrieval function; handles single and multiple table fetches, deduplication, character→numeric coercion
- `R/explore.R` — `irw_list_tables()`, `irw_filter()`, `irw_info()`; all metadata browsing
- `R/filter.R` — `irw_filter()` advanced filtering; supports multiple simultaneous criteria with AND logic
- `R/long2resp.R` — format conversion with diagnostics
- `R/merge.R` — `irw_merge()`; combines tables from the same source matched by DOI/BibTex
- `R/simulate.R` — `irw_simdata()` for 1PL/2PL/3PL dichotomous IRT simulation (no auth required)
- `R/simu_diff.r` — `irw_simu_diff()` for simulating item difficulties from empirical pools (Zhang et al. 2025)
- `R/redivis-metadata.R` — version-aware metadata caching helpers
- `R/utils_general.R` — `irw_download()`, `irw_save_bibtex()`

### Dependencies

`redivis` must be installed from GitHub (`redivis/redivis-r`), not CRAN. The CI workflow installs it explicitly before `rcmdcheck`. All other dependencies (`glue`, `httr`, `tibble`, `stats`) are on CRAN.
