# `irw`: R Package for the Item Response Warehouse

This repository hosts the R package `irw`, which provides programmatic access to the [Item Response Warehouse (IRW)](https://itemresponsewarehouse.org/), an open repository of harmonized item response data.

Documentation for all available functions can be found at: <https://itemresponsewarehouse.github.io/Rpkg/reference/index.html>.

Project map: [`ARCHITECTURE.md`](https://github.com/ben-domingue/irw/blob/main/ARCHITECTURE.md) in `ben-domingue/irw` — which repo owns
what, where the data lives, and which document is authoritative when two disagree.

## Installation

``` r
# Install the development version of irw from GitHub
# install.packages("pak")  # if you don't already have pak
pak::pak("itemresponsewarehouse/Rpkg")

# Load the package
library(irw)
```

`pak` also installs [redivis](https://github.com/redivis/redivis-r), the client
for the warehouse itself. Installers that ignore the `Remotes:` field --- `R CMD
INSTALL`, a plain tarball, r-universe --- will not, and every function that
downloads a table then asks you to install it:

``` r
remotes::install_github("redivis/redivis-r", ref = "main")
```

The functions that do not touch the warehouse --- `irw_simdata()`,
`irw_simdata_comp()`, `irw_simu_diff()`, `irw_imv()`, `irw_predict()`,
`irw_long2resp()`, `irw_resp2long()`, `irw_check_resp()` and `irw_covariates()`
--- work without it.

## IMPORTANT: Redivis Authentication

The IRW tables are hosted on [Redivis](https://redivis.com), a data management platform. To access these datasets, you'll need to:

1.  Have a Redivis account (create one at <https://redivis.com/?createAccount> if you don't have one).

2.  Authenticate using the Redivis R Client:

    1.  When you first use a function in `irw` that connects to Redivis (e.g. `irw_info()`), a browser window will open, prompting you to sign in to your Redivis account.

    2.  After signing in, click **Allow** to grant access for the Redivis R Client.

    3.  Once authentication is successful, close the browser window. You will see the message “Authentication was successful” in the R console.

**Note:** You only need to authenticate once per session. For detailed instructions, refer to the [Redivis R Client documentation](https://apidocs.redivis.com/client-libraries/redivis-r/getting-started).

## Usage Examples

``` r
# Get information about the IRW
irw_info() 

# View list of available tables in IRW
irw_list_tables()

# Filter tables containing the variable `rt`
irw_filter(var = "rt")
```

``` r
# fetch an IRW table
x = irw_fetch("4thgrade_math_sirt") 
```

``` r
# summarize a table without downloading it: item and response value sets,
# and optionally per-item counts and response ranges
s = irw_table_sets("4thgrade_math_sirt", per_item = TRUE)
s$items
s$resp
```

``` r
# read the whole corpus as one IRW version held it: the reproducibility switch
irw_use_version(332)                                 # everything below is frozen
df <- irw_fetch("gilbert_meta_12")                    # v332's copy, today and next year

irw_use_version()                                    # freeze today; record the number
irw_reset_version()                                  # back to the current release
```

IRW is eleven Redivis datasets versioned independently, so no single Redivis
version describes the corpus. The IRW version number does: it increments
whenever any dataset is published and names one exact combination of the
eleven, so a paper can cite it and a reader can reproduce the run.

Put `irw_use_version()` at the top of an analysis script and every `irw_fetch()`
below it is pinned — including metadata and item text — so the script returns
the same data after IRW is corrected or extended. A table added later is an
error rather than a quiet fetch from the current release.

``` r
# look a version up without reading it
irw_version()                                        # newest IRW version + its pins
irw_version(version = 332)                           # exactly what v332 held
irw_version("2026-08-01")                            # what was live on a date

# or pin one dataset at a time
irw_get_version()                                    # versions currently in use
irw_set_version("item_response_warehouse", "v45.1")
```

**Cite the number, not the date.** Date lookups before 21 July 2026 are
approximate and say so — Redivis overwrote its own release timestamps for the
older warehouse shards during a platform migration, so those versions can only
be bracketed, and a date can resolve to the wrong version. IRW version numbers
themselves are always exact.

## Troubleshooting

#### Export Quota

`irw_fetch()` downloads every row of a table. Redivis caps the bytes an account
can export in a rolling 30-day window, and one pass over the whole IRW corpus
comes close to that cap on its own. If you only need to know which items or
response values a table contains, use `irw_table_sets()`: it answers with a
server-side query, returns in seconds even for tables with tens of millions of
rows, and does not count against the export quota.

If the quota is exhausted, `irw_fetch()` reports it as a quota error rather than
a missing table.

#### Authentication Issues

-   No Browser Pop-up: If the browser window doesn't open automatically, check your pop-up blocker settings.

-   Authentication errors: If you see an authentication error, try clearing your browser cookies and attempting again.

## Feedback and Contributions

If you encounter issues or have suggestions for improving `irw`, please submit them on the [GitHub Issues page](https://github.com/itemresponsewarehouse/Rpkg/issues). Contributions are welcome!

### Releasing

Releases are cut from the Actions tab, not by hand:

1.  Run **release-prepare.yaml** and choose `patch`, `minor` or `major`. It bumps `Version:` in `DESCRIPTION` and writes the `NEWS.md` entry from the pull requests merged since the last release tag, then opens a `Release irw X.Y.Z` pull request.
2.  Tidy the generated `NEWS.md` on that branch if the PR titles need editing, then merge it.
3.  **release-publish.yaml** sees the new version land on `main`, tags `vX.Y.Z`, and publishes a GitHub release using that `NEWS.md` section. The pkgdown site rebuilds on the published release.

Nothing else needs a version number, so `DESCRIPTION` and `NEWS.md` cannot drift apart. A hand-edited version bump merged to `main` still gets tagged and released; only the changelog would then be your problem.
