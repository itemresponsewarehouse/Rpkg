# irw 1.4.0

- The core, item text, conjoint and rankings shard lists are now a floor rather than the whole truth: once per session irw reads the Redivis organization catalog and includes any shard that continues a family it already knows about. A shard seeded after a release used to be invisible to every installed copy, and invisible *silently* — a shard absent from the spec list produces no loop iteration, so neither warning path could fire, and `irw_list_tables()`, `irw_metadata()` and `irw_filter()` just came back short. The seventh core warehouse released on 2026-10-15 with 761 tables and no released client could see any of them. Discovery extends a family but never invents one, so a genuinely new data family still errors as before; it adopts a shard only once that shard has a published version and is publicly readable; it appends and never reorders; and it needs no login. A discovered shard is pinnable by `irw_use_version()` like any other dataset. Switch it off with `options(irw.shard_discovery = FALSE)` or `IRW_SHARD_DISCOVERY=0` (ben-domingue/irw#3091).

- Reading IRW from R needs no Redivis account again. The warehouse is public, and `redivis` 0.12.14 and later send read requests anonymously (redivis/redivis-r#47) now that Redivis has opened the dataset-level endpoints too (redivis/redivis-r#49). `irw_info()`, `irw_list_tables()`, `irw_filter()`, `irw_fetch()`, `irw_itemtext()` and the metadata functions all work with no login. The non-interactive stop added in 1.3.1 to keep `R CMD check` off a browser prompt has moved from opening a dataset to issuing a query, so it now guards only `irw_table_sets()`, which runs server-side SQL and which Redivis still requires a login for; it also still guards every warehouse call on a `redivis` older than 0.12.14, naming the upgrade. R only: the Python package and the MCP server always authenticate, so they still need an account (itemresponsewarehouse/Rpkg#202).

- New data family: rankings (`source = "rank"`), tables in which each respondent places a fixed set of options in order (ranked-choice ballots, jury rankings, survey ranking questions). The layout keeps `id`/`item`/`resp`, but `resp` is a *position* (1 = first choice) as recorded: skipped positions are gaps and a candidate marked twice keeps both rows. `irw_fetch()`, `irw_list_tables()`, `irw_info()`, `irw_metadata()` (the design record per table: method, seats, positions offered, ...), `irw_filter()` (`license`), `irw_license_options()` and `irw_save_bibtex()` take `source = "rank"`; `irw_fetch(dedup = TRUE)` leaves these tables alone. New `irw_rank_compress()` renumbers positions 1..k within each ranking and drops an item's later marks, which is what a person-by-item pivot or a Plackett-Luce fit needs first. The source is a shard list (`irw_rank`, ...) like core and conjoint, from day one. The standard is `data/rankings/README.md` in the irw repository (ben-domingue/irw#3024).
- irw_itemtext_licence(): the licence on a table's item wording, for the minority of tables whose wording its source released under an open licence (CC BY-SA, CC BY-NC, ...). irw_itemtext() prints that licence, its source and IRW's changes after fetching such a table, so the licence travels with the data rather than living only on the Redivis page. The record is `itemtext/itemtext_licences.csv` in the IRW repository, fetched once per session.
- irw_filter(source = "conj") gains `domain` (what a conjoint study is about: `"immigration_asylum"`, `"candidate_choice"`, ... ; a table can carry more than one) and `choice_object` (what one profile represents: `"person"`, `"policy"`, ...). Both columns are in irw_metadata(source = "conj") from the irw_meta release of 2026-10-13; the vocabulary and the tagging rules are in the irw repo's `data/conjoint/README.md` (ben-domingue/irw#2956).

- The conjoint source (`source = "conj"`) is now a shard list like core and item text, because `irw_conjoint` is near Redivis' 1000-table cap. Every shard is opened and searched newest-first; an unreleased shard is skipped with a warning. irw_list_tables(source = "conj") warns when a table name is published in more than one shard instead of merging the copies silently. One shard is configured until the second has a published release, so nothing changes for users yet (ben-domingue/irw#1975 did the same for item text).
- Documentation: the `source` argument is described as selecting the *data family* (core, nominal, competitions, simsyn, conjoint), and conjoint is no longer called experimental.
- irw_save_bibtex(source = "conj"): citations for conjoint tables, from irw_meta's new `conj_biblio` table.
- irw_metadata(source = "conj") returns irw_meta's `conj_metadata` (design counts and design facts per experiment), and irw_info() on a conjoint table now shows its description, DOI, licence and reference. irw_filter(source = "conj") filters on `n_respondents`, `n_attributes`, `outcome` ("choice"/"rating"), `country` and `license`, and irw_license_options(source = "conj") lists its licences.

- The error for a missing `redivis` client now prints an install command that works where it is pasted. `getOption("repos")` is the unresolved placeholder `"@CRAN@"` in a session with no profile, so under `Rscript` the command we told people to run failed with "trying to use CRAN without setting a mirror"; the message now resolves the mirror itself (reported by Padma Korsepati).

- The seventh core warehouse (`item_response_warehouse_7`) is in the configured shard list. 1.3.1's list stopped at the sixth, so its table listings were short by that shard's 761 tables with nothing to say so (itemresponsewarehouse/Rpkg#197).
- `irw_info()` no longer reports the string `"NA"` as a table's source. A missing source note read back from the metadata as the two characters `NA` rather than as a missing value, and was printed as though it were a source (itemresponsewarehouse/Rpkg#200).

# irw 1.3.1

First CRAN release (accepted 2026-10-18).

Changes asked for in CRAN's review of the first submission:

- **Breaking:** irw_download() and irw_save_bibtex() no longer have a default output path; name the file with `path` / `output_file`. Nothing is written to the working directory by default.
- The table cache now defaults to `tools::R_user_dir("irw", "cache")` (e.g. `~/.cache/R/irw`) instead of `~/.cache/irw`, per CRAN policy. Tables cached by 1.3.0 are fetched again once. The Python package still defaults to `~/.cache/irw`; set `IRW_CACHE_DIR` for both to share one cache.
- Examples: the ones that need no network run as-is; the ones that query Redivis are in `\donttest{}` instead of `\dontrun{}`.
- `gnm` and `BradleyTerry2` added to Suggests (used by the irw_simdata_comp() example).
- Reading the warehouse needs a Redivis login (the README's 1.3.0-era "login optional" section was wrong and is reverted). Outside an interactive session, functions that read the warehouse now stop at once with instructions when no login is cached, instead of starting a browser login that waits forever. New irw_has_credentials() reports whether a login is available; the warehouse examples run only when it is TRUE.
- irw_simdata() and irw_simdata_comp(): `seed` no longer changes the caller's random number stream; it is restored on exit.

- irw_covariate_labels() and irw_covariates(labels = TRUE): the source's value labels for coded covariates, from irw_meta's new `covariate_labels` table; opt-in, irw_fetch() unchanged (ben-domingue/irw#1775)
- Docs: the exact-instrument collections (`phq9`, `gad7`, ...) and what their `basis` values mean, in `irw_collection_members()` and `irw_collections()` (ben-domingue/irw#1712)
- irw_fetch(): once-per-session credit note for tables found via openESM and similar collections (ben-domingue/irw#2421)
- irw_save_bibtex(): also returns the openESM citation for tables found via openESM (ben-domingue/irw#2421)

# irw 1.3.0

- On-disk table cache, shared with the Python package (#170)

# irw 1.2.2

- .retry_with_backoff(): retry transient failures on small reads (#167)

# irw 1.2.1

- irw_filter(): skip the density default for sources without a density column (#165)
- irw_table_sets(): per_item is NULL when the table has no resp column (#164)
- irw_imv(): call imv::imv.binary() instead of a private copy (#163)

# irw 1.2.0

- Show custom licence terms in irw_info() and flag "Custom" in irw_license_options() (#159)
- irw_table_sets(): accept several table names, like irw_fetch() (#158)
- warehouses.md: adding a warehouse is a three-repo change (irw#1733) (#157)

# irw 1.1.3

- Untrack irw.Rcheck/, and fix the ignore rule that was meant to stop it (#152)
- Add irw_text_2 as the second item-text shard (#151)
- Make item text a shard list, ahead of the 1000-table cap (#150)
- Address metadata tables by name; the pinned reference ids are dead (#148)
- irw_use_version(): read the whole corpus at one IRW version (#146)
- README: document irw_version() alongside the pinning functions (#145)
- irw_version(): one number for the whole corpus, and as-of-date lookup (#144)

# irw 1.1.2

- Drop release PRs from the generated NEWS.md entry (#142)
- Move redivis to Suggests and check for it at runtime (#141)

# irw 1.1.1

- Compare release timestamps in UTC so NEWS.md skips released PRs (#139)
- Strip the arrow namespace reference from swmd_mokken.rda (#138)
- Install libcurl headers so redivis builds on Ubuntu (#137)
- Don't fail release-prepare when Actions can't open the PR (#136)

# irw 1.1.0

## New features

- `irw_imv()` computes the InterModel Vigorish, and `irw_predict()` returns
  per-response predicted probabilities from a fitted `mirt` model. Together
  they support out-of-sample model comparison, walked through in the new
  "Comparing Models Out of Sample with the IMV" article (#131, #130).
- `irw_set_version()`, `irw_get_version()` and `irw_reset_version()` pin IRW
  data to a Redivis version, so an analysis can be reproduced against the data
  it was written for (#128).
- `irw_collections()`, `irw_collection()` and `irw_collection_members()` expose
  the curated groupings of IRW tables (#125).
- `irw_recode()`, `irw_recode_key()` and `irw_decode()` swap long identifiers
  for keyboard-friendly ones and back again (#123).
- `irw_table_sets()` summarizes the items and response values in a table with a
  server-side query, returning in seconds and without spending export quota.
  Exhausted quota is now reported as a quota error rather than a missing table
  (#121).

## Changed behavior

- `irw_simdata()` draws default discriminations for `model = "2PL"` and
  `"3PL"` from lognormal(0, 0.5) rather than lognormal(0.2, 0.2). The old
  spread put the middle half of items between 0.95 and 1.25, which is a Rasch
  model in all but name; the new one keeps the median at 1 and widens that
  range to 0.7-1.4. **Simulated data changes for a fixed seed** when `a` is not
  supplied — pass `a = rlnorm(n_item, 0.2, 0.2)` for the old behavior (#132).

## Documentation and infrastructure

- New article pairing IRW data with `mirt`, `ltm`, `psychotools` and other
  psychometrics packages (#124), and a pointer to the IRW project map (#127).
- Item text table names now resolve case-insensitively (#122).
- Tag splitting is unified on one parser (#126).
- Collections table references re-pinned to current Redivis IDs (#129).
- Releases are cut from the Actions tab: a workflow bumps the version, writes
  this file and opens a release PR, and merging it tags and publishes
  (#133, #134).
