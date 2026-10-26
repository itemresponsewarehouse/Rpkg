# irw (development version)

- **Breaking (column names):** `irw_long2resp()` names each item column by the item id itself — `q1`, `item10`, `1` — instead of prefixing it with `item_`. The item id is the key that joins a wide matrix back to the item text, to `irw_itemtext()` and to anything else keyed by item, so every such merge previously needed a strip step, and on a table whose items are already called `item10` the column came back as `item_item10` — two strips that are not the same one. The Python client already returned the ids, so the same table gave the two clients different column names, and a client-specific column name is a client-specific join (ben-domingue/irw#3221). The prefix is still applied internally, unconditionally, so that a numeric item id does not become a numeric column label mid-reshape and so that items named `a` and `item_a` cannot collide; it is mapped back rather than stripped, which is what keeps `myitem_x` from returning as `myx`. Two consequences for callers: a numeric item id now yields a column named by that number, which needs backticks in a formula (`` `1` ``), and `irw_check_resp()` reports the item names the caller passed or will see rather than prefixed ones. `irw_decode()` still understands an `item_` prefix, so a matrix built by an older version of this package decodes as before.

- **Breaking (column order):** `irw_long2resp()` returns the item columns in natural order — `item1`, `item2`, `item10` — instead of the order they happened to appear in the input rows. `stats::reshape()` orders columns by first appearance, and Redivis does not guarantee row order, so the same table fetched twice could hand back its columns differently, with no error; a caller feeding the matrix into IRT code by column position had no way to notice. The Python client sorted already but lexicographically, which puts `item10` before `item2`, so both clients now share one key definition (digit runs compare as numbers, everything else as text, case-insensitively) and return the same order for the same table (ben-domingue/irw#3221).

- `redivis`'s advice to pin a qualified reference id is no longer printed. It is correct advice in general and wrong for this package: a reference id names one *version's* table, and a release cut rotates them — the 2026-09-04 `irw_meta` cut minted new ids for all thirteen tables at once — so there is nothing a caller can do with it. It was emitted once per dataset and once per table, so a single `irw_filter()` across the core shards produced dozens of them, which trains people to ignore warnings from this package including the ones that matter, like the shard-list and duplicate-name warnings beside it. Only that exact message is muffled, and only inside this package's own Redivis calls; every other `redivis` warning passes through. `irw_fetch()` already suppressed the table half of it and now suppresses the dataset half too (review P3, ben-domingue/irw#3229).

- **Breaking (default):** `irw_filter()`'s default `density` filter no longer caps at 1. `density` is **responses per person-item cell** -- `n_responses / (n_participants * n_items)` -- not the proportion of cells that are filled, so it is not bounded by 1: a table that measures a person-item pair more than once is legitimately above it. 768 of 6,720 core tables are above 1 (measured 2026-10-25), 700 of them carrying an occasion column (`wave`, `rater`, `trial_*`, `session`, ...), 122 carrying `date`, the largest 3,624.6 for a go/no-go task -- and the old default of `c(0.5, 1)` silently dropped every one of them, so no unfiltered `irw_filter()` call could return a repeated-measures table. The default is now `c(0.5, Inf)`: a lower bound only, which is what "exclude sparse matrices" was ever meant to do. Pass `density = c(0.5, 1)` for the old behaviour (ben-domingue/irw#3222).

- **Breaking:** irw_long2resp() no longer aggregates across an occasion column without being told to. The IRW standard defines nine ways a response can repeat and this reshape handled one: `wave` had a selector, while `rater`, `trial_*`, `session`, `order`, `period`, `block`, `subtest` and `timepoint` were dropped from the frame and then averaged together by the default `agg_method = "mean"` — so a table scored by several raters came back with cells that were the mean of raters who disagreed, a value no rater gave, and there was no argument a caller could pass to prevent it. New `occasion` argument chooses one, for any of those columns: `occasion = c(rater = "2")`, matched as a string exactly as `wave` is. When such a column keys a repeated `id`-`item` pair and you have named neither it nor an `agg_method`, the function now **stops** and names both ways forward rather than choosing an occasion for you; passing `agg_method` explicitly says that aggregating across it is what you want, and the message then names the column being pooled. The occasions kept are recorded on the result as the `"occasion"` attribute and any pooled as `"occasion_pooled"`, because a caller may have captured the messages and the MCP server discards them (ben-domingue/irw#3223).

- irw_fetch() gains `max_rows` and `columns`, which the Python client has had and R has not. Both are pushed to Redivis, so only the rows and columns asked for are exported and counted against the account's quota: `irw_fetch("neurips_2020", max_rows = 100)` transfers a page rather than 19.8 million rows, and 326 IRW tables are over a million. A bounded fetch is never written to the disk cache, because a slice cannot stand in for the table; a cached whole table does serve a slice, with no export at all. A column the table does not have is an error naming the table, raised before the download rather than mistaken for a missing table by the shard search (ben-domingue/irw#3221).

- irw_filter() now rejects a non-numeric value for a numeric filter instead of reporting no matches. `irw_filter(n_responses = "abc")` compared a numeric column against a string, which R resolves lexicographically, so the answer was "0 tables matched" -- a type error wearing the clothes of a finding about the corpus. The Python client raises for the same call. Checked before any metadata is fetched, and the same check now covers the conjoint and competition filters, which had a message of their own (ben-domingue/irw#3221).

- irw_filter() gains four psychometric filters for `source = "core"`: `difficulty_spread` (the share of a table's observed response range that its item means cover — the p-value range on a dichotomous test, normalised so a 1–5 Likert table is comparable), `mean_item_rest_r`, `mean_abs_item_rest_r` and `prop_items_extreme` (the share of items at floor or ceiling). Until now the catalogue could only be filtered by shape — how many responses, items, participants — so "tables whose items hang together, spanning the scale, with no floor effects" was not an askable question. The columns are classical and model-free, with no IRT model fitted anywhere, so they are defined for every core table rather than the dichotomous sixth, and they take no position on what the items measure. Two cautions documented on the arguments: response direction is not harmonised across IRW, so a signed mean item-rest correlation near zero can mean a reverse-keyed scale rather than an incoherent one (compare it with the absolute version, whose gap from the signed one is the tell), and rest scores are a weaker statement about a sparse bank, where respondents see almost disjoint item sets, than about a 20-item questionnaire. The per-item rows behind the summaries are irw_meta's new `item_classical` table, one row per table-item. Requires the irw_meta release of 2026-10-21 or later (ben-domingue/irw#1710).

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
