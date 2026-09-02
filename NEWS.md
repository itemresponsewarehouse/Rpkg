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
