## Resubmission

This is a resubmission. In this version I have:

* Removed the redundant "Tools for" from the start of the Description.

* Replaced \dontrun{} with \donttest{} in every example that queries the
  Item Response Warehouse over the internet (these need the Suggested
  `redivis` package and are guarded by requireNamespace()), and unwrapped the
  examples that run locally in under 5 seconds.

* Removed the default output paths: irw_download() now requires `path` and
  irw_save_bibtex() requires `output_file`, so neither writes to the working
  directory by default. Examples write only to tempdir().

* The examples that query the warehouse need a Redivis login, which a CRAN
  machine does not have. Without one, the redivis client would start a
  browser login and wait for it. irw now checks for credentials before the
  client is touched: outside an interactive session it stops at once with
  instructions, and these examples are wrapped in
  `@examplesIf irw_has_credentials()` (new exported function) inside
  \donttest{}, so on CRAN they are skipped instead of hanging. Checked with
  --as-cran in three settings: no credentials, redivis not installed, and
  credentials present (examples run, about 4.5 minutes in total).

* irw_simdata() and irw_simdata_comp() restore the user's .Random.seed after
  a seeded call instead of changing the global random number stream.

* Moved the on-disk cache of downloaded tables from ~/.cache/irw to
  tools::R_user_dir("irw", "cache"). The cache can be switched off with
  options(irw.cache = FALSE) and emptied with irw_clear_cache(); a table's
  outdated copies are removed when a newer one is cached.

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Suggested package not in mainstream repositories

`redivis` (the client for the Redivis platform that hosts the Item Response
Warehouse) is in Suggests and is available from r-universe, declared in
`Additional_repositories: https://redivis.r-universe.dev`. Every use is guarded
by requireNamespace(), so the package checks cleanly without it.
