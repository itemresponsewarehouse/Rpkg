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
