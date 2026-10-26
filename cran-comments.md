## Submission

This is an update to irw 1.3.1, our first CRAN release, accepted on 2026-10-18.
It is back sooner than the one-to-two months the policy asks for, and the
reason is a correction that makes the package report incomplete results
silently. irw has no reverse dependencies, so no other package needs
re-checking on account of it.

* **The list of data sources the package reads was out of date, and a source
  missing from that list is not reported as missing — it is not read at all.**
  The Item Response Warehouse is held in several Redivis datasets, and 1.3.1
  carries a hardcoded list of them that stops at the sixth. A seventh was
  published on 2026-10-15, days before 1.3.1 was accepted, and an eighth on
  2026-10-21. A dataset absent from the list produces no loop iteration, so
  there is no code path on which a warning could be issued: irw_list_tables(),
  irw_metadata() and irw_filter() simply came back short by 1,031 tables, 15.3%
  of the warehouse (961 in the seventh and 70 in the eighth, counted
  2026-10-26), with nothing to indicate it. New tables are published into
  the newest dataset, so the gap grows rather than holds.

  This version lists both, and stops the problem recurring: the list is now a
  floor rather than the whole truth, and the package reads the warehouse's
  public catalog once per session and includes any dataset that continues a
  family it already knows about. Discovery extends a family and never invents
  one, adopts a dataset only once it has a published version and is publicly
  readable, appends and never reorders, needs no login, and can be switched off
  with options(irw.shard_discovery = FALSE).

* **Four changes to returned values are breaking, and each replaces a silent
  wrong answer with a correct one or an error.** They are listed first in
  NEWS.md with the reasoning and the migration for each. In short:
  irw_long2resp() no longer averages across a repeated-measures column it was
  not told to aggregate (it stops, and names the two ways forward); the default
  density filter in irw_filter() no longer excludes every repeated-measures
  table by capping a ratio that is not bounded by 1; and irw_long2resp()'s item
  columns now come back in a defined order and under the warehouse's own item
  ids rather than in input-row order under a prefixed name. No reverse
  dependencies are affected, and the sibling Python client was changed in step
  so the two return the same thing for the same table.

* **Reading the warehouse no longer requires an account, and the README says so
  again.** In the review of 1.3.0 I claimed reading was possible without a
  login; that was wrong at the time, and the claim was removed for 1.3.1, which
  stops with instructions when no credentials are cached. It has since become
  true for an upstream reason: the `redivis` client stopped attaching a token to
  every request (redivis/redivis-r#47) and Redivis opened its dataset-level
  endpoints to anonymous reads (redivis/redivis-r#49). The guard that 1.3.1
  added to keep R CMD check away from a browser login has therefore moved from
  opening a dataset to issuing a query, which Redivis does still require a login
  for, and it stays in place for `redivis` clients older than 0.12.14.

* **Nothing in this submission downloads data on your machines.** The warehouse
  examples are inside \donttest{} and gated on
  `irw_has_credentials() || nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))`. R CMD check
  --as-cran runs \donttest examples, so the gate rather than the wrapper is what
  skips them; CRAN sets neither credentials nor that variable. We run them
  ourselves in continuous integration, in a job with no login, which is the
  environment they would run in on your machines.

* New features, none of which affect the check: a rankings data family
  (irw_fetch(source = "rank"), irw_rank_compress()), the conjoint family
  promoted out of experimental with its filters, metadata and citations,
  irw_itemtext_licence(), and max_rows/columns on irw_fetch(). See NEWS.md.

* Four maintainer notes under inst/developer/ shipped in 1.3.1 by oversight and
  are excluded from the build now.

## R CMD check results

0 errors | 0 warnings | 1 note

* The note is the one 1.3.1 carried: a Suggested package not in a mainstream
  repository, below.

## Suggested package not in mainstream repositories

`redivis` (the client for the Redivis platform that hosts the Item Response
Warehouse) is in Suggests and is available from r-universe, declared in
`Additional_repositories: https://redivis.r-universe.dev`. Every use is guarded
by requireNamespace(), so the package checks cleanly without it, and is checked
that way in continuous integration on every commit.
