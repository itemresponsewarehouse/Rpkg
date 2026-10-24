## Submission

This is an update to irw 1.3.1, which was accepted on 2026-10-18. The main
reason for it is a correction, described first.

* **The list of data sources the package reads was out of date, and that made
  the package report incomplete results silently.** The Item Response Warehouse
  is held in several Redivis datasets, and 1.3.1 carries a hardcoded list of
  them. A seventh was published on 2026-10-15, shortly before 1.3.1 was
  accepted, and a dataset absent from that list is not opened at all, so no
  warning could be issued: irw_list_tables(), irw_metadata() and irw_filter()
  simply returned about 12% fewer tables than the warehouse holds, with nothing
  to indicate it. This version adds the seventh dataset and, more importantly,
  stops the problem recurring: the list is now a floor, and the package reads
  the warehouse's public catalog once per session and includes any dataset that
  continues a family it already knows about. It extends a family and never
  invents one, appends and never reorders, and can be switched off with
  options(irw.shard_discovery = FALSE).

* **Reading the warehouse no longer requires an account, and the README says so
  again.** In the review of 1.3.0 I claimed reading was possible without a
  login; that was wrong at the time, and the claim was removed for 1.3.1, which
  stops with instructions when no credentials are cached. It has since become
  true for an upstream reason: the `redivis` client stopped attaching a token to
  every request (redivis/redivis-r#47) and Redivis opened its dataset-level
  endpoints to anonymous reads (redivis/redivis-r#49). The guard that 1.3.1
  added to keep R CMD check away from a browser login has therefore moved from
  opening a dataset to issuing a query, which Redivis does still require a login
  for, and it stays in place for `redivis` clients older than 0.12.14. The
  warehouse examples remain guarded by `@examplesIf irw_has_credentials()`
  inside \donttest{}, so nothing in this submission downloads data on your
  machines.

* New features, none of which affect the check: a rankings data family
  (irw_fetch(source = "rank"), irw_rank_compress()), the conjoint family
  promoted out of experimental with its filters, metadata and citations, and
  irw_itemtext_licence(). See NEWS.md.

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
