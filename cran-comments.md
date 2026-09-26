## New submission

This is a first submission of irw.

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Suggested package not in mainstream repositories

`redivis` (the client for the Redivis platform that hosts the Item Response
Warehouse) is in Suggests and is available from r-universe, declared in
`Additional_repositories: https://redivis.r-universe.dev`. Every use is guarded
by `requireNamespace()`, and examples that download data are not run, so the
package checks cleanly without it.
