# Releasing: when, and what waits

This note is for package maintainers only (not included in the user-facing README).
Companion to `warehouses.md`, which covers what to edit when a shard is added.

The decision of record is [ben-domingue/irw#3091](https://github.com/ben-domingue/irw/issues/3091),
because the policy binds three repositories and this is only one of them. What
follows is the operational half: the dates, the triggers, and the checks that
make them fire without anyone remembering to look.

## The schedule

**Four CRAN submissions a year: mid-March, mid-June, mid-September,
mid-December**, beginning with 1.4.0 in early November 2026.

Three months is comfortably inside CRAN's norm — the
[repository policy](https://cran.r-project.org/web/packages/policies.html) says
that for an established package "no more than every 1–2 months seems
appropriate" — which leaves room to spend on an off-cycle fix without the next
scheduled cut reading as churn.

The months are not arbitrary quarters. R cuts `x.y.0` every April, and that is
the event that can archive a package: a package whose `R CMD check` ERRORs on a
new `x.y.0` "will be archived (or in exceptional circumstances updated by the
CRAN team)". A mid-March submission therefore puts fresh code in front of CRAN
a few weeks before the release that matters, and the three other dates fall out
of spacing that evenly.

**PyPI stays continuous** — release on merge of anything user-visible. PyPI
imposes no frequency norm, and the Python client can track the warehouse
closely in a way the R one cannot. One rule couples them: a Python release that
changes *corpus reach* (a new shard, a new family) does not go out unless the
matching R change is on `main` and queued for the next CRAN cut. Otherwise the
README's note about what CRAN lacks becomes a stale apology rather than a true
statement.

The two packages do not release together. Lock-step would mean holding Python
work for up to three months to keep one feature set described in one place, and
the README note is a cheaper way to buy the same honesty.

## What forces an off-cycle submission

Two things, deliberately not more.

1. **A CRAN check-failure email.** Respond within two weeks, and say that date
   back to CRAN. The policy sets no fixed deadline for fixing a check problem,
   but it spares a package from archival only "unless the maintainer has set a
   firm deadline for an upcoming update (and keeps to it)". Archival is far more
   expensive to recover from than a release is to postpone, so this is the one
   trigger that outranks everything.
2. **A change where the installed version returns something *wrong*** rather
   than merely incomplete. The test is "returns something wrong", not "is
   annoying" — a function that errors, or that stops with instructions, is loud,
   and loud waits for the cycle. Numbers are what travel: a short table count
   goes into a methods section and nothing in the chain flags it.

Everything else waits. A missing data family is an unknown-`source` error and
the README covers it; anonymous reading was blocked for a release and that
waited; a missing shard used to belong on this list and no longer does, because
shard discovery (`R/shard-discovery.R`, Rpkg#205) means a released client finds
one on its own and says so once.

## The checks that make this run without remembering

- `.github/workflows/R-CMD-check.yaml` — every push to `main` and every PR,
  five platforms, plus the no-credential and no-redivis jobs.
- `.github/workflows/r-devel-weekly.yaml` — R-devel, weekly, hard dependencies
  only, because that is the configuration CRAN checks and the one whose failure
  can archive us. A quarterly cadence otherwise leaves ten-week stretches with
  no devel signal, which is the window of greatest exposure. A death in
  dependency setup reports green with a job summary rather than mailing a weekly
  failure about somebody else's binary snapshot; only a real check failure goes
  red. The header of that file explains why in more detail.
- `metadata/check_config_parity.py` in `ben-domingue/irw` — every PR there,
  offline: do the three config files agree with each other.
- `metadata/check_client_reach.py` in `ben-domingue/irw` — daily, through the
  drift report: does the *released* package on CRAN, and the one on PyPI, reach
  every live shard. All three configs can agree and all three be behind the
  warehouse, which is what happened in October 2026.

## Mechanics of a cut

Open one `usethis::use_release_issue()` checklist per CRAN submission and work
it. In addition:

- `cran-comments.md` should say plainly what the release corrects. For 1.4.0
  that is the out-of-date data-source list that left the corpus listing
  incomplete, and the return of anonymous reading — a reviewer who saw the
  1.3.0-era "login optional" claim reverted will notice it coming back, and the
  honest framing is that the claim was wrong then for an upstream reason since
  fixed.
- Bump the version in `DESCRIPTION`, and keep `NEWS.md` current as you go rather
  than writing it at the end.
- The tag is cut from the submitted state. Note that `main` keeps moving while
  CRAN reviews — 1.3.1 was tagged on 10-07 and accepted on 10-18, by which time
  `main` was 29 commits ahead — so the gap between CRAN and `main` on acceptance
  day is normal, and the README's dev-versus-CRAN list is how users learn about
  it. Delete that list once the release that closes the gap ships.
