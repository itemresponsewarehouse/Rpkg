#!/usr/bin/env Rscript
# Run the warehouse examples here rather than on CRAN's machines (Rpkg#208).
#
# The gate on each example stays exactly as a user sees it --
# `irw_has_credentials() || nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))` -- and this
# runner satisfies it with the environment variable rather than stripping it.
# That distinction is the point: the condition still means what it says, CRAN
# still skips (it sets no such variable), and the code that runs here is the
# code a reader of the manual runs. Reading IRW needs no account since #202,
# so these run anonymously.
#
# `tools::Rd2ex` is used only to turn each page into a script. `\dontrun`
# stays commented, `\donttest` does not -- the warehouse examples are all
# inside `\donttest`, so commenting it would run nothing and pass.
#
# Before that, a second pass runs every page the way CRAN does -- gate closed,
# no credentials, no IRW_RUN_EXAMPLES, `\donttest` run as `--as-cran` runs it
# -- with the network client packages irw calls replaced by tripwires. A page
# that reaches the network there has an example with no gate at all, which
# both the matrix jobs and the open pass below would run and pass, and which
# CRAN would then run on every flavor (Rpkg#214). That pass fails naming the
# page. It tests what the gate does rather than how it is spelled, so the
# `@examplesIf` form roxygen writes into \dontshow and an inline
# `if (...) { ... }` (man/irw_covariates.Rd) are the same to it, and it needs
# no list of networked entry points: whatever an example calls, the request
# has to leave through one of the client packages.
#
# Usage: IRW_RUN_EXAMPLES=1 Rscript inst/developer/run-examples.R [man/x.Rd ...]
#   (`--closed-gate` as the first argument runs only the closed pass; the
#   open pass starts it itself, in a fresh process with the gate shut.)

args <- commandArgs(trailingOnly = TRUE)
closed_gate <- length(args) > 0 && identical(args[[1]], "--closed-gate")
if (closed_gate) args <- args[-1]

if (!closed_gate && !nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))) {
  stop("Set IRW_RUN_EXAMPLES=1: without it every example's own gate skips it.")
}

files <- if (length(args)) args else list.files("man", pattern = "[.]Rd$", full.names = TRUE)
if (!length(files)) stop("No Rd files found; run this from the package root.")

# Turn each page into a script and source it, one page at a time. `on_page`
# runs after each page and may return a failure message of its own.
run_pages <- function(files, on_page = function(page) NULL) {
  dir <- tempfile("irw-examples-")
  dir.create(dir)
  ran <- character(0)
  failed <- list()
  for (rd in sort(files)) {
    script <- file.path(dir, sub("[.]Rd$", ".R", basename(rd)))
    tools::Rd2ex(rd, out = script, commentDontrun = TRUE, commentDonttest = FALSE)
    # Pages with no \examples section produce no file at all.
    if (!file.exists(script)) next

    cat("\n== ", basename(rd), " ==\n", sep = "")
    result <- tryCatch(
      {
        source(script, echo = TRUE, max.deparse.length = 500)
        "ok"
      },
      # The message alone is not always enough: the first real failure here came
      # back with an empty one, which named the page and said nothing about it.
      error = function(e) {
        msg <- conditionMessage(e)
        call <- if (is.null(conditionCall(e))) "" else paste(deparse(conditionCall(e)), collapse = " ")
        paste0(
          "<", paste(class(e), collapse = "/"), "> ",
          if (nzchar(msg)) msg else "(no message)",
          if (nzchar(call)) paste0(" [in: ", substr(call, 1, 200), "]") else ""
        )
      }
    )
    extra <- on_page(basename(rd))
    if (!is.null(extra)) result <- extra
    ran <- c(ran, basename(rd))
    if (!identical(result, "ok")) {
      failed[[basename(rd)]] <- result
    }
  }
  list(ran = ran, failed = failed)
}

if (closed_gate) {
  # ---- The closed pass: what CRAN runs, with the network shut. ----
  if (nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))) {
    stop("The closed pass must run without IRW_RUN_EXAMPLES set.", call. = FALSE)
  }
  library(irw)
  # A closed pass with credentials on hand would open every gate and prove
  # nothing. The caller hides the token and HOME; the redivis client finds its
  # credentials file by its own route rather than through HOME, so point it at
  # nothing as well (this process only). Then make sure it worked.
  if (requireNamespace("redivis", quietly = TRUE)) {
    auth_vars <- tryCatch(get("auth_vars", envir = asNamespace("redivis")), error = function(e) NULL)
    if (is.environment(auth_vars)) {
      auth_vars$credentials_file <- file.path(tempdir(), "no-redivis-credentials")
      auth_vars$cached_credentials <- NULL
    }
  }
  if (irw_has_credentials()) {
    stop("The closed pass found Redivis credentials, so the gates are open.", call. = FALSE)
  }

  # The network clients are derived from the package's own code: every
  # `pkg::` it calls that is an HTTP client. Adopting another client (httr2,
  # say) puts it under the tripwire with no edit here.
  http_clients <- c("redivis", "httr", "httr2", "curl", "RCurl", "crul")
  code <- unlist(lapply(list.files("R", pattern = "[.][Rr]$", full.names = TRUE), readLines, warn = FALSE))
  called <- unique(sub("::$", "", unlist(regmatches(code, gregexpr("[A-Za-z][A-Za-z0-9.]*::", code)))))
  clients <- intersect(http_clients, called)
  if (!"httr" %in% clients) {
    stop("Found no `httr::` call in R/; is this the package root?", call. = FALSE)
  }

  # Every exported binding of each client becomes a tripwire that records the
  # page it fired on and then errors. Recording matters: irw catches some
  # network errors and carries on (irw_itemtext_licence() warns and returns
  # NULL), so an error alone could be swallowed and the page would look clean.
  # `pkg::name` looks the binding up in the namespace at call time, which is
  # how every call irw makes reaches these.
  hits <- new.env()
  hits$calls <- character(0)
  trip <- function(what) {
    force(what)
    function(...) {
      hits$calls <- c(hits$calls, what)
      stop("network call with the example gate closed: ", what, call. = FALSE)
    }
  }
  # Non-function exports (`redivis::redivis`, the client object) are replaced
  # by an object whose every `$`/`[[` trips.
  registerS3method("$", "irw_tripwire", function(x, name) trip(paste0(attr(x, "what"), "$", name))(), envir = baseenv())
  registerS3method("[[", "irw_tripwire", function(x, i, ...) trip(paste0(attr(x, "what"), "[[", i))(), envir = baseenv())
  wired <- character(0)
  for (pkg in clients) {
    if (!requireNamespace(pkg, quietly = TRUE)) next
    ns <- asNamespace(pkg)
    for (name in getNamespaceExports(pkg)) {
      if (startsWith(name, ".__") || !exists(name, envir = ns, inherits = FALSE)) next
      obj <- get(name, envir = ns, inherits = FALSE)
      what <- paste0(pkg, "::", name)
      repl <- if (is.function(obj)) trip(what) else structure(list(), class = "irw_tripwire", what = what)
      locked <- bindingIsLocked(name, ns)
      if (locked) unlockBinding(name, ns)
      assign(name, repl, envir = ns)
      if (locked) lockBinding(name, ns)
    }
    wired <- c(wired, pkg)
  }
  cat("Closed pass: tripwires on", paste(wired, collapse = ", "), "\n")

  res <- run_pages(files, on_page = function(page) {
    calls <- unique(hits$calls)
    hits$calls <- character(0)
    if (length(calls)) {
      paste("reached the network with the gate closed, via", paste(calls, collapse = ", "))
    }
  })

  cat("\n--- closed pass:", length(res$ran), "pages,", length(res$failed), "failed ---\n")
  if (length(res$failed)) {
    for (nm in names(res$failed)) cat(" *", nm, ":", res$failed[[nm]], "\n")
    stop(
      "These pages fail as CRAN would run them -- gate closed, no credentials. ",
      "One that reached the network has an example with no gate: wrap it in ",
      "`@examplesIf irw_has_credentials() || nzchar(Sys.getenv(\"IRW_RUN_EXAMPLES\"))` ",
      "and re-document: ",
      paste(names(res$failed), collapse = ", "),
      call. = FALSE
    )
  }
  if (!length(res$ran)) stop("The closed pass executed no examples.", call. = FALSE)
  quit(save = "no", status = 0)
}

# The one page that cannot run here, and why. `irw_table_sets()` answers with a
# server-side query, and a query needs a Redivis login even though reading a
# table does not -- tests/testthat/test-anonymous-read.R pins that as the one
# remaining path that must ask for one. Its example therefore keeps the
# credentials-only gate. This list is deliberately explicit rather than a
# pattern: an exception should have to be named and justified, because every
# name on it is a page nothing runs.
credentials_only <- c("irw_table_sets.Rd")

# A page still carrying the old gate would skip silently while every other
# page ran, and the job would stay green on partial coverage -- the same
# failure shape as the gate itself. Catch it as a gate problem, by name,
# rather than letting it hide in a pass.
# A gate, not a mention: `if (` ... `irw_has_credentials()`. That matches both
# forms in use -- the `@examplesIf` condition roxygen writes into \dontshow,
# and an inline `if (irw_has_credentials()) { ... }` around one snippet, which
# is how man/irw_covariates.Rd hid a warehouse example from this runner. It
# does not match a bare call, so the page that documents
# `irw_has_credentials()` is not flagged for calling it.
stale <- Filter(
  function(rd) {
    if (basename(rd) %in% credentials_only) return(FALSE)
    lines <- grep("if *\\(.*irw_has_credentials\\(\\)", readLines(rd, warn = FALSE), value = TRUE)
    length(lines) > 0 && !all(grepl("IRW_RUN_EXAMPLES", lines, fixed = TRUE))
  },
  files
)
if (length(stale)) {
  stop(
    "These pages gate examples on credentials alone, so this runner cannot ",
    "reach them. Widen the @examplesIf to `irw_has_credentials() || ",
    "nzchar(Sys.getenv(\"IRW_RUN_EXAMPLES\"))` and re-document: ",
    paste(basename(stale), collapse = ", "),
    call. = FALSE
  )
}

# ---- The closed pass, in a fresh process with the gate shut. ----
# No IRW_RUN_EXAMPLES, no token, and a HOME and cache with no saved login or
# cached tables in them, so a table already on disk cannot hide a download.
self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
scratch <- tempfile("irw-closed-")
dir.create(scratch)
closed_env <- c(
  IRW_RUN_EXAMPLES = "",
  REDIVIS_API_TOKEN = "",
  HOME = scratch,
  R_USER_CACHE_DIR = file.path(scratch, "cache"),
  R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)
)
saved <- Sys.getenv(names(closed_env), unset = NA, names = TRUE)
do.call(Sys.setenv, as.list(closed_env))
status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(self), "--closed-gate", shQuote(files)))
for (nm in names(saved)) {
  if (is.na(saved[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, stats::setNames(list(saved[[nm]]), nm))
}
if (!identical(status, 0L)) {
  stop("The closed-gate pass failed; see the pages it names above.", call. = FALSE)
}

# ---- The open pass: every gate satisfied by IRW_RUN_EXAMPLES. ----
library(irw)

res <- run_pages(files)
ran <- res$ran
failed <- res$failed

cat("\n--- examples run:", length(ran), "pages,", length(failed), "failed ---\n")
if (length(failed)) {
  for (nm in names(failed)) cat(" *", nm, ":", failed[[nm]], "\n")
  stop("Example failures: ", paste(names(failed), collapse = ", "), call. = FALSE)
}
# A green run that executed nothing is the failure this guards against: it is
# what the gate did on CRAN, and what a commented-out \donttest would do here.
if (!length(ran)) stop("No examples executed.", call. = FALSE)
