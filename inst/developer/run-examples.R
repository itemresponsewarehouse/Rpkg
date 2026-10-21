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
# Usage: IRW_RUN_EXAMPLES=1 Rscript inst/developer/run-examples.R [man/x.Rd ...]

args <- commandArgs(trailingOnly = TRUE)
if (!nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))) {
  stop("Set IRW_RUN_EXAMPLES=1: without it every example's own gate skips it.")
}

files <- if (length(args)) args else list.files("man", pattern = "[.]Rd$", full.names = TRUE)
if (!length(files)) stop("No Rd files found; run this from the package root.")

# A page still carrying the old gate would skip silently while every other
# page ran, and the job would stay green on partial coverage -- the same
# failure shape as the gate itself. Catch it as a gate problem, by name,
# rather than letting it hide in a pass.
stale <- Filter(
  function(rd) {
    lines <- grep("irw_has_credentials()", readLines(rd, warn = FALSE), fixed = TRUE, value = TRUE)
    length(lines) > 0 && !any(grepl("IRW_RUN_EXAMPLES", lines, fixed = TRUE))
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

library(irw)

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
    error = function(e) conditionMessage(e)
  )
  ran <- c(ran, basename(rd))
  if (!identical(result, "ok")) {
    failed[[basename(rd)]] <- result
  }
}

cat("\n--- examples run:", length(ran), "pages,", length(failed), "failed ---\n")
if (length(failed)) {
  for (nm in names(failed)) cat(" *", nm, ":", failed[[nm]], "\n")
  stop("Example failures: ", paste(names(failed), collapse = ", "), call. = FALSE)
}
# A green run that executed nothing is the failure this guards against: it is
# what the gate did on CRAN, and what a commented-out \donttest would do here.
if (!length(ran)) stop("No examples executed.", call. = FALSE)
