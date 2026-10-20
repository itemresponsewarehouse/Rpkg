# End-to-end proof that reading IRW needs no Redivis account
# (itemresponsewarehouse/Rpkg#202, redivis/redivis-r#49).
#
# Opt-in, because it is the one test in the suite that reaches the network:
# set IRW_TEST_ANONYMOUS=true. The R-CMD-check workflow runs it in a job of
# its own, where there are no credentials to begin with.
#
# It runs in a subprocess, with a wall-clock timeout, for a reason. The failure
# this guards against is a login prompt in a session that cannot answer one:
# the redivis client responds to a 401 by starting the OAuth device flow and
# polling until the code expires. In-process that would hang the suite for
# minutes with no useful message; out of process it comes back as a timeout,
# which is the signal we want.

anonymous_read_script <- function(cache_dir) {
  sprintf(
    '.libPaths(c(%s))
     Sys.setenv(IRW_CACHE_DIR = %s)
     library(irw)
     stopifnot(!irw_has_credentials())

     tables <- suppressWarnings(irw_list_tables())
     stopifnot(is.data.frame(tables), "name" %%in%% names(tables), nrow(tables) > 500)

     x <- suppressMessages(irw_fetch("environment_ltm"))
     stopifnot(nrow(x) > 0, all(c("id", "item", "resp") %%in%% names(x)))

     # The one path that still needs a login, and must say so rather than
     # waiting for a browser.
     msg <- tryCatch({
       irw_table_sets("environment_ltm")
       NA_character_
     }, error = conditionMessage)
     stopifnot(grepl("needs a Redivis login", msg, fixed = TRUE))

     cat("ANONYMOUS-READ-OK\\n")',
    paste(deparse(.libPaths()), collapse = " "),
    deparse(cache_dir)
  )
}

test_that("fetching and listing IRW tables works with no Redivis account", {
  skip_on_cran()
  skip_if_not_installed("redivis")
  skip_if_not(nzchar(Sys.getenv("IRW_TEST_ANONYMOUS")),
              "set IRW_TEST_ANONYMOUS=true to run the networked anonymous check")
  skip_if_not(irw:::.irw_redivis_supports_anonymous(),
              "this redivis build authenticates every request")

  home <- withr::local_tempdir()
  cache <- withr::local_tempdir()
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(anonymous_read_script(cache), script)

  out <- withr::with_envvar(
    c(HOME = home, USERPROFILE = home, REDIVIS_API_TOKEN = NA,
      REDIVIS_DEFAULT_NOTEBOOK = NA, R_BROWSER = "false", NOT_CRAN = "true"),
    system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
            stdout = TRUE, stderr = TRUE, timeout = 600)
  )
  status <- attr(out, "status")

  expect_false(
    identical(status, 124L),
    info = paste(
      "the anonymous read timed out, which is what a login prompt looks like",
      "from here:", paste(out, collapse = "\n")
    )
  )
  expect_true(
    "ANONYMOUS-READ-OK" %in% out,
    info = paste("anonymous read failed:", paste(out, collapse = "\n"))
  )
  # Nothing was written to the throwaway HOME, i.e. no login happened.
  expect_length(list.files(home, all.files = TRUE, no.. = TRUE), 0)
})
