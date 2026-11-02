# .retry_with_backoff() used to be a no-op; these pin that it now retries
# transient failures and nothing else. .irw_sleep is stubbed so no test waits.

test_that("a transient failure is retried until it succeeds", {
  local_irw_binding(".irw_sleep", function(seconds) NULL)
  calls <- 0L
  out <- irw:::.retry_with_backoff(function() {
    calls <<- calls + 1L
    if (calls < 3L) stop("Timeout was reached: [redivis.com] Operation timed out")
    "ok"
  })
  expect_equal(out, "ok")
  expect_equal(calls, 3L)
})

test_that("backoff doubles and the last error is rethrown after the final attempt", {
  waits <- numeric(0)
  local_irw_binding(".irw_sleep", function(seconds) waits <<- c(waits, seconds))
  calls <- 0L
  expect_error(
    irw:::.retry_with_backoff(function() {
      calls <<- calls + 1L
      stop("HTTP 502 Bad Gateway")
    }),
    "502"
  )
  expect_equal(calls, 3L)
  expect_equal(waits, c(1, 2))
})

test_that("non-transient errors are rethrown at once", {
  local_irw_binding(".irw_sleep", function(seconds) stop("should not sleep"))
  for (msg in c(
    "[400 invalid_request_error] Unrecognized name: resp",
    "not_found_error: Table not found",
    "Cannot export more than 100GB within 30 days",
    "401 unauthorized",
    "object 'x' not found"
  )) {
    calls <- 0L
    expect_error(irw:::.retry_with_backoff(function() {
      calls <<- calls + 1L
      stop(msg)
    }))
    expect_equal(calls, 1L, info = msg)
  }
})

test_that("the error classifier accepts the transient signatures Python retries", {
  transient <- c(
    "HTTP 504 Gateway Timeout",
    "Recv failure: Connection reset by peer",
    "Invalid: Expected to be able to read 1024 bytes for message body, got 12",
    "Empty reply from server",
    "Failed writing body: broken pipe"
  )
  for (msg in transient) expect_true(irw:::.irw_is_transient_error(msg), info = msg)
  # A quota error that mentions a timeout is still quota, and is not retried.
  expect_false(irw:::.irw_is_transient_error("rate limit exceeded; request timed out"))
})

test_that("redivis' advice to pin a reference id is muffled, and nothing else is", {
  # The advice is wrong for this package: a reference id names one VERSION's
  # table and a release cut rotates them, so there is nothing a caller can do
  # with it. See the comment on .irw_redivis_advice_pattern.
  advice <- c(
    paste("No reference id was provided for the dataset, which may cause your",
          "code to break if the name changes. Consider using the qualified",
          'reference "item_response_warehouse:as2e"'),
    paste("No reference id was provided for the table, which may cause your",
          'code to break if the name changes. Consider using "swmd_mokken:4nvd"')
  )
  for (msg in advice) {
    expect_silent(irw:::.retry_with_backoff(function() {
      warning(msg)
      "value"
    }))
    expect_equal(
      suppressWarnings(irw:::.retry_with_backoff(function() { warning(msg); "value" })),
      "value"
    )
  }

  # Every other redivis warning has to survive: the shard-list and
  # duplicate-name warnings sit right beside this one.
  expect_warning(irw:::.retry_with_backoff(function() {
    warning("table name published in more than one shard")
    "value"
  }), "more than one shard")
})

test_that("muffling a warning does not swallow the retry behaviour", {
  calls <- 0L
  out <- irw:::.retry_with_backoff(function() {
    calls <<- calls + 1L
    warning("No reference id was provided for the dataset")
    if (calls < 2L) stop("HTTP 504 Gateway Timeout")
    "value"
  })
  expect_equal(out, "value")
  expect_equal(calls, 2L)
})
