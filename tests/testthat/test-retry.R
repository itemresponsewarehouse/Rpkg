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
