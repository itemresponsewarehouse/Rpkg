#' The IRW version manifest
#'
#' IRW is eleven Redivis datasets, each versioned independently, so there has
#' never been a single number a paper could cite. The manifest supplies one: a
#' record, kept in the `irw` repository, of which released version of every
#' dataset was live at every point in the corpus' history. `irw_version()`
#' reads it.
#'
#' The file is fetched from GitHub rather than shipped inside the package. It
#' is rewritten daily as datasets are published, and a copy baked in at build
#' time would tell a user on a three-month-old install that the newest IRW
#' version is whatever it was in June. Fetching costs one request per session.
#'
#' The path is `metadata/`, not `src/metadata/`: the `irw` repository's root is
#' the directory that appears as `src/` in a local working copy.
#'
#' @keywords internal
#' @noRd
.irw_manifest_url <- paste0(
  "https://raw.githubusercontent.com/ben-domingue/irw/main/",
  "metadata/version_manifest.tsv"
)

#' Download and cache the manifest for this session
#'
#' @return A data frame with one row per (IRW version, dataset).
#' @keywords internal
#' @noRd
.irw_manifest <- function() {
  if (!is.null(.irw_env$manifest)) {
    return(.irw_env$manifest)
  }

  response <- tryCatch(
    httr::GET(.irw_manifest_url, httr::timeout(30)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200) {
    stop(
      "Could not download the IRW version manifest from\n  ",
      .irw_manifest_url, "\n",
      "Check your internet connection and try again. Note that pinning a ",
      "dataset directly with `irw_set_version()` does not need the manifest.",
      call. = FALSE
    )
  }

  text <- httr::content(response, as = "text", encoding = "UTF-8")
  manifest <- utils::read.delim(
    text = text, sep = "\t", stringsAsFactors = FALSE, colClasses = "character"
  )

  .irw_manifest_check_columns(manifest)

  manifest$irw_version <- as.integer(manifest$irw_version)
  manifest$released <- .irw_parse_utc(manifest$irw_released_at)
  .irw_env$manifest <- manifest
  manifest
}

#' Refuse a manifest whose schema has moved on
#'
#' The file is written by the `irw` repository and read by an installed
#' package, so the two can drift. Guessing at renamed columns would produce
#' plausible-looking wrong pins, which is worse than not answering.
#'
#' @keywords internal
#' @noRd
.irw_manifest_check_columns <- function(manifest) {
  expected <- c("irw_version", "irw_released_at", "dataset", "redivis_tag",
                "redivis_released_at", "precision", "redivis_released_before")
  if (!identical(names(manifest)[seq_along(expected)], expected)) {
    stop(
      "The IRW version manifest does not have the expected columns. This ",
      "package may be out of date; please report it at\n  ",
      "https://github.com/itemresponsewarehouse/Rpkg/issues",
      call. = FALSE
    )
  }
  invisible(manifest)
}

#' Parse the manifest's ISO-8601 UTC timestamps
#'
#' @keywords internal
#' @noRd
.irw_parse_utc <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Coerce a user-supplied date or time to UTC
#'
#' Accepts what someone would actually type: `"2026-08-01"`, a timestamp, or a
#' `Date`/`POSIXct`. A bare date means the start of that day, so
#' `irw_version("2026-08-01")` answers "what was live when that day began".
#'
#' @keywords internal
#' @noRd
.irw_as_utc <- function(when) {
  if (inherits(when, "POSIXct")) {
    return(as.POSIXct(as.numeric(when), origin = "1970-01-01", tz = "UTC"))
  }
  if (inherits(when, "Date")) {
    return(as.POSIXct(format(when, "%Y-%m-%d"), format = "%Y-%m-%d", tz = "UTC"))
  }
  if (!is.character(when) || length(when) != 1L || is.na(when)) {
    stop(
      "'date' must be a single date or time, e.g. \"2026-08-01\", ",
      "\"2026-08-01 12:00:00\", or a Date/POSIXct object.",
      call. = FALSE
    )
  }
  when <- trimws(when)
  for (fmt in c("%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S",
                "%Y-%m-%d %H:%M", "%Y-%m-%d")) {
    parsed <- as.POSIXct(when, format = fmt, tz = "UTC")
    if (!is.na(parsed)) {
      return(parsed)
    }
  }
  stop(
    "Could not read ", shQuote(when), " as a date. Try \"2026-08-01\" or ",
    "\"2026-08-01 12:00:00\".",
    call. = FALSE
  )
}

#' Which IRW version was live, and what every dataset was pinned to
#'
#' IRW is made of eleven Redivis datasets that are versioned independently, so
#' no single Redivis version describes the corpus. The IRW version number does:
#' it increments whenever any dataset is published, and it names one exact
#' combination of the eleven. Cite it in a paper and a reader can reconstruct
#' the data you used.
#'
#' Called with no argument, reports the newest IRW version. Called with a date,
#' reports the version that was live on that date -- which is how you recover
#' what an analysis run months ago was actually reading.
#'
#' @section Dates before 21 July 2026 are approximate:
#' Redivis overwrote its own release timestamps for the older warehouse shards
#' during a platform migration: 142 of the corpus' 332 released versions claim
#' to have been released inside one 80-minute window on 2026-07-21. For those,
#' the manifest records the earliest date the version could have been live
#' (its creation date) and marks the row `bracketed`. A lookup that lands on
#' one warns, because the *tag* may then be wrong too -- a later version could
#' already have been released inside the bracket. IRW version numbers
#' themselves are always exact; only the mapping from a date to a version is
#' affected.
#'
#' @param date Optional. A date or time to look up, as `"2026-08-01"`,
#'   `"2026-08-01 12:00:00"`, or a `Date`/`POSIXct`. Defaults to the newest
#'   version.
#'
#' @return A data frame with one row per dataset and columns `dataset`,
#'   `version`, `released_at`, and `approximate`. The IRW version number is
#'   attached as the attribute `irw_version` and printed as a message.
#'
#' @seealso [irw_set_version()] to pin a dataset, [irw_get_version()] for the
#'   versions the current session is actually reading.
#' @examples
#' \dontrun{
#' irw_version()                # the newest IRW version, and its eleven pins
#' irw_version("2026-08-01")    # what was live on 1 August 2026
#'
#' # Replay it: pin each dataset to what that version held.
#' pins <- irw_version("2026-08-01")
#' for (i in seq_len(nrow(pins))) {
#'   irw_set_version(pins$dataset[i], pins$version[i])
#' }
#' }
#' @export
irw_version <- function(date = NULL) {
  manifest <- .irw_manifest()

  if (is.null(date)) {
    version <- max(manifest$irw_version)
    as_of <- FALSE
  } else {
    when <- .irw_as_utc(date)
    live <- manifest$irw_version[manifest$released <= when]
    if (length(live) == 0L) {
      stop(
        "IRW has no version from before ",
        format(min(manifest$released), "%Y-%m-%d"),
        "; the corpus did not exist yet.",
        call. = FALSE
      )
    }
    version <- max(live)
    as_of <- TRUE
  }

  rows <- manifest[manifest$irw_version == version, , drop = FALSE]
  out <- data.frame(
    dataset = rows$dataset,
    version = rows$redivis_tag,
    released_at = rows$redivis_released_at,
    approximate = rows$precision == "bracketed",
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  attr(out, "irw_version") <- version
  attr(out, "irw_released_at") <- rows$irw_released_at[1]

  message(
    "IRW v", version, " (released ", rows$irw_released_at[1], "), ",
    nrow(out), " dataset(s)."
  )

  # The two caveats are different and must not be confused. Asked for a
  # version, a bracketed row means only that we cannot date it -- the pins are
  # exactly what that version held. Asked what was live on a *date*, the same
  # row means the pin itself may be wrong.
  approx <- sum(out$approximate)
  if (approx > 0L && as_of) {
    warning(
      "This is approximate. ", approx, " of ", nrow(out), " pins rest on a ",
      "release date that Redivis overwrote, so for those the version tag may ",
      "be wrong as well: a later release could already have been live. Cite ",
      "an IRW version number rather than a date.",
      call. = FALSE
    )
  } else if (approx > 0L) {
    message(
      "  ", approx, " of ", nrow(out), " release dates are approximate ",
      "(Redivis overwrote them). The pins are exact; only their dates are ",
      "lower bounds."
    )
  }

  out
}
