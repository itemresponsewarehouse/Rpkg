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


#' Reject an argument pair that names two different points in history
#'
#' @keywords internal
#' @noRd
.irw_check_one_key <- function(version, date) {
  if (!is.null(version) && !is.null(date)) {
    stop(
      "Give either 'version' or 'date', not both. An IRW version number is ",
      "exact; a date has to be resolved to one.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Coerce a user-supplied IRW version number
#'
#' Accepts \code{332}, \code{332L} or \code{"332"}; anything else is a mistake
#' worth naming, since a date silently coerced to a version number would pin a
#' session to the wrong corpus.
#'
#' @keywords internal
#' @noRd
.irw_as_irw_version <- function(version) {
  if (is.character(version) && length(version) == 1L && !is.na(version) &&
      grepl("^[0-9]+$", trimws(version))) {
    version <- as.numeric(trimws(version))
  }
  if (!is.numeric(version) || length(version) != 1L || is.na(version) ||
      version != round(version) || version < 1 || version > 1e6) {
    stop(
      "'version' must be a single IRW version number, e.g. 332; got ",
      shQuote(paste(as.character(version), collapse = ", ")), ".\n",
      "To look up a date instead, name the argument: date = \"2026-08-01\".",
      call. = FALSE
    )
  }
  as.integer(version)
}

#' Resolve a version number or a date to one IRW version's manifest rows
#'
#' The single place that turns "which point in history" into rows, so that
#' \code{irw_version()} and \code{irw_use_version()} cannot disagree about what
#' a date or a number means.
#'
#' @param version Optional IRW version number.
#' @param date Optional date or time.
#' @return A list with \code{version}, \code{rows}, and \code{as_of} (whether
#'   the version was reached via a date, which is the case that can be wrong).
#' @keywords internal
#' @noRd
.irw_manifest_select <- function(version = NULL, date = NULL) {
  .irw_check_one_key(version, date)
  manifest <- .irw_manifest()

  if (!is.null(version)) {
    version <- .irw_as_irw_version(version)
    if (!version %in% manifest$irw_version) {
      stop(
        "IRW has no version ", version, ". Released versions run from ",
        min(manifest$irw_version), " to ", max(manifest$irw_version), ".",
        call. = FALSE
      )
    }
    as_of <- FALSE
  } else if (is.null(date)) {
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

  list(
    version = version,
    rows = manifest[manifest$irw_version == version, , drop = FALSE],
    as_of = as_of
  )
}

#' Which IRW version was live, and what every dataset was pinned to
#'
#' IRW is made of eleven Redivis datasets that are versioned independently, so
#' no single Redivis version describes the corpus. The IRW version number does:
#' it increments whenever any dataset is published and names one exact
#' combination of the eleven. Cite it in a paper and a reader can reconstruct
#' the data you used.
#'
#' Called with no argument, reports the newest IRW version. Called with a
#' `version`, reports exactly what that version held. Called with a `date`,
#' reports the version that was live then -- which is how you recover what an
#' analysis run months ago was actually reading.
#'
#' Use [irw_use_version()] to make the session read one of these versions.
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
#' affected. This is why a paper should cite the number, not the date.
#'
#' @param date Optional. A date or time to look up, as `"2026-08-01"`,
#'   `"2026-08-01 12:00:00"`, or a `Date`/`POSIXct`.
#' @param version Optional. An IRW version number, e.g. `332`. Mutually
#'   exclusive with `date`. Defaults, with `date`, to the newest version.
#'
#' @return A data frame with one row per dataset and columns `dataset`,
#'   `version`, `released_at`, and `approximate`. The IRW version number is
#'   attached as the attribute `irw_version` and printed as a message.
#'
#' @seealso [irw_use_version()] to read a whole IRW version,
#'   [irw_set_version()] to pin one dataset, [irw_get_version()] for the
#'   versions the current session is actually reading.
#' @examples
#' \dontrun{
#' irw_version()                # the newest IRW version, and its eleven pins
#' irw_version(version = 332)   # exactly what v332 held
#' irw_version("2026-08-01")    # what was live on 1 August 2026
#' }
#' @export
irw_version <- function(date = NULL, version = NULL) {
  sel <- .irw_manifest_select(version = version, date = date)
  rows <- sel$rows

  out <- data.frame(
    dataset = rows$dataset,
    version = rows$redivis_tag,
    released_at = rows$redivis_released_at,
    approximate = rows$precision == "bracketed",
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  attr(out, "irw_version") <- sel$version
  attr(out, "irw_released_at") <- rows$irw_released_at[1]

  message(
    "IRW v", sel$version, " (released ", rows$irw_released_at[1], "), ",
    nrow(out), " dataset(s)."
  )
  .irw_warn_approximate(out$approximate, sel$as_of)

  out
}

#' Report the two different meanings of a bracketed row
#'
#' The caveats are not the same and must not be confused. Asked for a version,
#' a bracketed row means only that we cannot date it -- the pins are exactly
#' what that version held. Asked what was live on a *date*, the same row means
#' the pin itself may be wrong.
#'
#' @param approximate Logical vector, one element per dataset.
#' @param as_of Whether the version was reached from a date.
#' @keywords internal
#' @noRd
.irw_warn_approximate <- function(approximate, as_of) {
  n <- sum(approximate)
  if (n == 0L) {
    return(invisible(NULL))
  }
  if (as_of) {
    warning(
      "This is approximate. ", n, " of ", length(approximate), " pins rest on ",
      "a release date that Redivis overwrote, so for those the version tag may ",
      "be wrong as well: a later release could already have been live. Cite ",
      "an IRW version number rather than a date.",
      call. = FALSE
    )
  } else {
    message(
      "  ", n, " of ", length(approximate), " release dates are approximate ",
      "(Redivis overwrote them). The pins are exact; only their dates are ",
      "lower bounds."
    )
  }
  invisible(NULL)
}

#' Read the whole corpus at one IRW version
#'
#' Pins every IRW dataset at once to what a single IRW version held, so that
#' `irw_fetch()` and everything around it return the same data whenever the
#' script is re-run -- even after IRW has been corrected or extended. This is
#' the reproducibility switch: put one call at the top of an analysis and the
#' rest of the script is frozen.
#'
#' ```r
#' irw_use_version(332)
#' df <- irw_fetch("gilbert_meta_12")   # v332's copy, today and next year
#' ```
#'
#' Called with no arguments it pins the newest version and reports its number:
#' run it at the start of a project and record the number it prints.
#'
#' The pin covers the metadata and item text datasets too, not just response
#' data, and lasts for the session; [irw_reset_version()] lifts it.
#'
#' @section Each dataset is pinned to its own version:
#' The response data is spread over several Redivis warehouses ("shards"), each
#' released on its own schedule, so one IRW version means a *different* Redivis
#' version in each. That is handled: every dataset is pinned to the tag the
#' manifest records for it, and a single `irw_fetch()` call may read several
#' shards at different versions.
#'
#' ```r
#' irw_use_version(200)   # shard 1 at v36.0, shard 2 at v3.3
#' L <- irw_fetch(c("idcr_martinez_2023_numSeries", "gcb5_2025"))
#' ```
#'
#' @section A dataset that did not exist yet:
#' Not every IRW dataset has a release in every IRW version -- the sixth
#' warehouse shard is younger than the first. Rather than let those fall
#' through to the current release and silently mix versions into a "reproduced"
#' run, they are pinned to nothing and error if something reads them. Fetches
#' are unaffected: a table that did not exist then is simply not found.
#'
#' @section Prefer a version number to a date:
#' A date has to be resolved to a version, and before 21 July 2026 that
#' resolution is approximate for the older warehouse shards -- see
#' [irw_version()]. Pinning by date therefore warns; pinning by number never
#' has to.
#'
#' @param version Optional IRW version number, e.g. `332`. Defaults to the
#'   newest version.
#' @param date Optional date or time to pin the corpus as it was then, e.g.
#'   `"2026-08-01"`. Mutually exclusive with `version`.
#' @param quiet Logical. Suppress the per-dataset summary.
#'
#' @return Invisibly, a data frame of `dataset` and `version` as pinned, with
#'   the IRW version number in the attribute `irw_version`.
#' @seealso [irw_version()], [irw_get_version()], [irw_reset_version()]
#' @examples
#' \dontrun{
#' irw_use_version(332)              # read the corpus as v332 held it
#' irw_use_version()                 # freeze today's version; record the number
#' irw_use_version(date = "2026-08-01")
#' irw_reset_version()               # back to the current release
#' }
#' @export
irw_use_version <- function(version = NULL, date = NULL, quiet = FALSE) {
  sel <- .irw_manifest_select(version = version, date = date)
  rows <- sel$rows
  specs <- .irw_pinnable_specs()

  unknown <- setdiff(rows$dataset, names(specs))
  if (length(unknown) > 0L) {
    warning(
      "IRW v", sel$version, " contains ", length(unknown), " dataset(s) this ",
      "version of the package does not know about: ",
      paste(unknown, collapse = ", "), ".\n",
      "They cannot be pinned or read here; update the package.",
      call. = FALSE
    )
  }

  tags <- stats::setNames(rows$redivis_tag, rows$dataset)
  pins <- stats::setNames(character(0), character(0))
  for (key in names(specs)) {
    tag <- if (key %in% names(tags)) .irw_normalize_version(unname(tags[[key]])) else NULL
    if (is.null(tag)) {
      # Either absent from this version, or a tag the manifest wrote in a form
      # we do not recognize. Both must block reads rather than fall through.
      pins[[key]] <- .irw_absent_version
    } else {
      .irw_verify_version(specs[[key]], key, tag)
      pins[[key]] <- tag
    }
  }

  .irw_env$pinned_versions <- pins
  .irw_clear_all_datasource_caches()

  out <- data.frame(
    dataset = names(pins),
    version = ifelse(unname(pins) == .irw_absent_version, NA_character_, unname(pins)),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  attr(out, "irw_version") <- sel$version

  absent <- sum(is.na(out$version))
  message(
    "Reading IRW v", sel$version, " (released ", rows$irw_released_at[1], ") ",
    "for this session: ", nrow(out) - absent, " dataset(s) pinned",
    if (absent > 0L) paste0(", ", absent, " not yet released then") else "",
    "."
  )
  if (!quiet) {
    for (i in seq_len(nrow(out))) {
      message("  ", out$dataset[i], ": ",
              if (is.na(out$version[i])) "not released yet" else out$version[i])
    }
  }
  .irw_warn_approximate(rows$precision == "bracketed", sel$as_of)

  invisible(out)
}
