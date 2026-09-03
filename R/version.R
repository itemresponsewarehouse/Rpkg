#' Accepted form of a Redivis version tag
#'
#' Redivis silently resolves an unrecognized version string to the current
#' release (e.g. \code{version = "banana"} returns the latest version rather
#' than erroring), which would defeat the point of pinning. Tags are therefore
#' validated here before being sent, and the tag Redivis resolves to is checked
#' against the tag that was asked for.
#'
#' @keywords internal
#' @noRd
.irw_version_pattern <- "^v?[0-9]+\\.[0-9]+$"

#' Pin value for a dataset that had no release at the pinned IRW version
#'
#' A dataset younger than the IRW version being read cannot be left unpinned:
#' unpinned means "current release", which would quietly mix today's data into
#' a run that is supposed to reproduce an old one. It is pinned to this
#' sentinel instead, and reading it errors. Deliberately not a valid version
#' tag, so \code{irw_set_version()} cannot be talked into setting it.
#'
#' @keywords internal
#' @noRd
.irw_absent_version <- "<none>"

#' Human-readable form of a pin
#'
#' @keywords internal
#' @noRd
.irw_format_pin <- function(tag) {
  ifelse(tag == .irw_absent_version, "not released yet", tag)
}

#' Normalize a version tag to Redivis' \code{"vN.N"} form
#'
#' @param version Character scalar, e.g. \code{"32.0"} or \code{"v32.0"}.
#' @return Normalized tag, or \code{NULL} if \code{version} is not a valid tag.
#' @keywords internal
#' @noRd
.irw_normalize_version <- function(version) {
  if (!is.character(version) || length(version) != 1L || is.na(version)) {
    return(NULL)
  }
  version <- trimws(version)
  if (!grepl(.irw_version_pattern, version)) {
    return(NULL)
  }
  if (startsWith(version, "v")) version else paste0("v", version)
}

#' Dataset key used to pin a version
#'
#' Redivis dataset specs carry a short hash (\code{"item_response_warehouse:as2e"}),
#' which users should not have to type. The key is the part before the colon.
#'
#' @param dataset A dataset spec string.
#' @keywords internal
#' @noRd
.irw_dataset_key <- function(dataset) {
  sub(":.*$", "", dataset)
}

#' Every pinnable Redivis dataset, keyed by dataset name
#'
#' Covers the table sources in \code{.irw_datasource_specs} plus the metadata
#' and item text datasets, so that a pinned session is reproducible in its
#' metadata as well as its response data.
#'
#' @return Named list of specs.
#' @keywords internal
#' @noRd
.irw_pinnable_specs <- function() {
  specs <- c(
    unlist(.irw_datasource_specs, recursive = FALSE, use.names = FALSE),
    list(.irw_meta_spec, .irw_itemtext_spec)
  )
  names(specs) <- vapply(specs, function(s) .irw_dataset_key(s$dataset), character(1))
  specs
}

#' Versions pinned in this session
#'
#' @return Named character vector (dataset key -> version tag); empty if none.
#' @keywords internal
#' @noRd
.irw_pins <- function() {
  if (!exists("pinned_versions", envir = .irw_env)) {
    return(stats::setNames(character(0), character(0)))
  }
  .irw_env$pinned_versions
}

#' Version pinned for one dataset, if any
#'
#' @param key Dataset key from \code{.irw_dataset_key()}.
#' @return Version tag, or \code{NULL} when the dataset is not pinned.
#' @keywords internal
#' @noRd
.irw_pinned_version <- function(key) {
  pins <- .irw_pins()
  if (!key %in% names(pins)) NULL else unname(pins[[key]])
}

#' Pins that apply to a given data source
#'
#' @param source One of \code{"core"}, \code{"nom"}, \code{"sim"}, \code{"comp"}.
#' @return Named character vector of pins for that source's datasets.
#' @keywords internal
#' @noRd
.irw_source_pins <- function(source) {
  keys <- vapply(.irw_datasource_specs[[source]], function(s) .irw_dataset_key(s$dataset), character(1))
  pins <- .irw_pins()
  pins[names(pins) %in% keys]
}

#' Drop every cached datasource, metadata table, and fingerprint
#'
#' A version change invalidates all of them at once: the opened dataset
#' objects, the table listings derived from them, and the metadata filtered
#' against those listings. Clearing by exclusion rather than by an enumerated
#' list means a cache added later is not silently left stale behind a pin.
#'
#' @keywords internal
#' @noRd
.irw_clear_all_datasource_caches <- function() {
  # The manifest is a download, not a cache of version-dependent data: it says
  # what every IRW version held, so it is still true after the pins change.
  keep <- c("pinned_versions", "itemtext_disclaimer_shown", "manifest")
  drop <- setdiff(ls(.irw_env, all.names = TRUE), keep)
  if (length(drop) > 0L) {
    rm(list = drop, envir = .irw_env)
  }
  invisible(NULL)
}

#' Open a dataset at a specific version and confirm the version took effect
#'
#' @param spec Dataset spec list.
#' @param version Normalized version tag.
#' @return The opened Redivis dataset object.
#' @keywords internal
#' @noRd
.irw_open_dataset_at_version <- function(spec, version) {
  ds <- .irw_redivis_dataset(spec, version)
  ds$get()
  ds
}

#' Confirm a version tag exists and is what Redivis actually serves
#'
#' Redivis resolves an unrecognized version to the current release instead of
#' failing, which would defeat the point of pinning, so every tag is opened and
#' the tag it resolves to is compared with the tag that was asked for.
#'
#' @param spec Dataset spec list.
#' @param dataset Dataset key, for the error message.
#' @param tag Normalized version tag.
#' @return Invisibly, the opened Redivis dataset object.
#' @keywords internal
#' @noRd
.irw_verify_version <- function(spec, dataset, tag) {
  ds <- tryCatch(
    .irw_open_dataset_at_version(spec, tag),
    error = function(e) {
      msg <- conditionMessage(e)
      if (.irw_redivis_error_type(msg) == "auth") {
        stop(.irw_auth_error_message(), call. = FALSE)
      }
      stop(
        "Version ", tag, " of ", dataset, " does not exist on Redivis.\n",
        "Use `irw_get_version()` to see the version currently in use.",
        call. = FALSE
      )
    }
  )

  resolved <- ds$properties$version$tag
  if (!identical(as.character(resolved), tag)) {
    stop(
      "Version ", tag, " of ", dataset, " could not be resolved on Redivis ",
      "(got ", shQuote(as.character(resolved)), " instead).",
      call. = FALSE
    )
  }
  invisible(ds)
}

#' Pin an IRW Redivis dataset to a released version
#'
#' Fixes the version of one IRW dataset for the remainder of the R session, so
#' that fetches, listings, and metadata are reproducible. Pin at the top of an
#' analysis script and the script will return the same data when it is re-run,
#' even after IRW has been updated.
#'
#' The pin covers every lookup that reads the dataset, not just fetches:
#' \code{irw_fetch()}, \code{irw_filter()}, \code{irw_list_tables()}, and
#' metadata all read the pinned version.
#'
#' Each IRW dataset is versioned independently on Redivis, so pinning is
#' per-dataset. \code{irw_get_version()} reports the versions currently in use,
#' which is how you find the tags to write into a script.
#'
#' A table that does not exist in the pinned version is an error rather than a
#' silent fetch from the current release: quietly mixing versions would break
#' the reproducibility the pin exists to provide.
#'
#' @param dataset Character. Name of the IRW dataset to pin, e.g.
#'   \code{"item_response_warehouse"}. See \code{irw_get_version()} for the
#'   full list.
#' @param version Character. A released version tag, e.g. \code{"v32.0"}
#'   (a leading \code{"v"} is optional).
#'
#' @return Invisibly, the named character vector of all pins in effect.
#' @seealso \code{\link{irw_get_version}}, \code{\link{irw_reset_version}}
#' @examples
#' \dontrun{
#' irw_set_version("item_response_warehouse", "v32.0")
#' irw_set_version("item_response_warehouse_2", "v1.8")
#'
#' df <- irw_fetch("kfcovid_fear_Li2020")   # served from v32.0
#'
#' irw_reset_version()                      # back to the current release
#' }
#' @export
irw_set_version <- function(dataset, version) {
  if (!is.character(dataset) || length(dataset) != 1L || is.na(dataset) || !nzchar(dataset)) {
    stop("'dataset' must be a single dataset name, e.g. \"item_response_warehouse\".", call. = FALSE)
  }
  specs <- .irw_pinnable_specs()
  if (!dataset %in% names(specs)) {
    stop(
      "Unknown IRW dataset: ", shQuote(dataset), ".\n",
      "Pinnable datasets are: ", paste(names(specs), collapse = ", "), ".",
      call. = FALSE
    )
  }

  tag <- .irw_normalize_version(version)
  if (is.null(tag)) {
    stop(
      "'version' must be a released version tag such as \"v32.0\"; got ",
      shQuote(as.character(version)[1]), ".\n",
      "Use `irw_get_version()` to see the versions currently in use.",
      call. = FALSE
    )
  }

  .irw_verify_version(specs[[dataset]], dataset, tag)

  pins <- .irw_pins()
  pins[[dataset]] <- tag
  .irw_env$pinned_versions <- pins
  .irw_clear_all_datasource_caches()

  message("Pinned ", dataset, " to ", tag, " for this session.")
  invisible(.irw_env$pinned_versions)
}

#' Report the IRW dataset versions in use
#'
#' Shows which Redivis version each IRW dataset is being read at, and whether
#' that version is pinned for the session or is simply the current release.
#' Record these tags to make an analysis reproducible; replay them with
#' \code{\link{irw_set_version}}.
#'
#' @param dataset Optional character vector of dataset names to report on.
#'   Defaults to every IRW dataset.
#'
#' @return A data frame with columns \code{dataset}, \code{version}, and
#'   \code{pinned}.
#' @seealso \code{\link{irw_set_version}}, \code{\link{irw_reset_version}}
#' @examples
#' \dontrun{
#' irw_get_version()
#' irw_get_version("item_response_warehouse")
#' }
#' @export
irw_get_version <- function(dataset = NULL) {
  specs <- .irw_pinnable_specs()
  if (!is.null(dataset)) {
    if (!is.character(dataset)) {
      stop("'dataset' must be a character vector of dataset names.", call. = FALSE)
    }
    unknown <- setdiff(dataset, names(specs))
    if (length(unknown) > 0L) {
      stop(
        "Unknown IRW dataset(s): ", paste(shQuote(unknown), collapse = ", "), ".\n",
        "Known datasets are: ", paste(names(specs), collapse = ", "), ".",
        call. = FALSE
      )
    }
    specs <- specs[dataset]
  }

  pins <- .irw_pins()
  versions <- vapply(names(specs), function(key) {
    # An absent pin has no version to report and nothing to ask Redivis about.
    if (identical(.irw_pinned_version(key), .irw_absent_version)) {
      return(NA_character_)
    }
    tag <- tryCatch(
      {
        ds <- .irw_open_dataset(specs[[key]])
        as.character(ds$properties$version$tag)
      },
      error = function(e) NA_character_
    )
    if (length(tag) != 1L) NA_character_ else tag
  }, character(1), USE.NAMES = FALSE)

  out <- data.frame(
    dataset = names(specs),
    version = versions,
    pinned = names(specs) %in% names(pins),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

#' Remove IRW version pins
#'
#' Returns one dataset, or the whole session, to reading the current release.
#'
#' @param dataset Optional character vector of dataset names to unpin. Defaults
#'   to every pinned dataset.
#'
#' @return Invisibly, the named character vector of pins still in effect.
#' @seealso \code{\link{irw_set_version}}, \code{\link{irw_get_version}}
#' @examples
#' \dontrun{
#' irw_reset_version("item_response_warehouse")
#' irw_reset_version()   # unpin everything
#' }
#' @export
irw_reset_version <- function(dataset = NULL) {
  pins <- .irw_pins()

  if (is.null(dataset)) {
    dropped <- names(pins)
    pins <- stats::setNames(character(0), character(0))
  } else {
    if (!is.character(dataset)) {
      stop("'dataset' must be a character vector of dataset names.", call. = FALSE)
    }
    dropped <- intersect(dataset, names(pins))
    pins <- pins[setdiff(names(pins), dataset)]
  }

  .irw_env$pinned_versions <- pins
  .irw_clear_all_datasource_caches()

  if (length(dropped) == 0L) {
    message("No IRW version pins were set.")
  } else {
    message("Unpinned ", paste(dropped, collapse = ", "), "; using the current release.")
  }
  invisible(pins)
}

#' Message for a table that is absent from the pinned version(s) of a source
#'
#' Distinguishes "this table has never existed in IRW" from "this table is not
#' in the release you pinned", which are the same Redivis not-found error but
#' call for entirely different fixes.
#'
#' @param table_name Table name that could not be found.
#' @param source Resolved data source.
#' @return A message string, or \code{NULL} when no pin applies to \code{source}.
#' @keywords internal
#' @noRd
.irw_pinned_not_found_message <- function(table_name, source) {
  pins <- .irw_source_pins(source)
  if (length(pins) == 0L) {
    return(NULL)
  }
  paste0(
    "\nTable ", shQuote(table_name), " does not exist in the pinned version(s): ",
    paste(paste(names(pins), .irw_format_pin(unname(pins))), collapse = ", "), ".\n",
    "It may have been added in a later release. Use `irw_reset_version()` to fetch ",
    "the current version, or `irw_list_tables()` to see what the pinned version holds."
  )
}
