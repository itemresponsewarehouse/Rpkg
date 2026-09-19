# On-disk cache for downloaded IRW tables, shared with the Python package.
#
# Every table download counts against the Redivis rolling 30-day export cap,
# and until this file nothing outlived the session -- not even within it: the
# same irw_fetch() twice exported the table twice. The cache keeps each
# downloaded table as a Parquet file that the Python package
# (src/irw/utils/redivis/disk_cache.py) reads and writes too, so a table fetched
# in either language is not exported again until it changes.
#
# The format is a contract between the two packages -- change it in both, or
# start a new tree beside v1. Spec: ben-domingue/irw#2253.
#
#     <root>/v1/<kind>/<table>/<hash>.parquet
#
# - kind is "tables" or "itemtext".
# - table is the name Redivis reports for the table, not what the caller typed.
# - hash is the table's Redivis content hash (tbl$properties$hash). It is keyed
#   on the table, not on the dataset's version tag: a release re-tags a whole
#   shard, and keying on the tag would re-export every unchanged table in it.
#   The hash stays put across releases for an unchanged table and moves when
#   the table is rebuilt, even at the same row count (checked 2026-09-19: the
#   three pisa2015 tables across v52.1 -> v53.0).
#
# The file holds the raw download -- the Arrow table Redivis sends -- and
# carries its provenance in the Parquet schema metadata (irw_* keys). It is
# checked after the shard search has found the table and before the export,
# so validity is the hash tbl$get() just read, and a hit costs only the
# metadata requests every fetch already makes.

.irw_cache_format <- "v1"
.irw_cache_kinds <- c("tables", "itemtext")

#' Folder where fetched tables are cached
#'
#' `irw_fetch()` and `irw_itemtext()` keep each table they download in this
#' folder, so later sessions do not export it again -- every export counts
#' against the Redivis 30-day export cap. The Python package uses the same
#' folder and files, so a table fetched in either language is reused by the
#' other.
#'
#' The folder is the `IRW_CACHE_DIR` environment variable if set, otherwise
#' `~/.cache/irw` on Linux (or `$XDG_CACHE_HOME/irw`), `~/Library/Caches/irw`
#' on macOS and `%LOCALAPPDATA%/irw/Cache` on Windows.
#'
#' Switch the cache off with `options(irw.cache = FALSE)` for a session, or
#' `IRW_CACHE=0` for all of them.
#'
#' @return Path to the cache folder (it may not exist yet).
#' @seealso [irw_cache_info()], [irw_clear_cache()]
#' @export
irw_cache_dir <- function() {
  override <- Sys.getenv("IRW_CACHE_DIR", "")
  if (nzchar(override)) {
    return(path.expand(override))
  }
  # Computed by hand rather than with tools::R_user_dir(), which adds an R/
  # level the Python package would not find.
  if (.Platform$OS.type == "windows") {
    base <- Sys.getenv("LOCALAPPDATA", "")
    if (!nzchar(base)) base <- file.path(path.expand("~"), "AppData", "Local")
    return(file.path(base, "irw", "Cache"))
  }
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    return(file.path(path.expand("~"), "Library", "Caches", "irw"))
  }
  base <- Sys.getenv("XDG_CACHE_HOME", "")
  if (!nzchar(base)) base <- file.path(path.expand("~"), ".cache")
  file.path(base, "irw")
}

#' Whether fetches read and write the disk cache
#'
#' On unless `options(irw.cache = FALSE)`, or `IRW_CACHE` is 0/false/no/off
#' and the option is unset. Needs arrow, which redivis itself imports, so in
#' practice it is always there when a fetch is possible.
#'
#' @keywords internal
#' @noRd
.irw_cache_enabled <- function() {
  opt <- getOption("irw.cache")
  on <- if (!is.null(opt)) {
    isTRUE(opt)
  } else {
    !tolower(trimws(Sys.getenv("IRW_CACHE", ""))) %in% c("0", "false", "no", "off")
  }
  on && requireNamespace("arrow", quietly = TRUE)
}

.irw_cache_safe <- function(x) {
  # IRW names are already filename-safe; this is a guard, not a mapping the
  # Python side has to reproduce for any real table.
  gsub("[^A-Za-z0-9._-]", "_", x)
}

.irw_cache_path <- function(kind, table, hash) {
  file.path(
    irw_cache_dir(), .irw_cache_format, kind,
    .irw_cache_safe(table), paste0(.irw_cache_safe(hash), ".parquet")
  )
}

#' Name, hash and address of a loaded Redivis table handle, or NULL
#'
#' NULL -- no hash, or a handle that was never `get()`-ed -- means the table
#' cannot be cached, and the fetch goes to Redivis as it always has.
#'
#' @keywords internal
#' @noRd
.irw_cache_identity <- function(tbl) {
  props <- tryCatch(tbl$properties, error = function(e) NULL)
  hash <- props$hash
  name <- props$name
  if (!is.character(hash) || !nzchar(hash) || !is.character(name) || !nzchar(name)) {
    return(NULL)
  }
  reference <- props$qualifiedReference
  if (!is.character(reference)) reference <- ""
  # "datapages.item_response_warehouse_6:fpe6:v2_1.<table>:<id>" -> the
  # dataset key pins are stored under.
  key <- sub(":.*$", "", sub("^[^.]+\\.", "", reference))
  list(
    table = name,
    hash = hash,
    reference = reference,
    pinned = nzchar(key) && !is.null(.irw_pinned_version(key))
  )
}

#' Download a table's rows, from the disk cache when it can
#'
#' @param tbl A Redivis table handle on which `get()` has been called.
#' @param kind `"tables"` or `"itemtext"`.
#' @return A tibble, exactly as `tbl$to_tibble()` would return it: redivis
#'   builds that with `tibble::as_tibble()` on the Arrow table, and so does a
#'   read from the cache.
#' @keywords internal
#' @noRd
.irw_cached_download <- function(tbl, kind) {
  identity <- if (.irw_cache_enabled()) .irw_cache_identity(tbl) else NULL
  if (is.null(identity)) {
    return(tbl$to_tibble())
  }
  path <- .irw_cache_path(kind, identity$table, identity$hash)
  hit <- .irw_cache_read(path)
  if (!is.null(hit)) {
    return(hit)
  }
  arrow_table <- tbl$to_arrow_table()
  .irw_cache_write(path, arrow_table, identity, kind)
  tibble::as_tibble(arrow_table)
}

.irw_cache_read <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    # mmap = FALSE: a memory-mapped file stays open for as long as the tibble
    # built on it lives, and Windows cannot then delete or replace it -- the
    # sweep, irw_clear_cache() and a corrupt-file rewrite all fail quietly.
    tibble::as_tibble(arrow::read_parquet(path, as_data_frame = FALSE, mmap = FALSE)),
    error = function(e) {
      # A truncated write should cost one re-export, not a failed fetch.
      message("Discarding unreadable IRW cache file ", path, "; fetching again.")
      unlink(path)
      NULL
    }
  )
}

#' Store an Arrow table in the cache, then drop older copies of it
#'
#' Never fails: a cache that cannot be written leaves the fetch exactly as it
#' was without one. Written beside its final name and renamed into place, so a
#' reader never sees half a file.
#'
#' @keywords internal
#' @noRd
.irw_cache_write <- function(path, arrow_table, identity, kind) {
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  ok <- tryCatch(
    {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      meta <- list(
        irw_format = .irw_cache_format,
        irw_kind = kind,
        irw_table = identity$table,
        irw_hash = identity$hash,
        irw_reference = identity$reference,
        irw_fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S+00:00", tz = "UTC"),
        irw_writer = "r"
      )
      # Extra schema metadata is ignored by as_tibble(), so stamping the
      # table the caller is about to convert changes nothing it returns.
      for (k in names(meta)) arrow_table$metadata[[k]] <- meta[[k]]
      arrow::write_parquet(arrow_table, tmp)
      if (!file.rename(tmp, path)) stop("could not move the file into place")
      TRUE
    },
    error = function(e) {
      unlink(tmp)
      warning("Could not write IRW cache file ", path, ": ", conditionMessage(e), call. = FALSE)
      FALSE
    }
  )
  if (!ok) {
    return(invisible(FALSE))
  }
  .irw_cache_announce_once()
  # A pinned fetch keeps what is there: the live copy of the same table is
  # still the one an unpinned fetch will want.
  if (!isTRUE(identity$pinned)) {
    .irw_cache_sweep(path, identity$table)
  }
  invisible(TRUE)
}

# Remove other copies of `table` beside `keep`. Matched on the table name
# stored inside each file, not on the folder, so two tables differing only in
# case cannot sweep each other on a case-insensitive filesystem.
.irw_cache_sweep <- function(keep, table) {
  # Compared by file name, not full path: on Windows dirname() turns "\\" into
  # "/", so the listed path of the file just written does not equal `keep`,
  # and a path comparison sweeps away the very copy it meant to keep.
  others <- setdiff(
    list.files(dirname(keep), pattern = "\\.parquet$"),
    basename(keep)
  )
  for (other in file.path(dirname(keep), others)) {
    stored <- .irw_cache_file_meta(other)$table
    if (is.null(stored) || identical(stored, table)) unlink(other)
  }
}

.irw_cache_announce_once <- function() {
  if (isTRUE(.irw_env$cache_announced)) {
    return(invisible(NULL))
  }
  .irw_env$cache_announced <- TRUE
  message(
    "irw: saved a copy of this table in ", irw_cache_dir(), ", so it is not ",
    "exported again while it is unchanged. See irw_cache_info(); switch off ",
    "with options(irw.cache = FALSE) or IRW_CACHE=0."
  )
}

.irw_cache_file_meta <- function(path) {
  meta <- tryCatch(
    {
      # Opened and closed explicitly: a reader left to the garbage collector
      # keeps the file open, and on Windows an open file cannot be deleted.
      file <- arrow::ReadableFile$create(path)
      on.exit(file$close())
      arrow::ParquetFileReader$create(file)$GetSchema()$metadata
    },
    error = function(e) list()
  )
  if (length(meta) == 0 || is.null(names(meta))) {
    return(list())
  }
  meta <- meta[startsWith(names(meta), "irw_")]
  stats::setNames(meta, sub("^irw_", "", names(meta)))
}

.irw_cache_entries <- function() {
  base <- file.path(irw_cache_dir(), .irw_cache_format)
  out <- unlist(lapply(.irw_cache_kinds, function(kind) {
    list.files(file.path(base, kind), pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  }))
  sort(if (is.null(out)) character(0) else out)
}

#' List the cached tables
#'
#' `irw_fetch()` and `irw_itemtext()` keep each table they download so that
#' later sessions do not export it again. A copy is used for as long as the
#' table is unchanged on Redivis, and replaced when it changes. See
#' [irw_cache_dir()] for where the files are and how to switch this off.
#'
#' @return A tibble with one row per cached file: `kind` (`"tables"` or
#'   `"itemtext"`), `table`, `hash` (Redivis' content hash, which decides
#'   whether the copy is current), `version` (the dataset release it was
#'   fetched from), `reference`, `size_mb`, `fetched_at` and `path`.
#' @examples
#' \dontrun{
#' info <- irw_cache_info()
#' sum(info$size_mb)
#' }
#' @export
irw_cache_info <- function() {
  paths <- .irw_cache_entries()
  if (length(paths) == 0 || !requireNamespace("arrow", quietly = TRUE)) {
    return(tibble::tibble(
      kind = character(), table = character(), hash = character(),
      version = character(), reference = character(), size_mb = numeric(),
      fetched_at = character(), path = character()
    ))
  }
  rows <- lapply(paths, function(p) {
    meta <- .irw_cache_file_meta(p)
    ref <- if (is.null(meta$reference)) NA_character_ else meta$reference
    version <- regmatches(ref, regexpr(":v[0-9]+_[0-9]+\\.", ref))
    version <- if (length(version)) gsub("_", ".", substr(version, 2, nchar(version) - 1)) else NA_character_
    tibble::tibble(
      kind = basename(dirname(dirname(p))),
      table = if (is.null(meta$table)) basename(dirname(p)) else meta$table,
      hash = if (is.null(meta$hash)) sub("\\.parquet$", "", basename(p)) else meta$hash,
      version = version,
      reference = ref,
      size_mb = round(file.size(p) / 1e6, 3),
      fetched_at = if (is.null(meta$fetched_at)) NA_character_ else meta$fetched_at,
      path = p
    )
  })
  do.call(rbind, rows)
}

#' Delete cached tables
#'
#' @param table Character vector of table names to remove, matched
#'   case-insensitively; a table's item text goes with it. `NULL` (the
#'   default) removes everything.
#' @return Invisibly, the number of bytes freed.
#' @seealso [irw_cache_info()], [irw_cache_dir()]
#' @export
irw_clear_cache <- function(table = NULL) {
  paths <- .irw_cache_entries()
  if (!is.null(table)) {
    wanted <- tolower(c(table, paste0(table, "__items")))
    stored <- vapply(paths, function(p) {
      t <- if (requireNamespace("arrow", quietly = TRUE)) .irw_cache_file_meta(p)$table else NULL
      if (is.null(t)) basename(dirname(p)) else t
    }, character(1))
    paths <- paths[tolower(stored) %in% wanted]
  } else {
    # Leftovers of writes that died before their rename.
    base <- file.path(irw_cache_dir(), .irw_cache_format)
    paths <- c(paths, list.files(base, pattern = "\\.tmp-[0-9]+$", recursive = TRUE, full.names = TRUE))
  }
  freed <- sum(file.size(paths), na.rm = TRUE)
  unlink(paths)
  # Drop table folders left empty.
  for (kind in .irw_cache_kinds) {
    dirs <- list.dirs(file.path(irw_cache_dir(), .irw_cache_format, kind), recursive = FALSE)
    for (d in dirs) if (length(list.files(d, all.files = TRUE, no.. = TRUE)) == 0) unlink(d, recursive = TRUE)
  }
  invisible(freed)
}
