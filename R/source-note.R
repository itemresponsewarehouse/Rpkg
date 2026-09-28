# Credit note for tables IRW took from an intermediary such as openESM.
#
# A table the IRW found through another collection carries that collection's
# key in the biblio column `Source_via` (ben-domingue/irw#2421). When such a
# table is fetched, irw_fetch() prints the collection's own note and citation
# with message(), once per session per collection -- not once per table, so
# fetching thirty openESM tables prints it once.
#
# A credit line must never break or slow a download meaningfully, so:
#
# - the lookup is one small server-side query, not a download of the whole
#   biblio table, and its answer is kept for a week in irw_cache_dir() as
#   source_via.csv -- a fresh session otherwise pays ~4 s opening irw_meta.
#   The Python package (src/irw/utils/redivis/source_note.py) reads and writes
#   the same file, so keep the two in step;
# - any failure -- no network, no `Source_via` column in an older release,
#   anything -- means no note, silently. A failure is cached too, so it costs
#   one session a week, not every session;
# - only the core source is looked up (every such table is there today).
#
# Silence it with options(irw.source_note = FALSE), suppressMessages(), or
# IRW_SOURCE_NOTE=0.

# The package's copy of metadata/aggregators.csv in ben-domingue/irw, which is
# the source of truth. A key biblio names but this does not know still gets
# the one-line note, without a citation.
.irw_aggregators <- list(
  openESM = list(
    note = paste(
      "These data were found via openESM, where additional metadata for this",
      "dataset are available."
    ),
    citation = paste(
      "Siepe, B. S., Haslbeck, J. M. B., Kloft, M., B\u00fcchner, A., Zhang, Y.,",
      "Fried, E. I., & Heck, D. W. (2026). Introducing openESM: A database of",
      "openly available experience sampling datasets. Behavior Research",
      "Methods, 58(8), 240. https://doi.org/10.3758/s13428-026-03112-y"
    ),
    # irw_save_bibtex() appends this once for any requested table found via
    # openESM. Names per Crossref (ben-domingue/irw#2503).
    bibtex = paste0(
      "@article{siepe2026openesm, title={Introducing openESM: A database ",
      "of openly available experience sampling datasets}, ",
      "author={Siepe, Bj{\\\"o}rn S. and Haslbeck, Jonas M. B. and ",
      "Kloft, Matthias and B{\\\"u}chner, Anabel and Zhang, Yong and ",
      "Fried, Eiko I. and Heck, Daniel W.}, journal={Behavior Research ",
      "Methods}, volume={58}, number={8}, pages={240}, year={2026}, ",
      "doi={10.3758/s13428-026-03112-y}}"
    )
  )
)

# BibTeX entries for the distinct known sources in `sources`, in order.
.irw_aggregator_bibtex <- function(sources) {
  sources <- unique(sources[!is.na(sources) & nzchar(sources)])
  out <- vapply(sources, function(via) {
    entry <- .irw_aggregators[[via]]$bibtex
    if (is.null(entry)) NA_character_ else entry
  }, character(1), USE.NAMES = FALSE)
  out[!is.na(out)]
}

.irw_source_note_cache_file <- "source_via.csv"
.irw_source_note_ttl_seconds <- 7 * 24 * 3600

.irw_source_note_enabled <- function() {
  if (identical(getOption("irw.source_note"), FALSE)) return(FALSE)
  env <- tolower(trimws(Sys.getenv("IRW_SOURCE_NOTE", "")))
  !env %in% c("0", "false", "no", "off")
}

.irw_source_note_path <- function() {
  file.path(irw_cache_dir(), .irw_source_note_cache_file)
}

# data.frame(table, via, url) from the cache file if younger than a week, else NULL.
.irw_source_note_read_cache <- function() {
  tryCatch({
    path <- .irw_source_note_path()
    age <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "secs"))
    if (is.na(age) || age > .irw_source_note_ttl_seconds) return(NULL)
    utils::read.csv(path, colClasses = "character", na.strings = character(0),
                    encoding = "UTF-8")
  }, error = function(e) NULL, warning = function(w) NULL)
}

.irw_source_note_write_cache <- function(lookup) {
  tryCatch({
    path <- .irw_source_note_path()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(path, ".tmp-", Sys.getpid())
    utils::write.csv(lookup[order(lookup$table), , drop = FALSE], tmp,
                     row.names = FALSE, fileEncoding = "UTF-8")
    file.rename(tmp, path)
  }, error = function(e) NULL, warning = function(w) NULL)
  invisible(NULL)
}

.irw_source_note_empty <- function() {
  data.frame(table = character(0), via = character(0), url = character(0),
             stringsAsFactors = FALSE)
}

.irw_source_note_query <- function() {
  tryCatch({
    .irw_require_redivis()
    ref <- .irw_open_meta_dataset()$table("biblio")$qualified_reference
    out <- suppressWarnings(redivis::redivis$query(sprintf(paste(
      "SELECT `table`, Source_via, URL__for_data_ FROM `%s`",
      "WHERE Source_via IS NOT NULL AND TRIM(Source_via) != ''"), ref))$to_tibble())
    url <- as.character(out$URL__for_data_)
    url[is.na(url) | trimws(url) %in% c("", "NA")] <- ""
    data.frame(table = tolower(as.character(out$table)),
               via = trimws(as.character(out$Source_via)),
               url = trimws(url), stringsAsFactors = FALSE)
  }, error = function(e) .irw_source_note_empty())
}

.irw_source_note_lookup <- function() {
  if (!is.null(.irw_env$source_note_lookup)) return(.irw_env$source_note_lookup)
  lookup <- .irw_source_note_read_cache()
  if (is.null(lookup)) {
    lookup <- .irw_source_note_query()
    .irw_source_note_write_cache(lookup)
  }
  .irw_env$source_note_lookup <- lookup
  lookup
}

.irw_source_note_text <- function(table, via, url) {
  entry <- .irw_aggregators[[via]]
  lines <- if (is.null(entry)) {
    sprintf("Note: '%s' was found via %s.", table, via)
  } else {
    sprintf("Note: '%s': %s", table, entry$note)
  }
  if (nzchar(url)) lines <- c(lines, paste("See", url))
  if (!is.null(entry)) lines <- c(lines, paste("Please also cite:", entry$citation))
  c(lines, paste("(shown once per session per source; silence with",
                 "options(irw.source_note = FALSE) or IRW_SOURCE_NOTE=0)"))
}

# Emit the credit note for any fetched table that came via an intermediary.
# Never raises.
.irw_source_note <- function(tables, source = "core") {
  tryCatch({
    if (!identical(source, "core") || !.irw_source_note_enabled()) return(invisible(NULL))
    lookup <- .irw_source_note_lookup()
    for (tbl in tables) {
      i <- match(tolower(tbl), lookup$table)
      if (is.na(i)) next
      via <- lookup$via[i]
      if (via %in% .irw_env$source_note_shown) next
      .irw_env$source_note_shown <- c(.irw_env$source_note_shown, via)
      message(paste(.irw_source_note_text(tbl, via, lookup$url[i]), collapse = "\n"))
    }
    invisible(NULL)
  }, error = function(e) invisible(NULL))
}
