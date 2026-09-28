# Point the on-disk table cache at a throwaway folder for the whole run, so a
# test whose fake table carries a hash never writes into (or reads from) the
# developer's real cache.
withr::local_envvar(
  IRW_CACHE_DIR = tempfile("irw-cache-"),
  IRW_CACHE = NA,
  .local_envir = testthat::teardown_env()
)
withr::local_options(irw.cache = NULL, .local_envir = testthat::teardown_env())

# Keep irw_fetch()'s credit note from querying the live biblio in tests;
# test-source-note.R turns it back on and replaces the lookup itself.
withr::local_envvar(IRW_SOURCE_NOTE = "0", .local_envir = testthat::teardown_env())
