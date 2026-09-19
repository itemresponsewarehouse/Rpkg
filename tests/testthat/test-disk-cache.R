# The assertion that matters is on the wire: whether the fake table's download
# methods were called. A test that only compared the returned tibbles would
# pass just as well if every fetch exported the table again.

skip_if_not_installed("arrow")

fake_frame <- function(n = 6) {
  data.frame(
    id = paste0("p", (seq_len(n) - 1) %/% 2),
    item = paste0("q", (seq_len(n) - 1) %% 2),
    resp = as.character(seq_len(n) - 1),
    stringsAsFactors = FALSE
  )
}

# A loaded Redivis table handle that counts its downloads.
fake_table <- function(frame = fake_frame(), name = "t_one", hash = "h1",
                       ref = "datapages.item_response_warehouse:as2e:v5_0") {
  calls <- new.env()
  calls$n <- 0L
  list(
    calls = calls,
    properties = list(
      name = name, hash = hash,
      qualifiedReference = paste0(ref, ".", name, ":abcd")
    ),
    get = function() invisible(NULL),
    to_arrow_table = function(...) {
      calls$n <- calls$n + 1L
      arrow::arrow_table(frame)
    },
    to_tibble = function(...) {
      calls$n <- calls$n + 1L
      tibble::as_tibble(arrow::arrow_table(frame))
    }
  )
}

local_fake_source <- function(tbl, env = parent.frame()) {
  ds <- list(get = function() invisible(NULL), table = function(name) tbl)
  local_mocked_bindings(
    .initialize_datasource = function(source = "core", ...) list(ds),
    .package = "irw",
    .env = env
  )
}

local_empty_cache <- function(env = parent.frame()) {
  withr::local_envvar(IRW_CACHE_DIR = withr::local_tempdir(.local_envir = env), .local_envir = env)
  local_irw_binding(".irw_pinned_version", function(key) NULL, env = env)
  e <- irw:::.irw_env
  e$cache_announced <- TRUE
}

cache_files <- function() {
  sort(list.files(Sys.getenv("IRW_CACHE_DIR"), pattern = "\\.parquet$", recursive = TRUE))
}

test_that("a second fetch reads the disk copy and exports nothing", {
  local_empty_cache()
  tbl <- fake_table()
  local_fake_source(tbl)
  first <- irw:::fetch_single_data("t_one")
  expect_equal(tbl$calls$n, 1L)
  second <- irw:::fetch_single_data("t_one")
  expect_equal(tbl$calls$n, 1L)
  expect_identical(first, second)
  expect_equal(cache_files(), "v1/tables/t_one/h1.parquet")
})

test_that("a cached table matches an uncached fetch, resp coercion included", {
  local_empty_cache()
  tbl <- fake_table()
  local_fake_source(tbl)
  withr::with_options(list(irw.cache = FALSE), expected <- irw:::fetch_single_data("t_one"))
  irw:::fetch_single_data("t_one")
  got <- irw:::fetch_single_data("t_one")
  expect_identical(got, expected)
  expect_type(got$resp, "double")
})

test_that("a changed hash is a miss and replaces the old copy", {
  local_empty_cache()
  local_fake_source(fake_table(hash = "h1"))
  irw:::fetch_single_data("t_one")
  newer <- fake_table(fake_frame(8), hash = "h2")
  local_fake_source(newer)
  out <- irw:::fetch_single_data("t_one")
  expect_equal(newer$calls$n, 1L)
  expect_equal(nrow(out), 8L)
  expect_equal(cache_files(), "v1/tables/t_one/h2.parquet")
})

test_that("a pinned write keeps the live copy", {
  local_empty_cache()
  local_fake_source(fake_table(hash = "h2"))
  irw:::fetch_single_data("t_one")
  local_irw_binding(".irw_pinned_version", function(key) {
    if (identical(key, "item_response_warehouse")) "v4.0" else NULL
  })
  local_fake_source(fake_table(hash = "h1", ref = "datapages.item_response_warehouse:as2e:v4_0"))
  irw:::fetch_single_data("t_one")
  expect_equal(cache_files(), c("v1/tables/t_one/h1.parquet", "v1/tables/t_one/h2.parquet"))
})

test_that("an unreadable file is discarded and the table exported again", {
  local_empty_cache()
  tbl <- fake_table()
  local_fake_source(tbl)
  irw:::fetch_single_data("t_one")
  writeLines("not parquet", file.path(Sys.getenv("IRW_CACHE_DIR"), cache_files()[1]))
  expect_message(out <- irw:::fetch_single_data("t_one"), "unreadable IRW cache file")
  expect_equal(tbl$calls$n, 2L)
  expect_equal(nrow(out), 6L)
})

test_that("a failed write still returns the data", {
  local_empty_cache()
  local_fake_source(fake_table())
  local_mocked_bindings(
    write_parquet = function(...) stop("disk full"),
    .package = "arrow"
  )
  expect_warning(out <- irw:::fetch_single_data("t_one"), "disk full")
  expect_equal(nrow(out), 6L)
  expect_length(cache_files(), 0L)
  expect_length(list.files(Sys.getenv("IRW_CACHE_DIR"), pattern = "tmp-", recursive = TRUE), 0L)
})

test_that("a table without a hash is not cached", {
  local_empty_cache()
  tbl <- fake_table()
  tbl$properties$hash <- NULL
  local_fake_source(tbl)
  irw:::fetch_single_data("t_one")
  irw:::fetch_single_data("t_one")
  expect_equal(tbl$calls$n, 2L)
  expect_length(cache_files(), 0L)
})

test_that("IRW_CACHE=0 switches it off and the option overrides it", {
  local_empty_cache()
  withr::local_envvar(IRW_CACHE = "off")
  tbl <- fake_table()
  local_fake_source(tbl)
  irw:::fetch_single_data("t_one")
  irw:::fetch_single_data("t_one")
  expect_equal(tbl$calls$n, 2L)
  expect_length(cache_files(), 0L)

  withr::local_options(irw.cache = TRUE)
  irw:::fetch_single_data("t_one")
  irw:::fetch_single_data("t_one")
  expect_equal(tbl$calls$n, 3L)
})

test_that("the file carries its provenance", {
  local_empty_cache()
  local_fake_source(fake_table())
  irw:::fetch_single_data("t_one")
  meta <- irw:::.irw_cache_file_meta(file.path(Sys.getenv("IRW_CACHE_DIR"), cache_files()[1]))
  expect_equal(meta$table, "t_one")
  expect_equal(meta$hash, "h1")
  expect_equal(meta$format, "v1")
  expect_equal(meta$writer, "r")
})

test_that("irw_cache_info() and irw_clear_cache()", {
  local_empty_cache()
  local_fake_source(fake_table(name = "t_one", hash = "a"))
  irw:::fetch_single_data("t_one")
  local_fake_source(fake_table(name = "T_Two", hash = "b"))
  irw:::fetch_single_data("T_Two")

  info <- irw_cache_info()
  expect_equal(sort(info$table), c("T_Two", "t_one"))
  expect_equal(unique(info$version), "v5.0")
  expect_gt(irw_clear_cache("t_two"), 0)
  expect_equal(irw_cache_info()$table, "t_one")
  irw_clear_cache()
  expect_equal(nrow(irw_cache_info()), 0L)
  expect_length(list.files(file.path(Sys.getenv("IRW_CACHE_DIR"), "v1", "tables")), 0L)
})

test_that("item text is cached under its own kind and cleared with its table", {
  local_empty_cache()
  tbl <- fake_table(name = "t_one__items")
  ds <- list(table = function(name) tbl)
  local_mocked_bindings(
    irw_list_itemtext_tables = function(...) "t_one",
    .irw_itemtext_table_index = function(...) list(t_one = ds),
    .package = "irw"
  )
  first <- irw:::.fetch_itemtext_table("t_one")
  second <- irw:::.fetch_itemtext_table("t_one")
  expect_equal(tbl$calls$n, 1L)
  expect_identical(first, second)
  expect_equal(cache_files(), "v1/itemtext/t_one__items/h1.parquet")
  irw_clear_cache("t_one")
  expect_length(cache_files(), 0L)
})

test_that("the cache folder follows the platform rule", {
  withr::local_envvar(IRW_CACHE_DIR = NA, XDG_CACHE_HOME = "/xdg")
  if (.Platform$OS.type == "windows" || identical(Sys.info()[["sysname"]], "Darwin")) {
    skip("Linux rule")
  }
  expect_equal(irw_cache_dir(), "/xdg/irw")
  withr::local_envvar(XDG_CACHE_HOME = NA)
  expect_equal(irw_cache_dir(), file.path(path.expand("~"), ".cache", "irw"))
  withr::local_envvar(IRW_CACHE_DIR = "/somewhere")
  expect_equal(irw_cache_dir(), "/somewhere")
})

test_that("R reads a file the Python package wrote", {
  # Written by pyarrow the way disk_cache.write() does: Redivis' Arrow table,
  # no pandas metadata, irw_* keys in the schema metadata.
  local_empty_cache()
  path <- irw:::.irw_cache_path("tables", "t_one", "h1")
  dir.create(dirname(path), recursive = TRUE)
  at <- arrow::arrow_table(id = c(1L, 2L), item = c("a", "b"), resp = c(0L, 1L))
  at$metadata <- list(irw_format = "v1", irw_table = "t_one", irw_hash = "h1", irw_writer = "python")
  arrow::write_parquet(at, path)
  tbl <- fake_table()
  local_fake_source(tbl)
  out <- irw:::fetch_single_data("t_one")
  expect_equal(tbl$calls$n, 0L)
  expect_equal(out$id, c(1L, 2L))
})

test_that("the sweep keeps the file it was given even when its path is spelled differently", {
  # On Windows dirname() rewrites "\" as "/", so the listed path of the file
  # just written differed from the path it was written to, and the sweep
  # deleted it. dirname() dropping a doubled "/" reproduces that on any OS.
  local_empty_cache()
  local_fake_source(fake_table(hash = "h1"))
  irw:::fetch_single_data("t_one")
  keep <- file.path(Sys.getenv("IRW_CACHE_DIR"), cache_files()[1])
  respelled <- paste0(dirname(keep), "//", basename(keep))
  irw:::.irw_cache_sweep(respelled, "t_one")
  expect_equal(cache_files(), "v1/tables/t_one/h1.parquet")
})

test_that("metadata of a missing file is empty, not an error", {
  expect_equal(irw:::.irw_cache_file_meta(tempfile(fileext = ".parquet")), list())
})
