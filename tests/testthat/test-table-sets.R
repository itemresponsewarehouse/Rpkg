# irw_table_sets() never exports, so these tests stand in for Redivis at the
# three internal seams it calls. Mocks go through local_irw_binding() because
# local_mocked_bindings() does not reliably restore in this package's setup
# (see helper-redivis.R), and a leaked .irw_query_tibble mock would break
# test-require-redivis.R.
local_fake_warehouse <- function(tables, env = parent.frame()) {
  looked_up <- character(0)
  local_irw_binding(".fetch_redivis_table", function(name, source = "core", ...) {
    if (!name %in% names(tables)) stop(sprintf("Table '%s' not found.", name), call. = FALSE)
    looked_up <<- c(looked_up, name)
    list(qualified_reference = paste0("ref.", name))
  }, env = env)
  local_irw_binding(".irw_table_variable_names", function(tbl) c("id", "item", "resp"), env = env)
  local_irw_binding(".irw_query_tibble", function(sql) {
    ref <- sub(".*FROM `ref\\.([^`]+)`.*", "\\1", sql)
    tab <- tables[[ref]]
    if (grepl("COUNT\\(\\*\\) AS n FROM", sql)) {
      return(tibble::tibble(n = nrow(tab)))
    }
    if (grepl("GROUP BY item", sql)) {
      return(tibble::tibble(item = sort(unique(tab$item)), n = as.vector(table(tab$item)),
                            resp_min = NA_real_, resp_max = NA_real_, n_resp_levels = NA_integer_))
    }
    if (grepl("AS item FROM", sql)) {
      return(tibble::tibble(item = sort(unique(tab$item))))
    }
    tibble::tibble(resp = sort(unique(tab$resp)))
  }, env = env)
  environment()
}

TABLES <- list(
  a = data.frame(item = c("q1", "q2", "q1"), resp = c("0", "1", "1")),
  b = data.frame(item = c("x"), resp = c("3"))
)

test_that("a single name returns the single-table list, unchanged", {
  local_fake_warehouse(TABLES)
  out <- irw_table_sets("a")
  expect_named(out, c("table", "n_rows", "items", "resp", "per_item"))
  expect_equal(out$table, "ref.a")
  expect_equal(out$n_rows, 3)
  expect_equal(out$items, c("q1", "q2"))
  expect_equal(out$resp, c(0, 1))
  expect_null(out$per_item)
})

test_that("several names return a named list of single-table results, in order", {
  local_fake_warehouse(TABLES)
  out <- irw_table_sets(c("b", "a"), per_item = TRUE)
  expect_named(out, c("b", "a"))
  expect_equal(out$a, irw_table_sets("a", per_item = TRUE))
  expect_equal(out$b$items, "x")
  expect_equal(nrow(out$a$per_item), 2)
})

test_that("a missing table in a sweep fails with that table's name", {
  local_fake_warehouse(TABLES)
  expect_error(irw_table_sets(c("a", "nope")), "nope", fixed = TRUE)
})

test_that("name must be one or more non-missing strings", {
  expect_error(irw_table_sets(character(0)), "one or more table names", fixed = TRUE)
  expect_error(irw_table_sets(c("a", NA)), "one or more table names", fixed = TRUE)
  expect_error(irw_table_sets(1), "one or more table names", fixed = TRUE)
})
