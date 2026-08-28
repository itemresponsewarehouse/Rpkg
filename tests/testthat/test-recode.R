library(testthat)

make_df <- function() {
  data.frame(
    id = c("María-01", "María-01", "René-02", "René-02"),
    item = c("读题能力", "Q_beta", "读题能力", "Q_beta"),
    resp = c(1, 0, 1, 1),
    stringsAsFactors = FALSE
  )
}

test_that("irw_recode round trips through irw_decode", {
  df <- make_df()
  d <- suppressMessages(irw_recode(df))
  out <- suppressMessages(irw_decode(d))
  expect_identical(out$id, df$id)
  expect_identical(out$item, df$item)
  expect_null(attr(out, "irw_recode_key", exact = TRUE))
})

test_that("codes use distinct prefixes and are zero-padded to width 4", {
  df <- make_df()
  d <- suppressMessages(irw_recode(df))
  expect_identical(sort(unique(d$id)), c("P0001", "P0002"))
  expect_identical(sort(unique(d$item)), c("I0001", "I0002"))
  key <- irw_recode_key(d)
  expect_identical(names(key), c("column", "original", "code"))
  expect_identical(sort(unique(key$column)), c("id", "item"))
})

test_that("cols restricts which columns are recoded", {
  df <- make_df()
  d <- suppressMessages(irw_recode(df, cols = "item"))
  expect_identical(d$id, df$id)
  expect_identical(unique(irw_recode_key(d)$column), "item")
})

test_that("prefix overrides the default", {
  df <- make_df()
  d <- suppressMessages(irw_recode(df, cols = "item", prefix = c(item = "Q")))
  expect_true(all(grepl("^Q[0-9]{4}$", d$item)))
})

test_that("NA values survive recode and decode", {
  df <- make_df()
  df$item[2] <- NA
  d <- suppressMessages(irw_recode(df, cols = "item"))
  expect_true(is.na(d$item[2]))
  out <- suppressMessages(irw_decode(d))
  expect_identical(out$item, df$item)
})

test_that("missing columns raise the standard error", {
  df <- make_df()
  df$item <- NULL
  expect_error(irw_recode(df), "Missing required IRW columns: item")
})

test_that("irw_recode_key errors on a plain data frame", {
  expect_error(irw_recode_key(make_df()), "irw_recode\\(\\)")
})

test_that("irw_decode accepts an explicitly supplied key", {
  df <- make_df()
  d <- suppressMessages(irw_recode(df))
  key <- irw_recode_key(d)
  attr(d, "irw_recode_key") <- NULL
  expect_error(irw_decode(d), "No recode key found")
  out <- suppressMessages(irw_decode(d, key = key))
  expect_identical(out$item, df$item)
})

test_that("recoding a second column keeps the earlier key rows", {
  df <- make_df()
  d <- suppressMessages(irw_recode(df, cols = "item"))
  d <- suppressMessages(irw_recode(d, cols = "id"))
  expect_identical(sort(unique(irw_recode_key(d)$column)), c("id", "item"))
})

test_that("wide output of irw_long2resp can be decoded by column name", {
  df <- make_df()
  d <- suppressMessages(irw_recode(df))
  key <- irw_recode_key(d)
  w <- suppressMessages(irw_long2resp(d, id_density_threshold = NULL))
  expect_true(all(grepl("^item_I[0-9]{4}$", setdiff(names(w), "id"))))
  w2 <- suppressMessages(irw_decode(w, key = key))
  expect_setequal(setdiff(names(w2), "id"), paste0("item_", unique(df$item)))
  expect_setequal(w2$id, unique(df$id))
})

test_that("values with unmarked ('unknown') encoding are handled", {
  # Strings read from files or databases are often not encoding-marked, which
  # radix sorting rejects unless they are normalised first.
  df <- make_df()
  Encoding(df$id) <- "unknown"
  Encoding(df$item) <- "unknown"
  d <- suppressMessages(irw_recode(df))
  expect_identical(sort(unique(d$item)), c("I0001", "I0002"))
  expect_identical(suppressMessages(irw_decode(d))$item, enc2utf8(df$item))
})
