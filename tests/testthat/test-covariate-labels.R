## Covariate value labels (ben-domingue/irw#1775). Entirely mocked: the rows
## are copied from metadata/covariate_labels.csv in ben-domingue/irw, the table
## that will be uploaded to irw_meta. It does not exist on Redivis yet, so the
## absent-table path is tested too.

# The real loader, captured before any test mocks it, for the tests that run it
# against a mocked irw_meta dataset.
real_fetch_labels <- irw:::.fetch_covariate_labels_table

withheld <- "[institution name withheld]"

fake_labels <- function() {
  tibble::tibble(
    table = c(rep("cucchi_2018_rfq", 5), rep("estevez_2021_actitu", 4 + 13)),
    covariate = c("cov_gender", "cov_gender", "cov_ethnicity", "cov_ethnicity",
                  "cov_ethnicity", "cov_gender", "cov_gender", "cov_grade",
                  "cov_grade", rep("cov_school", 13)),
    code = c("2", "1", "1", "12", "2", "1", "2", "1", "2", as.character(1:13)),
    label = c("male", "female", "white british", "black caribbean",
              "white irish", "hombre", "mujer", "5ºEP", "6ºEP",
              rep(withheld, 13))
  )
}

mock_labels <- function(env = parent.frame()) {
  local_mocked_bindings(
    .fetch_covariate_labels_table = fake_labels,
    .package = "irw",
    .env = env
  )
}

# Codes ship as doubles, the way Redivis numeric columns arrive.
estevez <- function() {
  data.frame(
    id = rep(1:3, each = 2),
    item = rep(c("a", "b"), 3),
    resp = c(1, 0, 1, 1, 0, 0),
    cov_gender = c(1, 1, 2, 2, NA, NA),
    cov_grade = c(1, 1, 2, 2, 1, 1),
    cov_school = c(3, 3, 7, 7, 3, 3),
    cov_other = c(9, 9, 8, 8, 7, 7),
    stringsAsFactors = FALSE
  )
}

clear_label_cache <- function(env = parent.frame()) {
  ns_env <- irw:::.irw_env
  rm(list = intersect(c("covariate_labels_tibble", "covariate_labels_version"),
                      ls(ns_env)), envir = ns_env)
  withr::defer(
    rm(list = intersect(c("covariate_labels_tibble", "covariate_labels_version"),
                        ls(ns_env)), envir = ns_env),
    envir = env
  )
}

fake_meta_dataset <- function(table_names, rows = NULL) {
  tbls <- lapply(table_names, function(n) list(name = n))
  list(
    properties = list(version = list(tag = "v31.0")),
    list_tables = function() tbls,
    table = function(name) {
      if (is.null(rows)) stop("read attempted on an absent table")
      list(to_tibble = function() rows)
    }
  )
}

## --- irw_covariate_labels() -------------------------------------------------

test_that("an irw_meta without the table is a classed, clear error", {
  clear_label_cache()
  local_mocked_bindings(
    .fetch_covariate_labels_table = real_fetch_labels,
    .irw_open_meta_dataset = function() fake_meta_dataset(c("metadata", "tags")),
    .package = "irw"
  )
  expect_error(irw_covariate_labels("cucchi_2018_rfq"),
               class = "irw_covariate_labels_unavailable")
  expect_error(irw_covariate_labels("cucchi_2018_rfq"),
               "no `covariate_labels` table")
  expect_error(
    suppressMessages(irw_covariates(estevez(), labels = TRUE,
                                    table = "estevez_2021_actitu")),
    class = "irw_covariate_labels_unavailable"
  )
  ## Without labels, irw_covariates() never touches irw_meta.
  expect_true(is.numeric(irw_covariates(estevez())$cov_gender))
})

test_that("a present table is read as character", {
  clear_label_cache()
  rows <- data.frame(table = "t", covariate = "cov_x", code = 1, label = "yes")
  local_mocked_bindings(
    .fetch_covariate_labels_table = real_fetch_labels,
    .irw_open_meta_dataset = function() fake_meta_dataset("covariate_labels", rows),
    .package = "irw"
  )
  out <- irw_covariate_labels("t")
  expect_identical(out$code, "1")
  expect_named(out, c("table", "covariate", "code", "label"))
})

test_that("irw_covariate_labels returns long rows in numeric code order", {
  mock_labels()
  out <- irw_covariate_labels("cucchi_2018_rfq")
  expect_named(out, c("table", "covariate", "code", "label"))
  expect_equal(out$covariate, c(rep("cov_ethnicity", 3), rep("cov_gender", 2)))
  expect_equal(out$code, c("1", "2", "12", "1", "2"))
  expect_equal(out$label[out$covariate == "cov_gender"], c("female", "male"))
})

test_that("irw_covariate_labels is case-insensitive and takes several tables", {
  mock_labels()
  out <- irw_covariate_labels(c("CUCCHI_2018_RFQ", "estevez_2021_actitu"))
  expect_setequal(unique(out$table), c("cucchi_2018_rfq", "estevez_2021_actitu"))
  school <- out[out$covariate == "cov_school", ]
  expect_equal(nrow(school), 13)
  expect_equal(unique(school$label), withheld)
})

test_that("a table without labels gives zero rows and a message", {
  mock_labels()
  expect_message(out <- irw_covariate_labels("no_such_table"),
                 "No covariate value labels")
  expect_equal(nrow(out), 0)
})

## --- irw_covariates(labels = ) ----------------------------------------------

test_that("default output is unchanged", {
  out <- irw_covariates(estevez())
  expect_true(is.numeric(out$cov_gender))
})

test_that("labels = TRUE decodes to factors with the source labels", {
  mock_labels()
  expect_message(
    out <- irw_covariates(estevez(), labels = TRUE, table = "estevez_2021_actitu"),
    "Decoded with source labels: cov_gender, cov_grade"
  )
  expect_true(is.factor(out$cov_gender))
  expect_equal(levels(out$cov_gender), c("hombre", "mujer"))
  expect_equal(as.character(out$cov_gender), c("hombre", "mujer", NA))
  expect_equal(as.character(out$cov_grade), c("5ºEP", "6ºEP", "5ºEP"))
  expect_equal(out$cov_other, c(9, 8, 7))
})

test_that("withheld institutions stay as codes", {
  mock_labels()
  expect_message(
    out <- irw_covariates(estevez(), labels = TRUE, table = "estevez_2021_actitu"),
    "Left as codes"
  )
  expect_equal(out$cov_school, c(3, 7, 3))
})

test_that("codes without a label keep their code text and are reported", {
  mock_labels()
  df <- data.frame(id = 1:4, item = "a", resp = 1,
                   cov_ethnicity = c(1, 12, 16, 2))
  expect_message(
    out <- irw_covariates(df, labels = TRUE, table = "cucchi_2018_rfq"),
    "cov_ethnicity \\(16\\)"
  )
  expect_equal(as.character(out$cov_ethnicity),
               c("white british", "black caribbean", "16", "white irish"))
  expect_equal(levels(out$cov_ethnicity),
               c("white british", "white irish", "black caribbean", "16"))
})

test_that("codes shipped as text decode too", {
  mock_labels()
  df <- data.frame(id = 1:2, item = "a", resp = 1, cov_gender = c("1", "2.0"))
  out <- suppressMessages(irw_covariates(df, labels = TRUE, table = "cucchi_2018_rfq"))
  expect_equal(as.character(out$cov_gender), c("female", "male"))
})

test_that("the same codes mean different things in different tables", {
  mock_labels()
  df <- data.frame(id = 1:2, item = "a", resp = 1, cov_gender = c(1, 2))
  a <- suppressMessages(irw_covariates(df, labels = TRUE, table = "cucchi_2018_rfq"))
  b <- suppressMessages(irw_covariates(df, labels = TRUE, table = "estevez_2021_actitu"))
  expect_equal(as.character(a$cov_gender), c("female", "male"))
  expect_equal(as.character(b$cov_gender), c("hombre", "mujer"))
})

test_that("labels = TRUE needs a table, and one source_table is enough", {
  mock_labels()
  expect_error(irw_covariates(estevez(), labels = TRUE), "pass `table`")
  df <- estevez()
  df$source_table <- "estevez_2021_actitu"
  out <- suppressMessages(irw_covariates(df, labels = TRUE))
  expect_equal(as.character(out$cov_gender), c("hombre", "mujer", NA))
  df$source_table <- rep(c("estevez_2021_actitu", "cucchi_2018_rfq"), each = 3)
  expect_error(irw_covariates(df, labels = TRUE), "several tables")
})

test_that("a labels data frame is used without a network read", {
  local_mocked_bindings(
    .fetch_covariate_labels_table = function() stop("network read"),
    .package = "irw"
  )
  rows <- fake_labels()
  out <- suppressMessages(irw_covariates(
    estevez(), labels = rows[rows$table == "estevez_2021_actitu", ]
  ))
  expect_equal(as.character(out$cov_gender), c("hombre", "mujer", NA))
  expect_error(irw_covariates(estevez(), labels = rows), "several tables")
})

test_that("labels survive align", {
  mock_labels()
  out <- suppressMessages(irw_covariates(
    estevez(), labels = TRUE, table = "estevez_2021_actitu", align = c(3, 2, 99)
  ))
  expect_true(is.factor(out$cov_gender))
  expect_equal(as.character(out$cov_gender), c(NA, "mujer", NA))
  expect_equal(out$id, c(3, 2, 99))
})
