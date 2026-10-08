test_that("conj is a known source", {
  expect_identical(irw:::.irw_resolve_source("conj"), "conj")
  expect_identical(irw:::.irw_single_datasource_cache_key("conj"), "conj_datasource")
  expect_true(grepl("^irw_conjoint:", irw:::.irw_datasource_specs$conj[[1]]$dataset))
})

test_that("features without published conj metadata say so", {
  # Not via irw_metadata(): test-collections.R's mock of it outlives that file.
  expect_error(irw:::.irw_conj_not_yet("irw_metadata()"), "not available for the conjoint source")
  expect_error(irw_table_sets("x", source = "conj"), "irw_conj_long")
})

test_that("irw_save_bibtex(source = \"conj\") reads conj_biblio", {
  local_mocked_bindings(
    .fetch_conj_biblio_table = function() tibble::tibble(
      table = "kreps_2020_covid_vaccine", BibTex = "@article{Kreps2020, title={Vaccine}}",
      DOI__for_paper_ = NA_character_),
    .fetch_biblio_table = function() stop("read the core biblio"),
    .fetch_redivis_table = function(...) TRUE)
  f <- withr::local_tempfile(fileext = ".bib")
  out <- suppressMessages(irw_save_bibtex("kreps_2020_covid_vaccine", output_file = f, source = "conj"))
  expect_identical(out, "@article{kreps_2020_covid_vaccine, title={Vaccine}}")
})

test_that("the conj metadata fetcher reads conj_metadata, filtered to live tables", {
  # Not via irw_metadata(): test-collections.R's mock of it outlives that file (Rpkg#187).
  local_irw_binding(".irw_env", new.env())
  read <- character()
  local_mocked_bindings(
    .irw_open_meta_dataset = function() list(
      properties = list(version = list(tag = "v37.1")),
      table = function(name) { read <<- c(read, name)
        list(to_tibble = function() tibble::tibble(table = c("kreps_2020_covid_vaccine", "gone_2020")))}),
    .irw_filter_rows_to_live_tables = function(df, source) {
      expect_identical(source, "conj"); df[df$table != "gone_2020", ] })
  m <- irw:::.fetch_conj_metadata_table()
  expect_identical(read, "conj_metadata")
  expect_identical(m$table, "kreps_2020_covid_vaccine")
})

test_that("irw_info() on a conj table reads conj_biblio", {
  local_mocked_bindings(
    .fetch_redivis_table = function(...) structure(
      list(properties = list(numRows = 10, numBytes = 2048, variableCount = 5, url = "u")),
      dataset_version = "v2.0"),
    .irw_table_variable_names = function(...) c("id", "task"),
    .fetch_conj_biblio_table = function() tibble::tibble(
      table = "kreps_2020_covid_vaccine", DOI__for_paper_ = "10.1/x", URL__for_data_ = "d",
      Derived_License = "CC0 1.0", Custom_License_Terms = NA_character_,
      Description = "Vaccine conjoint", Reference_x = "Kreps et al. (2020)"),
    .fetch_biblio_table = function() stop("read the core biblio"))
  msgs <- capture_messages(irw_info("kreps_2020_covid_vaccine", source = "conj"))
  expect_true(any(grepl("Vaccine conjoint", msgs)))
  expect_true(any(grepl("CC0 1.0", msgs)))
  expect_false(any(grepl("No bibliography row", msgs)))
})

fake_conj_meta <- function() tibble::tibble(
  table = c("a_us_choice", "b_pooled_both", "c_gb_rating", "d_named"),
  n_respondents = c(300, 18000, 900, 2000), n_attributes = c(4, 9, 6, 11),
  outcomes = c("choice", "choice;rating", "rating", "choice_neighbor;rating_neighbor"),
  country = c("US", "AT;DE;GB", "GB", "TR"))
fake_conj_bib <- function() tibble::tibble(
  table = c("a_us_choice", "b_pooled_both", "c_gb_rating", "d_named"),
  Derived_License = c("CC0 1.0", "CC0 1.0", "CC BY 4.0", "CC0 1.0"))

test_that("irw_filter(source = \"conj\") filters on its design metadata", {
  local_mocked_bindings(.fetch_conj_metadata_table = fake_conj_meta,
                        .fetch_conj_biblio_table = fake_conj_bib)
  f <- function(...) suppressMessages(irw_filter(source = "conj", ...))
  expect_identical(f(), c("a_us_choice", "b_pooled_both", "c_gb_rating", "d_named"))
  expect_identical(f(outcome = "rating"), c("b_pooled_both", "c_gb_rating", "d_named"))
  expect_identical(f(outcome = c("choice", "rating")), c("b_pooled_both", "d_named"))
  expect_identical(f(country = "gb"), c("b_pooled_both", "c_gb_rating"))
  expect_identical(f(n_respondents = c(1000, Inf)), c("b_pooled_both", "d_named"))
  expect_identical(f(n_attributes = 9), "b_pooled_both")
  expect_identical(f(license = "CC BY 4.0"), "c_gb_rating")
  expect_identical(f(country = "US", outcome = "rating"), character(0))
  expect_error(f(outcome = "vote"), "choice")
})

test_that("conj filters and other sources' filters do not cross", {
  local_mocked_bindings(.fetch_conj_metadata_table = fake_conj_meta,
                        .fetch_conj_biblio_table = fake_conj_bib)
  expect_error(irw_filter(source = "conj", n_items = c(5, 10)), "not available for `source = \"conj\"`")
  expect_error(irw_filter(source = "conj", construct_type = "x"), "not available for `source = \"conj\"`")
  expect_error(irw_filter(country = "US"), "only available when `source = \"conj\"`")
  expect_error(irw_filter(source = "comp", outcome = "choice"), "only available when `source = \"conj\"`")
})

test_that("irw_license_options(source = \"conj\") reads conj_biblio", {
  local_mocked_bindings(.fetch_conj_biblio_table = fake_conj_bib)
  lo <- irw_license_options(source = "conj")
  expect_identical(lo$license[1], "CC0 1.0")
  expect_identical(lo$count[1], 3L)
})

conj_df <- function() {
  data.frame(id = rep(1:2, each = 4), task = rep(c(1, 1, 2, 2), 2), profile = rep(1:2, 4),
             choice = c(1, 0, 0, 1, 1, 0, 0, 0), rating = c(5, 3, NA, 6, 7, 2, 4, 4),
             attr_party = rep(c("Democrat", "Republican"), 4), cov_age = rep(c(30, 40), each = 4),
             stringsAsFactors = FALSE)
}

test_that("irw_conj_long stacks outcomes into id/item/resp", {
  long <- irw_conj_long(conj_df())
  expect_identical(names(long)[1:5], c("id", "item", "resp", "trial_task", "trial_profile"))
  expect_true(all(c("trial_attr_party", "cov_age") %in% names(long)))
  expect_equal(sum(long$item == "choice"), 8L)
  expect_equal(sum(long$item == "rating"), 7L)       # the NA rating is dropped
  expect_false(any(duplicated(long[, c("id", "item", "trial_task", "trial_profile")])))
})

test_that("irw_conj_long keeps named extra outcomes and can select outcomes", {
  d <- conj_df(); d$choice_effective <- d$choice
  expect_setequal(unique(irw_conj_long(d)$item), c("choice", "rating", "choice_effective"))
  expect_identical(unique(irw_conj_long(d, outcomes = "rating")$item), "rating")
  expect_error(irw_conj_long(d, outcomes = "resp"), "Not outcome columns")
  expect_error(irw_conj_long(d[, -3]), "missing profile")
})
