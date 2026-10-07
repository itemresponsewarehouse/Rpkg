test_that("conj is a known source", {
  expect_identical(irw:::.irw_resolve_source("conj"), "conj")
  expect_identical(irw:::.irw_single_datasource_cache_key("conj"), "conj_datasource")
  expect_true(grepl("^irw_conjoint:", irw:::.irw_datasource_specs$conj[[1]]$dataset))
})

test_that("features without published conj metadata say so", {
  # Not via irw_metadata(): test-collections.R's mock of it outlives that file.
  expect_error(irw:::.irw_conj_not_yet("irw_metadata()"), "not available for the conjoint source")
  expect_error(irw:::.irw_filter_biblio("conj"), "not available for the conjoint source")
  expect_error(irw_table_sets("x", source = "conj"), "irw_conj_long")
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
