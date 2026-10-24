library(testthat)

test_that("irw_license_options routes source-specific bibliography tables", {
  local_mocked_bindings(
    .fetch_simsyn_biblio_table = function() {
      data.frame(
        table = c("sim_a", "sim_b", "sim_c"),
        Derived_License = c("CC BY 4.0", "CC0 1.0", "CC BY 4.0"),
        stringsAsFactors = FALSE
      )
    },
    .fetch_nominal_biblio_table = function() {
      data.frame(
        table = c("nom_a", "nom_b"),
        Derived_License = c("ODC-By 1.0", "ODC-By 1.0"),
        stringsAsFactors = FALSE
      )
    },
    .fetch_comps_biblio_table = function() {
      data.frame(
        table = c("comp_a", "comp_b"),
        Derived_License = c("CC BY 4.0", "CC0 1.0"),
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("irw")
  )

  expect_equal(
    irw_license_options(source = "sim"),
    data.frame(
      license = c("CC BY 4.0", "CC0 1.0"),
      count = c(2L, 1L)
    )
  )

  expect_equal(
    irw_license_options(source = "nom"),
    data.frame(
      license = "ODC-By 1.0",
      count = 2L
    )
  )

  expect_equal(
    irw_license_options(source = "comp"),
    data.frame(
      license = c("CC BY 4.0", "CC0 1.0"),
      count = c(1L, 1L)
    )
  )
})

test_that("irw_filter applies construct_name filters and reports empty matches", {
  local_mocked_bindings(
    irw_metadata = function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
      expect_identical(source, "core")
      data.frame(
        table = c("tab_big_five", "tab_other"),
        density = c(0.8, 0.8),
        stringsAsFactors = FALSE
      )
    },
    .fetch_tags_table = function() {
      data.frame(
        table = c("tab_big_five", "tab_other"),
        construct_name = c("Big Five", "Other"),
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("irw")
  )

  expect_equal(
    irw_filter(construct_name = "Big Five", density = NULL),
    "tab_big_five"
  )

  expect_message(
    expect_equal(
      irw_filter(construct_name = "Not Here", density = NULL),
      character(0)
    ),
    "0 tables matched construct_name = 'Not Here'\\."
  )
})

test_that("irw_filter routes simulation and competition sources", {
  local_mocked_bindings(
    irw_metadata = function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
      expect_identical(source, "sim")
      data.frame(
        table = c("sim_keep", "sim_drop"),
        variables = c("resp|wave", "resp"),
        stringsAsFactors = FALSE
      )
    },
    .fetch_simsyn_biblio_table = function() {
      data.frame(
        table = c("sim_keep", "sim_drop"),
        Derived_License = c("CC0 1.0", "CC BY 4.0"),
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("irw")
  )

  expect_equal(
    irw_filter(source = "sim", var = "wave", license = "CC0 1.0", density = NULL),
    "sim_keep"
  )

  local_mocked_bindings(
    .fetch_comps_metadata_table = function() {
      data.frame(
        table = c("comp_a", "comp_b"),
        n_responses = c(100, 200),
        n_actors = c(2, 4),
        stringsAsFactors = FALSE
      )
    },
    .fetch_comps_biblio_table = function() {
      data.frame(
        table = c("comp_a", "comp_b"),
        Derived_License = c("CC0 1.0", "CC BY 4.0"),
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("irw")
  )

  expect_equal(
    irw_filter(source = "comp", n_actors = 4, license = "CC BY 4.0"),
    "comp_b"
  )

  expect_equal(
    irw_filter_comp(n_actors = 4, license = "CC BY 4.0"),
    "comp_b"
  )
})

test_that("irw_filter rejects tag filters on untagged sources", {
  # comp and sim deliberately have no tags -- see inst/developer/tags.md
  expect_error(
    irw_filter(source = "sim", construct_name = "Big Five", density = NULL),
    "Tag filters are only available for `source` in"
  )

  expect_error(
    irw_filter(source = "comp", construct_name = "Big Five"),
    "not available for `source = \"comp\"`"
  )
})

test_that("irw_filter accepts tag filters on nominal", {
  local_mocked_bindings(
    .fetch_nominal_tags_table = function() {
      data.frame(
        table = c("nom_a", "nom_b"),
        construct_type = c("Cognitive/educational", "Personality"),
        stringsAsFactors = FALSE
      )
    },
    irw_metadata = function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
      expect_identical(source, "nom")
      data.frame(
        table = c("nom_a", "nom_b"),
        n_responses = c(100, 200),
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("irw")
  )

  expect_equal(
    irw_filter(source = "nom", construct_type = "Cognitive/educational", density = NULL),
    "nom_a"
  )

  expect_message(
    expect_equal(
      irw_filter(source = "nom", construct_type = "Not Here", density = NULL),
      character(0)
    ),
    "0 tables matched construct_type = 'Not Here'\\."
  )
})

test_that("tags dispatch errors for untagged sources", {
  expect_error(irw_tags(source = "comp"), "Tags are not available")
  expect_error(irw_tag_options("sample", source = "sim"), "Tags are not available")
})

test_that(".irw_license_terms treats blanks and literal NA as no terms", {
  bib <- data.frame(
    table = c("a", "b", "c", "d"),
    Derived_License = c("Custom", "Custom", "CC BY 4.0", "Custom"),
    Custom_License_Terms = c("Academic use only.", "NA", " Attribute separately. ", ""),
    stringsAsFactors = FALSE
  )
  expect_identical(
    irw:::.irw_license_terms(bib),
    c("Academic use only.", NA, "Attribute separately.", NA)
  )
  # Pinned versions from before the column existed
  expect_identical(irw:::.irw_license_terms(bib[, 1:2]), rep(NA_character_, 4))
})

test_that("irw_license_options says how many Custom tables record their terms", {
  local_mocked_bindings(
    .fetch_simsyn_biblio_table = function() {
      data.frame(
        table = c("sim_a", "sim_b", "sim_c"),
        Derived_License = c("Custom", "Custom", "CC BY 4.0"),
        Custom_License_Terms = c("Academic use only.", "NA", "NA"),
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("irw")
  )
  expect_message(
    out <- irw_license_options(source = "sim"),
    "1 of 2 tables marked Custom record their terms",
    fixed = TRUE
  )
  expect_equal(out, data.frame(license = c("Custom", "CC BY 4.0"), count = c(2L, 1L)))
})

test_that("irw_filter skips the density default when nom metadata has no density", {
  local_mocked_bindings(
    irw_metadata = function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
      data.frame(table = c("nom_b", "nom_a"), n_items = c(5, 20), stringsAsFactors = FALSE)
    },
    .env = asNamespace("irw")
  )

  expect_equal(irw_filter(source = "nom"), c("nom_a", "nom_b"))
  expect_equal(irw_filter(source = "nom", n_items = c(10, Inf)), "nom_a")
  expect_error(irw_filter(source = "nom", density = c(0.3, 1)),
               "not available for `source = \"nom\"`.*density")
  expect_error(irw_filter(source = "nom", var = "wave", longitudinal = TRUE),
               "var, longitudinal", fixed = TRUE)
})

##Tier-0 psychometric filters (ben-domingue/irw#1710). These columns differ from
##every other numeric filter in one way that matters: they can be NA, because a
##table whose responses are all one value has no difficulty spread and a
##single-item table has no rest score. `metadata[NA, ]` does not drop a row, it
##invents an all-NA one, so the NA guard is the thing these tests are really about.
.tier0_metadata <- function() {
  data.frame(
    table = c("tab_coherent", "tab_reverse_keyed", "tab_floor", "tab_constant"),
    n_responses = c(1000, 1000, 1000, 1000),
    n_categories = c(2, 5, 2, 1),
    n_participants = c(100, 100, 100, 100),
    n_items = c(10, 10, 10, 10),
    responses_per_participant = c(10, 10, 10, 10),
    responses_per_item = c(100, 100, 100, 100),
    density = c(0.9, 0.9, 0.9, 0.9),
    difficulty_spread = c(0.6, 0.5, 0.2, NA),
    mean_item_rest_r = c(0.55, 0.01, 0.30, NA),
    mean_abs_item_rest_r = c(0.55, 0.42, 0.30, NA),
    prop_items_extreme = c(0, 0, 0.8, NA),
    variables = c("id| item| resp", "id| item| resp", "id| item| resp", "id| item| resp"),
    stringsAsFactors = FALSE
  )
}

test_that("tier-0 filters select on psychometric properties", {
  local_mocked_bindings(
    irw_metadata = function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) .tier0_metadata(),
    .env = asNamespace("irw")
  )

  expect_equal(irw_filter(mean_abs_item_rest_r = c(0.4, 1)),
               c("tab_coherent", "tab_reverse_keyed"))
  expect_equal(irw_filter(prop_items_extreme = c(0, 0)),
               c("tab_coherent", "tab_reverse_keyed"))
  expect_equal(irw_filter(difficulty_spread = c(0.55, 1)), "tab_coherent")
})

test_that("a reverse-keyed scale is found by the gap between signed and absolute r", {
  local_mocked_bindings(
    irw_metadata = function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) .tier0_metadata(),
    .env = asNamespace("irw")
  )

  expect_equal(
    irw_filter(mean_item_rest_r = c(-1, 0.1), mean_abs_item_rest_r = c(0.3, 1)),
    "tab_reverse_keyed"
  )
})

test_that("tables whose tier-0 columns are NA are dropped, not returned as NA rows", {
  local_mocked_bindings(
    irw_metadata = function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) .tier0_metadata(),
    .env = asNamespace("irw")
  )

  out <- irw_filter(difficulty_spread = c(0, 1))
  expect_false(any(is.na(out)))
  expect_false("tab_constant" %in% out)
  expect_length(out, 3L)
})

test_that("a tier-0 filter against metadata that predates the columns errors", {
  local_mocked_bindings(
    irw_metadata = function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
      m <- .tier0_metadata()
      m[, setdiff(names(m), c("difficulty_spread", "mean_item_rest_r",
                              "mean_abs_item_rest_r", "prop_items_extreme"))]
    },
    .env = asNamespace("irw")
  )

  expect_error(irw_filter(difficulty_spread = c(0, 1)),
               "not a column of the published metadata")
})

test_that("tier-0 filters are rejected for sources whose metadata lacks them", {
  local_mocked_bindings(
    irw_metadata = function(source = "core", sim = FALSE, comp = FALSE, nom = FALSE) {
      data.frame(table = c("nom_a", "nom_b"), n_responses = c(10, 20),
                 stringsAsFactors = FALSE)
    },
    .env = asNamespace("irw")
  )

  expect_error(irw_filter(source = "nom", mean_abs_item_rest_r = c(0.4, 1)),
               "mean_abs_item_rest_r")
  expect_error(irw_filter(source = "comp", difficulty_spread = c(0, 1)),
               "difficulty_spread")
  expect_error(irw_filter(source = "conj", prop_items_extreme = c(0, 0)),
               "prop_items_extreme")
})


# A type error must not surface as an empty result (review R-22, irw#3221).
# `irw_filter(n_responses = "abc")` reported "0 tables matched", which reads as
# a finding about the corpus rather than a mistake in the call -- and the Python
# client raises for the same input. Checked before any metadata is fetched, so
# these need no network and no mocks.
test_that("a non-numeric numeric filter errors rather than matching nothing", {
  expect_error(irw_filter(n_responses = "abc"), "must be numeric")
  expect_error(irw_filter(n_items = TRUE), "must be numeric")
  expect_error(irw_filter(density = "0.5"), "must be numeric")
  expect_error(irw_filter(n_participants = c(1, 2, 3)), "single value or")
  expect_error(irw_filter(n_items = NA_real_), "cannot be NA")
})

test_that("the check is the same one for every source's numeric filters", {
  expect_error(irw_filter(n_actors = "many", source = "comp"), "must be numeric")
  expect_error(irw_filter(n_respondents = "many", source = "conj"), "must be numeric")
  expect_null(irw:::.irw_check_numeric_filter("n_items", NULL))
  expect_null(irw:::.irw_check_numeric_filter("n_items", c(10, 50)))
})
