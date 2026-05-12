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

test_that("irw_filter rejects tag filters on non-core sources", {
  expect_error(
    irw_filter(source = "sim", construct_name = "Big Five", density = NULL),
    "Tag filters are only available for `source = \"core\"`"
  )
})
