# irw_itemtext_licence() reads the repository's itemtext_licences.csv; these
# tests mock the fetched table so they run offline.

fake_licences <- data.frame(
  table = c("iowa_gambling_task", "Some_NC_Table"),
  licence = c("CC BY-SA 4.0", "CC BY-NC 4.0"),
  licence_url = c("https://creativecommons.org/licenses/by-sa/4.0/",
                  "https://creativecommons.org/licenses/by-nc/4.0/"),
  attribution = c("Steingroever et al. (2015)", "Someone (2024)"),
  changes = c("Reformatted by IRW to one row per item.", "Reformatted."),
  basis = c("wording in the deposit", "wording in the article"),
  stringsAsFactors = FALSE
)

test_that("irw_itemtext_licence returns the row, case-insensitively, or NULL", {
  local_irw_binding(".irw_itemtext_licences", function() fake_licences)
  hit <- irw_itemtext_licence("IOWA_GAMBLING_TASK")
  expect_s3_class(hit, "data.frame")
  expect_equal(nrow(hit), 1)
  expect_equal(hit$licence, "CC BY-SA 4.0")
  expect_null(irw_itemtext_licence("gilbert_meta_49"))
  expect_error(irw_itemtext_licence(), "single table name")
})

test_that("a licensed table prints its licence line; an unlicensed one prints nothing", {
  local_irw_binding(".irw_itemtext_licences", function() fake_licences)
  expect_message(.itemtext_licence_message("some_nc_table"), "CC BY-NC 4.0")
  expect_message(.itemtext_licence_message("some_nc_table"), "implies no licence")
  expect_silent(.itemtext_licence_message("gilbert_meta_49"))
})

test_that("a failed fetch warns once and returns NULL rather than 'unlicensed'", {
  local_irw_binding(".irw_itemtext_licences_url", "http://127.0.0.1:9/nope.csv")
  old <- list(.irw_env$itemtext_licences, .irw_env$itemtext_licences_failed)
  .irw_env$itemtext_licences <- NULL
  .irw_env$itemtext_licences_failed <- NULL
  withr::defer({
    .irw_env$itemtext_licences <- old[[1]]
    .irw_env$itemtext_licences_failed <- old[[2]]
  })
  expect_warning(res <- irw_itemtext_licence("iowa_gambling_task"), "Could not fetch")
  expect_null(res)
  expect_silent(irw_itemtext_licence("iowa_gambling_task"))  # warned once already
})
