library(usethis)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParams.R"))


test_that("void", {
  expect_error(
    TreatmentPatterns:::selectRowsCombinationWindow())
})

test_that("minimal", {
  expect_s3_class(
    TreatmentPatterns:::selectRowsCombinationWindow(doEraCollapseTH),
    "data.frame")
})

test_that("validate GAP_PREVIOUS", {
  treatmentHistoryCW <- TreatmentPatterns:::selectRowsCombinationWindow(
    doEraCollapseTH)

  x <- treatmentHistoryCW$GAP_PREVIOUS
  y <- treatmentHistoryCW[, GAP_PREVIOUS := difftime(
    event_start_date,
    data.table::shift(event_end_date, type = "lag"), units = "days"),
    by = person_id]
  y <- y$GAP_PREVIOUS
  y <- as.integer(y)

  expect_true(identical(x, y))
})

test_that("validate SELECTED_ROWS", {
  treatmentHistoryCW <- TreatmentPatterns:::selectRowsCombinationWindow(
    doEraCollapseTH)

  # 0 NULL / NA
  expect_equal(sum(is.na(treatmentHistoryCW$SELECTED_ROWS)), 0)
  # Min: 0
  expect_equal(min(treatmentHistoryCW$SELECTED_ROWS), 0)
  # Max: 1
  expect_equal(max(treatmentHistoryCW$SELECTED_ROWS), 1)
})
