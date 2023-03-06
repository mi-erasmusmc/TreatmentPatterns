library(TreatmentPatterns)
library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "examples", "SettingObjects", "createDummySettings.R"))

source(system.file(
  package = "TreatmentPatterns",
  "testing",
  "testParams.R"))

test_that("void", {
  expect_error(TreatmentPatterns:::doEraCollapse())
})

test_that("minimal", {
  # update generic test variables
  nrows <- nrow(doSplitEventCohortsTH)
  doSplitEventCohortsTH$gap_same <- c(rep(1, nrows/2), rep(2, nrows/2))
  eraCollapseSize <- 1.5
  
  treatmentHistoryFiltered <- TreatmentPatterns:::doEraCollapse(
    doSplitEventCohortsTH,
    eraCollapseSize)
  
  expect_s3_class(treatmentHistory, "data.frame")
  expect_true(nrow(treatmentHistoryFiltered) == 0.5 * nrow(doSplitEventCohortsTH))
})

test_that("invalid_input", {
  expect_error(TreatmentPatterns:::doEraCollapse(treatmentHistory = NULL))
  expect_error(TreatmentPatterns:::doEraCollapse(treatmentHistory = treatment_history,
                                                 eraCollapseSize = NULL))
  expect_error(TreatmentPatterns:::doEraCollapse(treatmentHistory = treatment_history,
                                                 eraCollapseSize = mtcars))
})
