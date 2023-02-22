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
  expect_s3_class(treatment_history <- TreatmentPatterns:::doEraCollapse(
    doSplitEventCohortsTH,
    eraCollapseSize), "data.frame")
})
