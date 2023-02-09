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
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory())
})

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::doCreateTreatmentHistory(
    current_cohorts = current_cohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments), "data.frame")
})
