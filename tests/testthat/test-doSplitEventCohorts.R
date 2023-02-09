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
  expect_error(TreatmentPatterns:::doSplitEventCohorts())
})

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::doSplitEventCohorts(
    treatment_history = doEraDurationTH,
    splitEventCohorts = splitEventCohorts,
    splitTime = splitTime,
    outputFolder = saveSettings$outputFolder), "data.frame")
})
