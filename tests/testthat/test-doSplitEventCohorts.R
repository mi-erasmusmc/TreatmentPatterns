library(TreatmentPatterns)
library(testthat)

source(list.files(system.file(
  package = "TreatmentPatterns",
  "examples", "R Settings Objects"), full.names = TRUE))

source(list.files(system.file(
  package = "TreatmentPatterns",
  "testing"), full.names = TRUE))

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
