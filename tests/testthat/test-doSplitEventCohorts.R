library(TreatmentPatterns)
library(testthat)

source("inst/examples/R Settings Objects/createDummySettings.R")
source("tests/testParams.R")

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
