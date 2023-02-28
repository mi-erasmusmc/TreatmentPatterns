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
  expect_error(TreatmentPatterns:::doMaxPathLength())
})

test_that("minimal", {
  treatmentHistoryFiltered <- TreatmentPatterns:::doMaxPathLength(
    doFilterTreatmentsTHPP, 
    maxPathLength)
  
  expect_s3_class(treatment_history, "data.frame")
  
  expect_s3_class(treatmentHistoryFiltered, "data.frame")
  expect_true(nrow(treatmentHistoryFiltered) < nrow(treatment_history))
})

test_that("invalid_input", {
  expect_error(TreatmentPatterns:::doMaxPathLength(treatment_history = NULL))
  expect_error(TreatmentPatterns:::doMaxPathLength(treatment_history = treatment_history,
                                                   maxPathLength = NULL))
  expect_error(TreatmentPatterns:::doMaxPathLength(treatment_history = treatment_history,
                                                   maxPathLength = mtcars))
})
