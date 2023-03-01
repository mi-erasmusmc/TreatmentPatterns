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
  expect_error(TreatmentPatterns:::doEraDuration())
})

test_that("minimal", {
  treatmentHistoryFiltered <- TreatmentPatterns:::doEraDuration(
    treatment_history = treatmentHistory,
    minEraDuration = minEraDuration)
  
  expect_s3_class(treatmentHistoryFiltered, "data.frame")
  expect_true(nrow(treatmentHistoryFiltered) < nrow(treatmentHistory))
})

test_that("invalid_input", {
  expect_error(TreatmentPatterns:::doEraDuration(treatment_history = NULL))
  expect_error(TreatmentPatterns:::doEraDuration(treatment_history = treatmentHistory,
                                                 minEraDuration = NULL))
  expect_error(TreatmentPatterns:::doEraDuration(treatment_history = treatmentHistory,
                                                 minEraDuration = mtcars))
})
