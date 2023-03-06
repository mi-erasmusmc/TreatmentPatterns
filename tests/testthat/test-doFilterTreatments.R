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
  expect_error(TreatmentPatterns:::doFilterTreatments())
})

test_that("minimal", {
  treatmentHistoryFiltered <- TreatmentPatterns:::doFilterTreatments(
    treatment_history = doCombinationWindowTH,
    filterTreatments = filterTreatments)
  
  expect_s3_class(treatmentHistoryFiltered, "data.frame")
  expect_true(nrow(treatmentHistoryFiltered) < nrow(treatmentHistory))
})

test_that("invalid_input", {
  expect_error(TreatmentPatterns:::doMaxPathLength(treatment_history = NULL))
  expect_error(TreatmentPatterns:::doMaxPathLength(treatment_history = treatmentHistory,
                                                   filterTreatments = NULL))
  expect_error(TreatmentPatterns:::doMaxPathLength(treatment_history = treatmentHistory,
                                                   filterTreatments = mtcars))
})
