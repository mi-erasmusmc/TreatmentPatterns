library(TreatmentPatterns)
library(testthat)

source(list.files(system.file(
  package = "TreatmentPatterns",
  "examples", "R Settings Objects"), full.names = TRUE))

source(list.files(system.file(
  package = "TreatmentPatterns",
  "testing"), full.names = TRUE))

test_that("void", {
  expect_error(TreatmentPatterns:::addLabels())
})

test_that("minimal", {
  expect_s3_class(treatment_history <- TreatmentPatterns:::addLabels(
    doMaxPathLengthTH, 
    saveSettings$outputFolder), "data.frame")
})
