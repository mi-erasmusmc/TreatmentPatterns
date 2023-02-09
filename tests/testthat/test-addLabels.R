library(TreatmentPatterns)
library(testthat)

source(list.files(system.file(
  package = "TreatmentPatterns",
  "examples", "SettingObjects"), full.names = TRUE))

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParams.R"))

test_that("void", {
  expect_error(TreatmentPatterns:::addLabels())
})

test_that("minimal", {
  expect_s3_class(treatment_history <- TreatmentPatterns:::addLabels(
    doMaxPathLengthTH, 
    saveSettings$outputFolder), "data.frame")
})
