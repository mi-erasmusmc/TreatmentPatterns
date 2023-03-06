library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("void", {
  expect_error(TreatmentPatterns:::inputSunburstPlot())
})

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::inputSunburstPlot(
    treatment_pathways[[1]],
    saveSettings$outputFolder,
    saveSettings$databaseName,
    "Viral_Sinusitis",
    FALSE,
    index_year = 'all'), "data.frame")
})
