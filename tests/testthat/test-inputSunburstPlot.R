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
    data = treatment_pathways[[1]],
    tempFolder = saveSettings$tempFolder,
    outputFolder = saveSettings$outputFolder,
    databaseName = saveSettings$databaseName,
    studyName = "Viral_Sinusitis",
    addNoPaths = TRUE,
    index_year = 'all'), "data.frame")
})