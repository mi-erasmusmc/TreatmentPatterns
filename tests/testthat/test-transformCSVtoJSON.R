library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

data <- TreatmentPatterns:::inputSunburstPlot(
  treatment_pathways[[1]],
  saveSettings$outputFolder,
  saveSettings$databaseName,
  "Viral_Sinusitis",
  FALSE,
  index_year = 'all')

test_that("void", {
  expect_error(TreatmentPatterns:::transformCSVtoJSON())
})

test_that("minimal", {
  expect_no_error(
    TreatmentPatterns:::transformCSVtoJSON(
      data = data,
      outcomes = 3,
      folder = "output",
      file_name = "stuff.json"))
})
