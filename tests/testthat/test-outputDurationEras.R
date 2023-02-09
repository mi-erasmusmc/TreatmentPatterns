library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("void", {
  expect_error(TreatmentPatterns:::outputDurationEras())
})

test_that("minimal", {
  expect_output(TreatmentPatterns:::outputDurationEras(
    outputFolder = saveSettings$outputFolder,
    tempFolder = saveSettings$tempFolder,
    databaseName = saveSettings$databaseName,
    studyName = "Viral_Sinusitis",
    eventCohortIds = eventCohortIds,
    groupCombinations = TRUE,
    minCellCount = 5
  ), "outputDurationEras done")
})
