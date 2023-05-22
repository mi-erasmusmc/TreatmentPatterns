library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("void", {
  expect_error(TreatmentPatterns:::preprocessSunburstData())
})

test_that("minimal", {
  expect_output(TreatmentPatterns:::preprocessSunburstData(
    data = treatmentPathways[[1]],
    tempFolder = saveSettings$tempFolder,
    outputFolder = saveSettings$outputFolder,
    databaseName = saveSettings$databaseName,
    studyName = "Viral_Sinusitis",
    eventCohortIds = eventCohortIds,
    addNoPaths = FALSE
  ), "preprocessSunburstData done")
})
