library(TreatmentPatterns)
library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

outputFile <- "percentageGroupsTreatedNoYear.csv"

test_that("void", {
  expect_error(TreatmentPatterns:::outputTreatedPatients())
})

test_that("minimal", {
  expect_output(TreatmentPatterns:::outputTreatedPatients(
    data = treatment_pathways[[1]],
    eventCohortIds = eventCohortIds,
    groupCombinations = groupCombinations,
    outputFolder = saveSettings$outputFolder,
    outputFile = outputFile), "outputTreatedPatients done")
})
