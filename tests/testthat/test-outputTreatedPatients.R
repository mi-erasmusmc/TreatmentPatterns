library(TreatmentPatterns)
library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

outputFile <- tempfile(pattern = paste0(
  saveSettings$databaseName,
  "_Viral_Sinusitis",
  "_percentage_groups_treated_noyear"),
  fileext = ".csv"
)

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
