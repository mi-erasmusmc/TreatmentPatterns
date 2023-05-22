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

out <- TreatmentPatterns:::outputDurationEras(
  outputFolder = saveSettings$outputFolder,
  tempFolder = saveSettings$tempFolder,
  databaseName = saveSettings$databaseName,
  studyName = "Viral_Sinusitis",
  eventCohortIds = eventCohortIds,
  groupCombinations = TRUE,
  minCellCount = 5
)

test_that("output", {
  expect_equal(ncol(out), 8)
  expect_equal(typeof(out$event_cohort_name), "character")
  expect_equal(typeof(out$event_seq), "character")
  expect_equal(typeof(out$AVG_DURATION), "double")
  expect_equal(typeof(out$MEDIAN), "double")
  expect_equal(typeof(out$SD), "double")
  expect_equal(typeof(out$MIN), "double")
  expect_equal(typeof(out$MAX), "double")
  expect_equal(typeof(out$COUNT), "double")
})
