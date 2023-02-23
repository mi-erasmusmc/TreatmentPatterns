library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("void", {
  expect_error(
    TreatmentPatterns:::percentageGroupTreated()
  )
})

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::percentageGroupTreated(
    data = treatment_pathways[[1]],
    eventCohortIds = eventCohortIds,
    groupCombinations = groupCombinations,
    outputFolder = saveSettings$outputFolder), "data.frame")
})
