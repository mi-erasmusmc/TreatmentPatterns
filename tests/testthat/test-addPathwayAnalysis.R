library(TreatmentPatterns)
library(testthat)

source(list.files(system.file(
  package = "TreatmentPatterns",
  "examples", "SettingObjects"), full.names = TRUE))

test_that("void", {
  expect_error(addPathwayAnalysis())
})

test_that("minimal", {
  expect_no_error(addPathwayAnalysis(
    pathwaySettings = pathwaySettings,
    targetCohortIds = c(1),
    eventCohortIds = c(2,3)))
})

test_that("some settings", {
  expect_s3_class(addPathwayAnalysis(
    pathwaySettings = pathwaySettings,
    targetCohortIds = c(1),
    eventCohortIds = c(2,3),
    studyName = "stuff",
    minEraDuration = 3,
    maxPathLength = 2), "pathwaySettings")
})
