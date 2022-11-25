# Libraries
library(TreatmentPatterns)
library(testthat)

test_that("Void", {
  expect_error(addPathwaySettings())
})

test_that("Blind parameters", {
  expect_type(addPathwaySettings(targetCohortId = 1, eventCohortIds = 2),
              "list")
})

test_that("Blind parameters", {
  expect_type(addPathwaySettings(1, 2), "list")
})

test_that("Blind parameters", {
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      studyName = "MyVeryCoolStudy",
      includeTreatments = iris,
      periodPriorToIndex = iris,
      minEraDuration = iris,
      splitEventCohorts = iris,
      splitTime = iris,
      eraCollapseSize = iris,
      combinationWindow = iris,
      minPostCombinationDuration = iris,
      filterTreatments = iris,
      maxPathLength = 5,
      minCellCount = iris,
      minCellMethod = iris,
      groupCombinations = iris,
      addNoPaths = iris
    )
  )
})
