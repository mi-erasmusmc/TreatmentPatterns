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

test_that("Parameters order", {
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

# Inverse test
## Add pathwaySettings
pathwaySettingsNum <- addPathwaySettings(
  targetCohortId = 1,
  eventCohortIds = 2,
  studyName = 1,
  includeTreatments = 2,
  periodPriorToIndex = "0",
  minEraDuration = "0",
  splitEventCohorts = 3,
  splitTime = "30",
  eraCollapseSize = "30",
  combinationWindow = "30",
  minPostCombinationDuration = "30",
  filterTreatments = 1,
  maxPathLength = "5",
  minCellCount = "5",
  minCellMethod = 0,
  groupCombinations = "10",
  addNoPaths = 3.1
)

test_that("targetCohortId", {
  # double
  expect_type(pathwaySettingsNum$targetCohortId, "double")
})

test_that("eventCohortIds", {
  # double
  expect_type(pathwaySettingsNum$eventCohortIds, "double")
})

test_that("studyName", {
  # chr
  expect_warning(expect_type(pathwaySettingsNum$studyName, "character"))
})

test_that("includeTreatments", {
  # chr
  expect_warning(expect_type(pathwaySettingsNum$includeTreatments, "character"))
})

test_that("periodPriorToIndex", {
  # double
  expect_warning(expect_type(pathwaySettingsNum$periodPriorToIndex, "double"))
})

test_that("minEraDuration", {
  # double
  expect_warning(expect_type(pathwaySettingsNum$minEraDuration, "double"))
})

test_that("splitEventCohorts", {
  # chr
  expect_warning(expect_type(pathwaySettingsNum$splitEventCohorts, "character"))
})

test_that("splitTime", {
  # double
  expect_warning(expect_type(pathwaySettingsNum$splitTime, "double"))
})

test_that("eraCollapseSize", {
  # double
  expect_warning(expect_type(pathwaySettingsNum$eraCollapseSize, "double"))
})

test_that("combinationWindow", {
  # double
  expect_warning(expect_type(pathwaySettingsNum$combinationWindow, "double"))
})

test_that("minPostCombinationDuration", {
  # double
  expect_warning(expect_type(pathwaySettingsNum$minPostCombinationDuration, "double"))
})

test_that("filterTreatments", {
  # chr
  expect_warning(expect_type(pathwaySettingsNum$filterTreatments, "character"))
})

test_that("maxPathLength", {
  # double
  expect_warning(expect_type(pathwaySettingsNum$maxPathLength, "double"))
})

test_that("minCellCount", {
  # double
  expect_warning(expect_type(pathwaySettingsNum$minCellCount, "double"))
})

test_that("minCellMethod", {
  # chr
  expect_warning(expect_type(pathwaySettingsNum$minCellMethod, "character"))
})

test_that("groupCombinations", {
  # double
  expect_warning(expect_type(pathwaySettingsNum$groupCombinations, "double"))
})

test_that("addNoPaths", {
  # logical
  expect_warning(expect_type(pathwaySettingsNum$addNoPaths, "logical"))
})
