# Libraries
library(TreatmentPatterns)
library(testthat)

test_that("Void", {
  expect_error(addPathwaySettings())
})

test_that("Blind parameters", {
  result <- addPathwaySettings(targetCohortId = 1, eventCohortIds = 2)
  
  expect_type(result, "list")
  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 17)
  expect_equal(colnames(result), c("studyName", "targetCohortId", "eventCohortIds", "includeTreatments", "periodPriorToIndex",
                                   "minEraDuration", "splitEventCohorts", "splitTime", "eraCollapseSize", "combinationWindow",
                                   "minPostCombinationDuration", "filterTreatments", "maxPathLength", "minCellCount", "minCellMethod",
                                   "groupCombinations", "addNoPaths"))
  expect_equal(result$studyName, "name_unknown")
  expect_equal(result$targetCohortId, 1)
  expect_equal(result$eventCohortIds, "2")
  expect_equal(result$includeTreatments, "startDate")
  expect_equal(result$periodPriorToIndex, 0)
  expect_equal(result$minEraDuration, 0)
  expect_equal(result$splitEventCohorts, "")
  expect_equal(result$splitTime, 30)
  expect_equal(result$eraCollapseSize, 30)
  expect_equal(result$combinationWindow, 30)
  expect_equal(result$minPostCombinationDuration, 30)
  expect_equal(result$filterTreatments, "First")
  expect_equal(result$maxPathLength, 5)
  expect_equal(result$minCellCount, 5)
  expect_equal(result$minCellMethod, "Remove")
  expect_equal(result$groupCombinations, 10)
  expect_equal(result$addNoPaths, TRUE)
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


test_that("targetCohortId", {
  # double
  expect_error(
    addPathwaySettings(
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
    ),
    "Must be of type 'character'"
  )
})

test_that("eventCohortIds", {
  # double
  expect_error(
    addPathwaySettings(targetCohortId = 1,
                       eventCohortIds = "2"),
    "Must be of type 'numeric'"
  )
})

test_that("studyName", {
  # chr
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      studyName = TRUE
    ),
    "Must be of type 'character'"
  )
})

test_that("includeTreatments", {
  # chr
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      includeTreatments = "2"
    ),
    "Must be a subset of \\{'startDate','endDate'\\}"
  )
})

test_that("periodPriorToIndex", {
  # double
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      periodPriorToIndex = "2"
    ),
    "Must be of type 'numeric'"
  )
})

test_that("minEraDuration", {
  # double
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      minEraDuration = "2"
    ),
    "Must be of type 'numeric'"
  )
})

test_that("splitEventCohorts", {
  # chr
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      splitEventCohorts = 2
    ),
    "Must be of type 'character'"
  )
})

test_that("splitTime", {
  # double
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      splitTime = "2"
    ),
    "Must be of type 'numeric'"
  )
})

test_that("eraCollapseSize", {
  # double
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      eraCollapseSize = "2"
    ),
    "Must be of type 'numeric'"
  )
})

test_that("combinationWindow", {
  # double
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      combinationWindow = "2"
    ),
    "Must be of type 'numeric'"
  )
})

test_that("minPostCombinationDuration", {
  # double
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      minPostCombinationDuration = "2"
    ),
    "Must be of type 'numeric'"
  )
})

test_that("filterTreatments", {
  # chr
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      filterTreatments = "2"
    ),
    "Must be a subset of \\{'First','Changes','All'\\}"
  )
})

test_that("maxPathLength", {
  # double
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      maxPathLength = "2"
    ),
    "Must be of type 'numeric'"
  )
})

test_that("minCellCount", {
  # double
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      minCellCount = "2"
    ),
    "Must be of type 'numeric'"
  )
})

test_that("minCellMethod", {
  # chr
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      minCellMethod = 2
    ),
    "Must be of type 'character'"
  )
})

test_that("groupCombinations", {
  # double
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      groupCombinations = "2"
    ),
    "Must be of type 'numeric', not 'character'"
  )
})

test_that("addNoPaths", {
  # logical
  expect_error(
    addPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = 2,
      addNoPaths = "2"
    ),
    "Must be of type 'logical'"
  )
})
