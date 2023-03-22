# Libraries
library(testthat)
library(TreatmentPatterns)

targetCohorts <- data.frame(cohortId = c(1),
                            cohortName = c("Hypertension"))

eventCohorts <- data.frame(
  cohortId = c(10, 11, 12, 13, 14),
  cohortName = c(
    "Hydrochlorothiazide",
    "Metorolol",
    "Amlodipine",
    "Lisinopril",
    "Losartan"
  )
)

cohortSettings <- createCohortSettings(
  targetCohorts,
  eventCohorts)

test_that("Void", {
  expect_error(createPathwaySettings())
})

test_that("Minimal", {
  expect_s3_class(createPathwaySettings(cohortSettings),
                  "pathwaySettings")
})

# studyName
test_that("studyName", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = 3),
    "Must be of type 'character'")
})

# targetCohortId
test_that("targetCohortId", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "myStudyName",
    targetCohortId = 1),
    "multiple actual arguments")
})

# eventCohortIds
test_that("eventCohortIds", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "myStudyName",
    targetCohortId = 1),
    "multiple actual arguments")
})

# includeTreatments
test_that("includeTreatments", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "stuff"),
    "Must be a subset of \\{'startDate','endDate'\\}")
})

# periodPriorToIndex
test_that("periodPriorToIndex: type", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = "0"),
    "Must be of type 'numeric'")
})

# minEraDuration
test_that("minEraDuration: type", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = "0"),
    "Must be of type 'numeric'")
})

# splitEventCohorts
test_that("splitEventCohorts", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = 3),
    "Must be of type 'character'")
})


# splitTime
test_that("splitTime: type", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = "30"),
    "Must be of type 'numeric'")
})

test_that("splitTime: negative", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = -30),
    "Element 1 is not >= 0.")
})

# eraCollapseSize
test_that("eraCollapseSize: type", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = "30"),
    "Must be of type 'numeric'")
})

test_that("eraCollapseSize: negative", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = -30),
    "Element 1 is not >= 0.")
})

# combinationWindow
test_that("combinationWindow: type", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = "30"),
    "Must be of type 'numeric'")
})

test_that("combinationWindow: negative", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = -30),
    "Element 1 is not >= 0.")
})

# minPostCombinationDuration
test_that("minPostCombinationDuration: type", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = "30"),
    "Must be of type 'numeric'")
})

test_that("minPostCombinationDuration: negative", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = -30),
    "Element 1 is not >= 0.")
})

# filterTreatments
test_that("filterTreatments", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "stuff"),
    "Must be a subset of \\{'First','Changes','All'\\}")
})

# maxPathLength
test_that("maxPathLength: type", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = "5"),
    "Must be of type 'numeric'")
})

test_that("maxPathLength: negative", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = -5),
    "Element 1 is not >= 0.")
})

# minCellCount
test_that("minCellCount: type", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5,
    minCellCount = "5"),
    "Must be of type 'numeric'")
})

test_that("minCellCount: negative", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5,
    minCellCount = -5),
    "Element 1 is not >= 0.")
})

# minCellMethod
# No assertions

# groupCombinations
test_that("groupCombinations: type", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5,
    minCellCount = 5,
    minCellMethod = "Remove",
    groupCombinations = "10"),
    "Must be of type 'numeric'")
})

test_that("groupCombinations: negative", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5,
    minCellCount = 5,
    minCellMethod = "Remove",
    groupCombinations = -10),
    "Element 1 is not >= 0.")
})

# addNoPaths
test_that("addNoPaths: type", {
  expect_error(createPathwaySettings(
    cohortSettings,
    studyName = "MyStudyName",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5,
    minCellCount = 5,
    minCellMethod = "Remove",
    groupCombinations = 10,
    addNoPaths = "FALSE"),
    "Must be of type 'logical'")
})
