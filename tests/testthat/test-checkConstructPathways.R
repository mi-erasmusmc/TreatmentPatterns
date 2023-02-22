library(TreatmentPatterns)
library(testthat)

dataSettings <- createDataSettings(
  connectionDetails = Eunomia::getEunomiaConnectionDetails(),
  cdmDatabaseSchema = "main",
  resultSchema = "main",
  cohortTable = "myCohortTable"
)

saveSettings <- createSaveSettings(
  rootFolder = "./",
  databaseName = "Eunomia", 
  outputFolder = "./inst",
  tempFolder = "./inst")

targetCohorts <- data.frame(cohortId = c(1),
                            cohortName = c('Hypertension'))

eventCohorts <- data.frame(
  cohortId = c(10, 11, 12, 13, 14),
  cohortName = c(
    'Hydrochlorothiazide',
    'Metorolol',
    'Amlodipine',
    'Lisinopril',
    'Losartan'
  )
)

cohortSettings <- createCohortSettings(
  targetCohorts, 
  eventCohorts)

pathwaySettings <- createPathwaySettings(
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
  addNoPaths = FALSE)

test_that("Void", {
  expect_error(
    checkConstructPathways() 
  )
})

test_that("Minimal", {
  expect_true(
    checkConstructPathways(dataSettings, 
                           pathwaySettings, 
                           saveSettings) 
  )
})

test_that("Assert logical", {
  expect_logical(
    checkConstructPathways(dataSettings, 
                           pathwaySettings, 
                           saveSettings) 
  )
})

test_that("Expect error if wrong order of variables", {
  expect_error(
    checkConstructPathways(pathwaySettings, 
                           dataSettings,
                           saveSettings) 
  )
})
