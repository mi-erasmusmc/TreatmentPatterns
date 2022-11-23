# createCohortSettings
# Parameters:
#   cohortsToCreate_location = NULL
#   targetCohorts = NULL
#   eventCohorts = NULL
#   loadCohorts = FALSE
#   cohortsFolder = NULL
#   baseUrl = NULL
#   generateCohorts = TRUE
#   includeDescendants = TRUE
# Returns cohortSettings object

# Libraries
library(testthat)
library(TreatmentPatterns)

# Variables
targetCohortsInMem <- data.frame(
  cohortId = c(1),
  atlasId = c(1777380),
  cohortName = c('Hypertension'),
  conceptSet = ""
)

eventCohortsInMem <- data.frame(
  cohortId = c(10, 11, 12, 13, 14),
  atlasId = c(1777381, 1777382, 1777383, 1777384, 1777385),
  cohortName = c(
    'Hydrochlorothiazide',
    'Metorolol',
    'Amlodipine',
    'Lisinopril',
    'Losartan'
  ),
  conceptSet = c("", "", "", "", "")
)

cohortsToCreatePath <- file.path(
  system.file(package = "TreatmentPatterns"),
  "examples",
  "OMOP CDM",
  "inst",
  "settings",
  "cohorts_to_create.csv"
)

cohortsToCreateWrongpath <- file.path(
  system.file(package = "TreatmentPatterns"),
  "examples",
  "OMOP CDM",
  "inst",
  "settings",
  "Numenor.csv"
)

cohortsFolderPath <-
  file.path(system.file(package = "TreatmentPatterns"),
            "examples",
            "OMOP CDM",
            "inst",
            "cohorts")

test_that("Void", {
  expect_error(createCohortSettings())
})

test_that("Minimal - in script", {
  expect_warning(
    expect_s3_class(
      createCohortSettings(targetCohorts = targetCohortsInMem,
                           eventCohorts = eventCohortsInMem),
      class = "cohortSettings"
    ),
    regexp = c("cohortsFolder missing")
  )
})

test_that("Minimal - from file", {
  expect_warning(
    expect_s3_class(
      createCohortSettings(cohortsToCreate_location = cohortsToCreatePath),
      class = "cohortSettings"
    ),
    regexp = c("cohortsFolder missing")
  )
})

test_that("OMOP: Settings in script", {
  expect_s3_class(
    createCohortSettings(
      targetCohorts = targetCohortsInMem,
      eventCohorts = eventCohortsInMem,
      baseUrl = "http://api.ohdsi.org:8080/WebAPI",
      loadCohorts = TRUE
    ),
    "cohortSettings"
  )
})

test_that("OMOP: Settings from file", {
  expect_s3_class(
    createCohortSettings(
      cohortsToCreate_location = cohortsToCreatePath,
      cohortsFolder = cohortsFolderPath
    ),
    "cohortSettings"
  )
})

test_that("OMOP: Settings from file - wrong file", {
  expect_error(
    createCohortSettings(
      cohortsToCreate_location = cohortsToCreateWrongpath,
      cohortsFolder = cohortsFolderPath
    ),
    regexp = c("does not exist")
  )
})
