# createDataSettings
# Input parameters:
#   pathwaySettings_location = NULL
#   pathwaySettings_list = NULL
#   targetCohortId = NULL
#   eventCohortIds = NULL
#   ...
# Returns dataSettings object

# Libraries
library(testthat)
library(TreatmentPatterns)

test_that("Void", {
  expect_error(createPathwaySettings())
})

test_that("Minimal A", {
  expect_s3_class(createPathwaySettings(pathwaySettings_list = list(
    addPathwaySettings(
      studyName = c("default"),
      targetCohortId = 1,
      eventCohortIds = c(10, 11, 12, 13, 14),
      addNoPaths = TRUE
    )
  )),
  "pathwaySettings")
})

test_that("Minimal B", {
  expect_s3_class(createPathwaySettings(
    pathwaySettings_location = file.path(
      system.file(package = "TreatmentPatterns"),
      "examples",
      "OMOP CDM",
      "inst",
      "settings",
      "pathway_settings.csv"
    )
  ),
  "pathwaySettings")
})

test_that("Minimal C", {
  expect_s3_class(createPathwaySettings(
    targetCohortId = 1,
    eventCohortIds = c(10, 11, 12, 13, 14)
  ),
  "pathwaySettings")
})

test_that("Multiple params", {
  expect_error(
    createPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = c(10, 11, 12, 13, 14),
      pathwaySettings_location = file.path(
        system.file(package = "TreatmentPatterns"),
        "examples",
        "OMOP CDM",
        "inst",
        "settings",
        "pathway_settings.csv"
      ),
      pathwaySettings_list = list(
        addPathwaySettings(
          studyName = c("default"),
          targetCohortId = 1,
          eventCohortIds = c(10, 11, 12, 13, 14),
          addNoPaths = TRUE
        )
      )
    )
  )
})

test_that("Extra param: studyName", {
  createPathwaySettings(
    targetCohortId = 1,
    eventCohortIds = c(10, 11, 12, 13, 14),
    studyName = "studyName"
  )
})

test_that("Extra param: studyName", {
  expect_s3_class(
    createPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = c(10, 11, 12, 13, 14),
      studyName = "studyName"
    ),
    "pathwaySettings"
  )
})

test_that("Extra param: dataframe instead of numeric", {
  expect_error(
    createPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = c(10, 11, 12, 13, 14),
      minCellCount  = iris,
      
    ),
    "pathwaySettings"
  )
})

test_that("Extra param: dataframe instead of character", {
  expect_error(
    createPathwaySettings(
      targetCohortId = 1,
      eventCohortIds = c(10, 11, 12, 13, 14),
      minCellMethod = iris
    ),
    "pathwaySettings"
  )
})
