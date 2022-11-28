# createCharacterizationSettings
# Parameters
#   baselineCovariates_location = NULL
#   baselineCovariates = data.frame()
#   standardCovariateSettings = FeatureExtraction::createCovariateSettings()
#   returnCovariates = "all"
#   minCellCount = 5
# Returns characterizationSettings object

# Libraries
library(TreatmentPatterns)
library(testthat)

test_that("Void", {
  expect_s3_class(createCharacterizationSettings(),
                  "characterizationSettings")
})

test_that("Invalid file - nonexisting file", {
  expect_error(
    createCharacterizationSettings(baselineCovariates_location = "Numenor"),
    c("does not exist")
  )
})

test_that("Invalid file - numeric", {
  expect_error(
    createCharacterizationSettings(baselineCovariates_location = 0),
    c("is not a path")
  )
})

test_that("Negative minCellCount", {
  expect_warning(createCharacterizationSettings(minCellCount = -1),
                 c("Negative minCellCount"))
})

test_that("returnCovariates - nonsense", {
  expect_warning(createCharacterizationSettings(returnCovariates = "stuff"),
                 c("using all"))
})

test_that("baselineCovariates_location", {
  characterizationSettings <-
    createCharacterizationSettings(baselineCovariates_location = NULL)
  
  expect_null(characterizationSettings$baselineCovariates_location)
})

test_that("baselineCovariates_location", {
  expect_error(createCharacterizationSettings(baselineCovariates_location = 3))
})

test_that("baselineCovariates_location", {
  expect_error(createCharacterizationSettings(baselineCovariates_location = ""))
})

test_that("baselineCovariates_location", {
  expect_s3_class(
    createCharacterizationSettings(baselineCovariates_location = "./inst/examples/OMOP CDM/inst/settings/characterization_settings.csv"),
    "characterizationSettings"
  )
})

test_that("baselineCovariates_location", {
  expect_error(
    createCharacterizationSettings(baselineCovariates_location = "./inst/examples/OMOP CDM/inst/settings/cohorts_to_create.csv")
  )
})

test_that("baselineCovariates", {
  expect_error(createCharacterizationSettings(baselineCovariates = data.frame()))
})

test_that("baselineCovariates", {
  expect_error(createCharacterizationSettings(baselineCovariates = list()))
})

test_that("baselineCovariates", {
  df <- data.frame(covariateName = c("Age"),
                   covariateId = c(1002))
  expect_s3_class(
    createCharacterizationSettings(baselineCovariates = df),
    "characterizationSettings"
  )
})

test_that("baselineCovariates", {
  l <- list(covariateName = c("Age"),
            covariateId = c(1002))
  expect_s3_class(
    createCharacterizationSettings(baselineCovariates = l),
    "characterizationSettings"
  )
})

test_that("returnCovariates", {
  expect_error(createCharacterizationSettings(returnCovariates = "stuff"))
})

test_that("returnCovariates", {
  expect_error(createCharacterizationSettings(minCellCount = "5"))
})
