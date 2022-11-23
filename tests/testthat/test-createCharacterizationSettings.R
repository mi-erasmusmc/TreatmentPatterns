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
  expect_s3_class(
    createCharacterizationSettings(),
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