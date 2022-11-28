library(TreatmentPatterns)
library(testthat)

# Source dummySettings
source("inst/examples/R Settings Objects/createDummySettings.R")

test_that("Void", {
  expect_error(executeTreatmentPatterns())
})

test_that("Minimal", {
  expect_warning(expect_error(
    executeTreatmentPatterns(
      dataSettings = dataSettings,
      cohortSettings = cohortSettings,
      characterizationSettings = characterizationSettings,
      pathwaySettings = pathwaySettings,
      saveSettings = saveSettings
    ), c("1 row, data has 0")
  ))
})

test_that("Parameter order", {
  expect_warning(expect_error(
    executeTreatmentPatterns(
      dataSettings,
      cohortSettings,
      pathwaySettings,
      saveSettings,
      characterizationSettings
    ), c("1 row, data has 0")
  ))
})
