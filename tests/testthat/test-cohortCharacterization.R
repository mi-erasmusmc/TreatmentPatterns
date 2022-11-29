library(testthat)
library(TreatmentPatterns)

source("inst/examples/R Settings Objects/createDummySettings.R")

test_that("Void", {
  expect_error(cohortCharacterization())
})

createCohorts(
  dataSettings = dataSettings,
  cohortSettings = cohortSettings,
  saveSettings = saveSettings
)

test_that("Minimal", {
  expect_error(
    cohortCharacterization(
      dataSettings = dataSettings,
      characterizationSettings = characterizationSettings,
      saveSettings = saveSettings
    ),
    c("replacement has 1 row, data has 0")
  )
})

test_that("No dataSettings", {
  expect_error(
    cohortCharacterization(
      dataSettings = 1,
      characterizationSettings = characterizationSettings,
      saveSettings = saveSettings
    ),
    c("Incorrect class for dataSettings")
  )
})

test_that("characterizationSettings", {
  expect_error(
    cohortCharacterization(
      dataSettings = dataSettings,
      characterizationSettings = 1,
      saveSettings = saveSettings
    ),
    c("Incorrect class for characterizationSettings")
  )
})

test_that("saveSettings", {
  expect_error(
    cohortCharacterization(
      dataSettings = dataSettings,
      characterizationSettings = characterizationSettings,
      saveSettings = 1
    ),
    c("Incorrect class for saveSettings")
  )
})
