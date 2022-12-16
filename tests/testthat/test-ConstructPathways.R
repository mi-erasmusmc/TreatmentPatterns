library(TreatmentPatterns)
library(testthat)

source("inst/examples/R Settings Objects/createDummySettings.R")

test_that("Void", {
  expect_error(
    constructPathways() 
  )
})

createCohorts(
  dataSettings = dataSettings,
  cohortSettings = cohortSettings,
  saveSettings = saveSettings
)

test_that("Minimal", {
  constructPathways(
    dataSettings = dataSettings,
    pathwaySettings = pathwaySettings,
    saveSettings = saveSettings
  )
})
