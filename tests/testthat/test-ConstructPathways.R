library(TreatmentPatterns)
library(testthat)

source(list.files(system.file(
  package = "TreatmentPatterns",
  "examples", "SettingObjects"), full.names = TRUE))

test_that("Void", {
  expect_error(
    constructPathways() 
  )
})

test_that("Minimal", {
  expect_output(
    constructPathways(
    dataSettings = dataSettings,
    pathwaySettings = pathwaySettings,
    saveSettings = saveSettings
  ), "constructPathways done.")
})
