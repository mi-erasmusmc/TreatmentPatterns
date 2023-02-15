library(TreatmentPatterns)
library(testthat)

test_that("Void", {
  
  source(list.files(system.file(
    package = "TreatmentPatterns",
    "examples", "SettingObjects"), full.names = TRUE))
  
  expect_error(
    constructPathways() 
  )
})

test_that("Minimal", {
  
  source(list.files(system.file(
    package = "TreatmentPatterns",
    "examples", "SettingObjects"), full.names = TRUE))
  
  expect_output(
    constructPathways(
    dataSettings = dataSettings,
    pathwaySettings = pathwaySettings,
    saveSettings = saveSettings
  ), "constructPathways done.")
})

test_that("Wrong variables", {
  
  source(list.files(system.file(
    package = "TreatmentPatterns",
    "examples", "SettingObjects"), full.names = TRUE))
  
  expect_error(
    constructPathways(
      dataSettings = saveSettings,
      pathwaySettings = dataSettings,
      saveSettings = pathwaySettings
    ))
})
