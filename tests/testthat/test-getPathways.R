library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("void", {
  expect_error(TreatmentPatterns:::getPathways())
})

test_that("minimal", {
  # Create Viral Sinusits dir
  fs::dir_create(paste0(
    saveSettings$outputFolder,
    "/Viral_Sinusitis"))
  
  expect_s3_class(
    TreatmentPatterns:::getPathways(
      outputFolder = saveSettings$outputFolder,
      tempFolder = saveSettings$tempFolder,
      databaseName = saveSettings$databaseName,
      studyName = "Viral_Sinusitis",
      minCellCount = 5)[[1]], "data.frame")
})
