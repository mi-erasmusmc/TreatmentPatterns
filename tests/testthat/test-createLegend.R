library(TreatmentPatterns)
library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("void", {
  expect_error(
    TreatmentPatterns:::createLegend()
  )
})

test_that("minimal", {
  TreatmentPatterns:::createLegend(
    studyName = "Viral_Sinusitis",
    outputFolder = saveSettings$outputFolder,
    databaseName = saveSettings$databaseName)
  
  expect_true(
    file.exists(
      glue::glue("{saveSettings$outputFolder}/Viral_Sinusitis/legend.html")))
})

