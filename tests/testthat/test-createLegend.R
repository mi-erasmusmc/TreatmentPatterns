library(TreatmentPatterns)
library(testthat)
library(glue)

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

test_that("Validate read file", {
  expect_true(
    is.character(readLines(file.path(
      saveSettings$outputFolder,
    "Viral_Sinusitis",
    glue("{saveSettings$databaseName}_Viral_Sinusitis_all_input.txt")))))
})
