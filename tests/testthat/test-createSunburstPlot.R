library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

data <- TreatmentPatterns:::inputSunburstPlot(
  treatment_pathways[[1]],
  saveSettings$outputFolder,
  saveSettings$databaseName,
  "Viral_Sinusitis",
  FALSE,
  index_year = 'all')

cohorts <- readr::read_csv(
  file.path(saveSettings$outputFolder, "settings", "cohorts_to_create.csv"),
  col_types = list("i", "c", "c"))

outcomes <- c(
  cohorts$cohortName[cohorts$cohortId %in% eventCohortIds],
  "Other")

test_that("void", {
  expect_error(TreatmentPatterns::createSunburstPlot())
})

test_that("minimal", {
  fileName <- paste0(saveSettings$databaseName, "_", "Viral_Sinusitis", "_all")
  
  outFileName <- file.path(
    saveSettings$outputFolder, 
    paste0(fileName, "_sunburstplot.html"))
  
  TreatmentPatterns::createSunburstPlot(
    data = data,
    outcomes = outcomes, 
    folder = saveSettings$outputFolder,
    fileName = fileName,
    shiny = TRUE)
  
  expect_true(file.exists(outFileName))
})

test_that("invalid input", {
  expect_error(
    TreatmentPatterns:::createSunburstPlot(
      data = NULL)
    )
  
  expect_error(
    TreatmentPatterns:::createSunburstPlot(
      data = data,
      outcomes = -1)
    )
  
  expect_error(
    TreatmentPatterns:::createSunburstPlot(
      data = data,
      outcomes = outcomes,
      folder = -1)
    )
  
  expect_error(
    TreatmentPatterns:::createSunburstPlot(
      data = data,
      outcomes = outcomes, 
      folder = saveSettings$outputFolder,
      fileName = -1)
    )
  
  expect_error(
    TreatmentPatterns:::createSunburstPlot(
      data = data,
      outcomes = outcomes, 
      folder = saveSettings$outputFolder,
      fileName = fileName,
      shiny = mtcars)
    )
})

