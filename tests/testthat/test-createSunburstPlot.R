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
  expect_error(TreatmentPatterns:::createSunburstPlot())
})

test_that("minimal", {
  TreatmentPatterns:::createSunburstPlot(
    data,
    outcomes,
    folder = saveSettings$outputFolder,
    file_name = file.path("Viral_Sinusitis", paste0(
      saveSettings$databaseName, "_", "Viral_Sinusitis", "_all")),
    shiny = TRUE)
  expect_true(file.exists(paste0(saveSettings$outputFolder, file.path("/Viral_Sinusitis", paste0(
    saveSettings$databaseName, "_", "Viral_Sinusitis", "_all", "_sunburstplot.html")))))
})
