library(TreatmentPatterns)
library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("Minimal", {
  expect_output(TreatmentPatterns:::outputSankeyDiagram(
    data = treatment_pathways[[1]],
    outputFolder = saveSettings$outputFolder,
    databaseName = saveSettings$databaseName,
    studyName = "Viral_Sinusitis",
    groupCombinations = TRUE
  ),
  "outputSankeyDiagram done")
  
  expect_true(file.exists(file.path(
    saveSettings$outputFolder,
    "Viral_Sinusitis",
    paste0("Eunomia", "_", "Viral_Sinusitis", "_all_sankeydiagram.html"))))
})
