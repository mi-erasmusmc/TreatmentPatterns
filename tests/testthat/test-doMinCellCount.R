library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

tp <- treatment_pathways

tempNoYear <- tempfile(
  pattern = paste0(
    saveSettings$databaseName,
    "_Viral_Sinusitis",
    "_percentage_groups_treated_noyear"),
  fileext = ".csv")

TreatmentPatterns:::outputTreatedPatients(
  data = tp[[1]],
  eventCohortIds = eventCohortIds,
  groupCombinations = groupCombinations,
  outputFolder = saveSettings$outputFolder,
  outputFile = tempNoYear
)

tempYear <- tempfile(paste0(
  saveSettings$databaseName,
  "_",
  "Viral_Sinusitis",
  "_percentage_groups_treated_withyear.csv"
))

TreatmentPatterns:::outputTreatedPatients(
  data = tp[[2]],
  eventCohortIds = eventCohortIds,
  groupCombinations = TRUE,
  outputFolder = saveSettings$outputFolder,
  outputFile = tempYear
)

test_that("void", {
  expect_error(TreatmentPatterns:::doMinCellCount())
})

test_that("minimal", {
  expect_output(
    df <- TreatmentPatterns:::doMinCellCount(
      file_noyear = treatment_pathways[[1]],
      file_withyear = treatment_pathways[[2]],
      outputFolder = saveSettings$outputFolder,
      tempFolder = saveSettings$tempFolder,
      databaseName = saveSettings$databaseName,
      studyName = "Viral_Sinusitis",
      groupCombinations = groupCombinations,
      minCellCount = 5,
      minCellMethod = "remove"
    ),
    "(Remove  \\d+  paths with too low frequency \\(\\w+ year\\)\\n){2}doMinCellCount done"
  )
  expect_type(df, "list")
})
