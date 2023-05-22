library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

tp <- treatmentPathways

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
  outputFile = basename(tempNoYear)
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
  outputFile = basename(tempYear)
)

test_that("void", {
  expect_error(TreatmentPatterns:::doMinCellCount())
})

df <- TreatmentPatterns:::doMinCellCount(
  fileNoYear = treatmentPathways[[1]],
  fileWithYear = treatmentPathways[[2]],
  outputFolder = saveSettings$outputFolder,
  tempFolder = saveSettings$tempFolder,
  databaseName = saveSettings$databaseName,
  studyName = "Viral_Sinusitis",
  groupCombinations = groupCombinations,
  minCellCount = 5,
  minCellMethod = "remove")

test_that("minimal", {
    expect_type(df, "list")
})

test_that("output 1", {
  expect_equal(ncol(df[[1]]), 2)
  expect_equal(typeof(df[[1]]$index_year), "character")
  expect_equal(typeof(df[[1]]$N), "character")
})

test_that("output 2", {
  expect_equal(ncol(df[[2]]), 3)
  expect_equal(typeof(df[[2]]$event_cohort_name1), "character")
  expect_equal(typeof(df[[2]]$event_cohort_name2), "character")
  expect_equal(typeof(df[[2]]$freq), "integer")
})

test_that("output 3", {
  expect_equal(ncol(df[[3]]), 4)
  expect_equal(typeof(df[[3]]$event_cohort_name1), "character")
  expect_equal(typeof(df[[3]]$event_cohort_name2), "character")
  expect_equal(typeof(df[[3]]$index_year), "integer")
  expect_equal(typeof(df[[3]]$freq), "integer")
})
