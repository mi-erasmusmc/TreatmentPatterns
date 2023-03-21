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


out <- TreatmentPatterns:::getPathways(
  outputFolder = saveSettings$outputFolder,
  tempFolder = saveSettings$tempFolder,
  databaseName = saveSettings$databaseName,
  studyName = "Viral_Sinusitis",
  minCellCount = 5)

test_that("item 1", {
  expect_equal(ncol(out[[1]]), expected = 3)
  expect_equal(typeof(out[[1]]$freq), "integer")
  expect_equal(typeof(out[[1]]$event_cohort_name1), "character")
  expect_equal(typeof(out[[1]]$event_cohort_name2), "character")
})

test_that("item 2", {
  expect_equal(ncol(out[[2]]), expected = 4)
  expect_equal(typeof(out[[2]]$freq), "integer")
  expect_equal(typeof(out[[2]]$event_cohort_name1), "character")
  expect_equal(typeof(out[[2]]$event_cohort_name2), "character")
  expect_equal(typeof(out[[2]]$index_year), "integer")
})
