library(TreatmentPatterns)
library(testthat)
library(DatabaseConnector)
library(Eunomia)

source(list.files(system.file(
  package = "TreatmentPatterns",
  "examples", "SettingObjects"), full.names = TRUE))

con <- connect(dataSettings$connectionDetails)
on.exit(disconnect(con))

test_that("void", {
  expect_error(
    TreatmentPatterns:::getCohortCounts())
})

test_that("void", {
  expect_s3_class(
    TreatmentPatterns:::getCohortCounts(
      connection = con,
      cohortDatabaseSchema = "main",
      cohortTable = "cohort_table",
      cohortIds = c(1, 2, 3, 4, 5, 6, 7)), 
    "data.frame")
})
