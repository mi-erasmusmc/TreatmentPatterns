# createDataSettings
# Input parameters:
#   OMOP_CDM = "TRUE"
#   connectionDetails = NULL
#   cdmDatabaseSchema = NULL
#   cohortDatabaseSchema = NULL
#   cohortTable = "treatmentpatterns_cohorts"
#   cohortLocation = NULL
# Returns dataSettings object

# Libraries
library(testthat)
library(TreatmentPatterns)
library(Eunomia)

test_that("Void", {
  expect_error(createDataSettings())
})

test_that("Minimal", {
  testthat::expect_s3_class(createDataSettings(
    connectionDetails = getEunomiaConnectionDetails(),
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main"), "dataSettings")
})

test_that("OMOP", {
  testthat::expect_s3_class(createDataSettings(
    connectionDetails = getEunomiaConnectionDetails(),
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    OMOP_CDM = TRUE,
    cohortTable = "treatmentpatterns_cohorts"), "dataSettings")
})

test_that("NOMOP", {
  testthat::expect_s3_class(createDataSettings(
    connectionDetails = getEunomiaConnectionDetails(),
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    OMOP_CDM = FALSE,
    cohortLocation = list.files(
      path = system.file(
        "examples/other format/inst/cohorts",
        package = "TreatmentPatterns"),
      full.names = TRUE)),
    "dataSettings")
})
