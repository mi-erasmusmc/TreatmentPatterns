library(TreatmentPatterns)
library(testthat)
library(DatabaseConnector)

source(list.files(system.file(
  package = "TreatmentPatterns",
  "examples", "SettingObjects"), full.names = TRUE))

con <- connect(dataSettings$connectionDetails)

test_that("void", {
  expect_error(
    TreatmentPatterns:::getCohortCharacteristics()
    )
})

test_that("minimal", {
  expect_output(
    TreatmentPatterns:::getCohortCharacteristics(
      connectionDetails = NULL,
      connection = con,
      cdmDatabaseSchema = "main",
      cohortDatabaseSchema = "main",
      cohortTable = "cohort_table",
      cohortIds = c(1,2,3),
      cdmVersion = 5,
      covariateSettings = CohortDiagnostics::getDefaultCovariateSettings(),
      batchSize = 100), "Cohort characterization took .+ secs")
})
