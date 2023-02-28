# Libraries
library(testthat)
library(TreatmentPatterns)
library(Eunomia)

test_that("Void", {
  expect_error(checkDataSettings())
})

test_that("Minimal", {
  testthat::expect_true(
    checkDataSettings(
      connectionDetails = Eunomia::getEunomiaConnectionDetails(),
      cdmDatabaseSchema = "main",
      resultSchema = "main",
      cohortTable <- "cohort_table"
    )
  )
})

test_that("connectionDetails", {
  testthat::expect_error(
    checkDataSettings(connectionDetails = "getEunomiaConnectionDetails()"),
    c("Must inherit from class 'ConnectionDetails'")
  )
})

test_that("cdmDatabaseSchema", {
  testthat::expect_error(
    checkDataSettings(
      connectionDetails = Eunomia::getEunomiaConnectionDetails(),
      cdmDatabaseSchema = 3
    ),
    c("Must be of type 'character")
  )
})

test_that("resultSchema", {
  testthat::expect_error(
    checkDataSettings(
      connectionDetails = Eunomia::getEunomiaConnectionDetails(),
      cdmDatabaseSchema = "main",
      resultSchema = 2
    ),
    c("Must be of type 'character")
  )
})

test_that("cohortTable", {
  testthat::expect_error(
    checkDataSettings(
      connectionDetails = Eunomia::getEunomiaConnectionDetails(),
      cdmDatabaseSchema = "main",
      resultSchema = "main",
      cohortTable = TRUE
    ),
    c("Must be of type 'character")
  )
})
