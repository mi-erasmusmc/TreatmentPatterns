# Libraries
library(testthat)
library(TreatmentPatterns)

test_that("Void", {
  expect_error(createDataSettings())
})

test_that("Minimal", {
  testthat::expect_s3_class(
    createDataSettings(
      connectionDetails = Eunomia::getEunomiaConnectionDetails(),
      cdmDatabaseSchema = "main",
      resultSchema = "main"
    ),
    "dataSettings"
  )
})

test_that("Assert connectionDetails", {
  testthat::expect_error(
    createDataSettings(connectionDetails = "getEunomiaConnectionDetails()"),
    c("Must inherit from class 'ConnectionDetails'")
  )
})

test_that("Assert cdmDatabaseSchema", {
  testthat::expect_error(
    createDataSettings(
      connectionDetails = Eunomia::getEunomiaConnectionDetails(),
      cdmDatabaseSchema = 3
    ),
    c("Must be of type 'character")
  )
})

test_that("Assert resultSchema", {
  testthat::expect_error(
    createDataSettings(
      connectionDetails = Eunomia::getEunomiaConnectionDetails(),
      cdmDatabaseSchema = "main",
      resultSchema = 2
    ),
    c("Must be of type 'character")
  )
})

test_that("Assert cohortTable", {
  testthat::expect_error(
    createDataSettings(
      connectionDetails = Eunomia::getEunomiaConnectionDetails(),
      cdmDatabaseSchema = "main",
      resultSchema = "main",
      cohortTable = TRUE
    ),
    c("Must be of type 'character")
  )
})

test_that("All parameters", {
  testthat::expect_s3_class(
    createDataSettings(
      connectionDetails = Eunomia::getEunomiaConnectionDetails(),
      cdmDatabaseSchema = "main",
      resultSchema = "main",
      cohortTable = "myCohortTable"
    ),
    "dataSettings"
  )
})
