# Libraries
library(testthat)
library(TreatmentPatterns)
library(Eunomia)

test_that("Void", {
  expect_error(checkDataSettings())
})

test_that("Minimal", {
  e <- new.env()
  e$connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  e$cdmDatabaseSchema <- "main"
  e$resultSchema <- "main"
  e$cohortTable <- "cohort_table"
  
  testthat::expect_true(
    TreatmentPatterns:::checkDataSettings(e)
  )
})

test_that("connectionDetails", {
  e <- new.env()
  e$cdmDatabaseSchema <- "main"
  e$resultSchema <- "main"
  e$cohortTable <- "cohort_table"
  e$connectionDetails <- "getEunomiaConnectionDetails()"
  
  testthat::expect_error(
    TreatmentPatterns:::checkDataSettings(e),
    c("Must inherit from class 'ConnectionDetails'")
  )
})

test_that("cdmDatabaseSchema", {
  e <- new.env()
  e$connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  e$resultSchema <- "main"
  e$cohortTable <- "cohort_table"
  e$cdmDatabaseSchema <- 3
  
  testthat::expect_error(
    TreatmentPatterns:::checkDataSettings(e),
    c("Must be of type 'character")
  )
})

test_that("resultSchema", {
  e <- new.env()
  e$connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  e$cdmDatabaseSchema <- "main"
  e$resultSchema <- "main"
  e$cohortTable <- "cohort_table"
  e$resultSchema <- 2
  
  testthat::expect_error(
    TreatmentPatterns:::checkDataSettings(e),
    c("Must be of type 'character")
  )
})

test_that("cohortTable", {
  e <- new.env()
  e$connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  e$cdmDatabaseSchema <- "main"
  e$resultSchema <- "main"
  e$cohortTable <- TRUE
  
  testthat::expect_error(
    TreatmentPatterns:::checkDataSettings(e),
    c("Must be of type 'character")
  )
})
