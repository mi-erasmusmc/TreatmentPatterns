# Libraries
library(testthat)
library(TreatmentPatterns)

test_that("Void", {
  expect_error(TreatmentPatterns:::checkSaveSettings())
})

test_that("Minimal", {
  e <- new.env()
  e$rootFolder <- "./"
  e$databaseName <- "Eunomia"
  e$outputFolder <- "./inst"
  e$tempFolder <- "./inst"

  expect_true(TreatmentPatterns:::checkSaveSettings(e))
})

test_that("Assert rootFolder: wrong type", {
  e <- new.env()
  e$rootFolder <- 3
  e$databaseName <- "Eunomia"
  e$outputFolder <- "./inst"
  e$tempFolder <- "./inst"

  expect_error(TreatmentPatterns:::checkSaveSettings(e),
    c("No directory provided"))
})

test_that("Assert rootFolder: No valid folder", {
  e <- new.env()
  e$rootFolder <- "stuff/things/bla/bla"
  e$databaseName <- "Eunomia"
  e$outputFolder <- "./inst"
  e$tempFolder <- "./inst"

  expect_error(TreatmentPatterns:::checkSaveSettings(e),
               c("does not exist."))
})

test_that("Assert databaseName", {
  e <- new.env()
  e$rootFolder <- "./"
  e$databaseName <- 3
  e$outputFolder <- "./inst"
  e$tempFolder <- "./inst"

  expect_error(
    TreatmentPatterns:::checkSaveSettings(e),
    c("Must be of type 'character'")
  )
})

test_that("Assert outputFolder: wrong type", {
  e <- new.env()
  e$rootFolder <- "./"
  e$databaseName <- "Eunomia"
  e$outputFolder <- 3
  e$tempFolder <- "./inst"

  expect_error(TreatmentPatterns:::checkSaveSettings(e),
    c("Must be of type 'character'"))
})

test_that("Assert tempFolder: wrong type", {
  e <- new.env()
  e$rootFolder <- "./"
  e$databaseName <- "Eunomia"
  e$outputFolder <- "./inst"
  e$tempFolder <- 1

  expect_error(TreatmentPatterns:::checkSaveSettings(e),
    c("No path provided"))
})
