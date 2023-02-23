# Libraries
library(testthat)
library(TreatmentPatterns)

test_that("Void", {
  expect_error(checkSaveSettings())
})

test_that("Minimal", {
  expect_logical(checkSaveSettings(rootFolder = "./",
                                   databaseName = "Eunomia", 
                                   outputFolder = "./inst",
                                   tempFolder = "./inst"))
})

test_that("Assert rootFolder: wrong type", {
  expect_error(checkSaveSettings(rootFolder = 3,
                                 databaseName = "Eunomia", 
                                 outputFolder = "./inst",
                                 tempFolder = "./inst"),
               c("No directory provided"))
})

test_that("Assert rootFolder: No valid folder", {
  expect_error(checkSaveSettings(rootFolder = "stuff/things/bla/bla",
                                 databaseName = "Eunomia", 
                                 outputFolder = "./inst",
                                 tempFolder = "./inst"),
               c("does not exist."))
})

test_that("Assert databaseName", {
  expect_error(
    checkSaveSettings(rootFolder = "./", 
                       databaseName = 3, 
                       outputFolder = "./inst",
                       tempFolder = "./inst"),
    c("Must be of type 'character'")
  )
})

test_that("Assert outputFolder: wrong type", {
  expect_error(checkSaveSettings(
    rootFolder = "./",
    databaseName = "Eunomia", 
    outputFolder = 3,
    tempFolder = "./inst"),
    c("Must be of type 'character'"))
})

test_that("Assert tempFolder: wrong type", {
  expect_error(checkSaveSettings(
    rootFolder = "./",
    databaseName = "Eunomia", 
    outputFolder = "./inst",
    tempFolder = 1),
    c("No path provided"))
})

