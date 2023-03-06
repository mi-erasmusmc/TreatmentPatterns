# Libraries
library(testthat)
library(TreatmentPatterns)

test_that("Void", {
  expect_error(createSaveSettings())
})

test_that("Minimal", {
  expect_s3_class(createSaveSettings(rootFolder = "./"),
                  "saveSettings")
})

test_that("Assert rootFolder: wrong type", {
  expect_error(createSaveSettings(rootFolder = 3),
               c("No directory provided"))
})

test_that("Assert rootFolder: No valid folder", {
  expect_error(createSaveSettings(rootFolder = "stuff/things/bla/bla"),
               c("does not exist."))
})

test_that("Assert databaseName", {
  expect_error(
    createSaveSettings(rootFolder = "./", databaseName = 3),
    c("Must be of type 'character'")
  )
})

test_that("Assert outputFolder: wrong type", {
  expect_error(createSaveSettings(
    rootFolder = "./",
    databaseName = "Eunomia", outputFolder = 3),
               c("Must be of type 'character'"))
})

test_that("Assert outputFolder: existing path", {
  expect_s3_class(createSaveSettings(
    rootFolder = "./",
    databaseName = "Eunomia", 
    outputFolder = "./inst"),
    "saveSettings")
})

test_that("Assert outputFolder: non-existing path", {
  expect_s3_class(createSaveSettings(
    rootFolder = "./",
    databaseName = "Eunomia", 
    outputFolder = "./VeryCoolOutputPathThatDoesNotExist"),
    "saveSettings")
})

test_that("Assert tempFolder: existing path", {
  expect_s3_class(createSaveSettings(
    rootFolder = "./",
    databaseName = "Eunomia", 
    outputFolder = "./inst",
    tempFolder = "./inst"),
    "saveSettings")
})

test_that("Assert tempFolder: non-existing path", {
  expect_s3_class(createSaveSettings(
    rootFolder = "./",
    databaseName = "Eunomia", 
    outputFolder = "./inst",
    tempFolder = "./VeryCoolTempPathThatDoesNotExist"),
    "saveSettings")
})

# All == Assert tempFolder: existing path