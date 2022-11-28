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

test_that("Parameter order", {
  expect_s3_class(createSaveSettings("./"),
                  "saveSettings")
})

test_that("Weird parameters", {
  expect_error(
    createSaveSettings(
      databaseName = iris,
      rootFolder = iris,
      outputFolder = iris,
      tempFolder = iris
    )
  )
})

# Create saveSettings with numeric
saveSettings <- createSaveSettings(1, 2, 3, 4)

test_that("databaseName", {
  expect_type(saveSettings$databaseName, "character")
})

test_that("rootFolder", {
  expect_type(saveSettings$rootFolder, "character")
})

test_that("outputFolder", {
  expect_type(saveSettings$outputFolder, "character")
})

test_that("tempFolder", {
  expect_type(saveSettings$tempFolder, "character")
})
