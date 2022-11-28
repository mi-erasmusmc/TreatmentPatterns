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

