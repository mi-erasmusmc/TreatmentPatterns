library(TreatmentPatterns)
library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("void", {
  expect_error(
    TreatmentPatterns::generateOutput(),
    "argument .+ is missing")
})

test_that("minimal", {
  expect_output(
    TreatmentPatterns::generateOutput(
      saveSettings = saveSettings),
    "generateOutput done.")
})
