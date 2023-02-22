library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("void", {
  expect_error(
    TreatmentPatterns:::groupInfrequentCombinations()
    )
})

test_that("minimal", {
  expect_s3_class(
    TreatmentPatterns:::groupInfrequentCombinations(
      data = treatment_pathways[[1]],
      groupCombinations = groupCombinations),
    "data.frame")
})
