library(TreatmentPatterns)
library(testthat)

source("inst/examples/R Settings Objects/createDummySettings.R")
source("tests/testParams.R")

test_that("void", {
  expect_error(TreatmentPatterns:::doEraDuration())
})

test_that("", {
   expect_s3_class(treatment_history <- TreatmentPatterns:::doEraDuration(
    treatment_history = treatment_history,
    minEraDuration = minEraDuration), "data.frame")
})
