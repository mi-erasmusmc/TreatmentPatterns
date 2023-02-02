library(TreatmentPatterns)
library(testthat)

source("inst/examples/R Settings Objects/createDummySettings.R")
source("tests/testParams.R")

test_that("void", {
  expect_error(TreatmentPatterns:::doMaxPathLength())
})

test_that("minimal", {
  expect_s3_class(treatment_history <- TreatmentPatterns:::doMaxPathLength(
    doFilterTreatmentsTHPP, 
    maxPathLength), "data.frame")
})
