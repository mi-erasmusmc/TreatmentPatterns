library(TreatmentPatterns)
library(testthat)

source(list.files(system.file(
  package = "TreatmentPatterns",
  "examples", "R Settings Objects"), full.names = TRUE))

source(list.files(system.file(
  package = "TreatmentPatterns",
  "testing"), full.names = TRUE))

test_that("void", {
  expect_error(TreatmentPatterns:::doEraDuration())
})

test_that("", {
   expect_s3_class(treatment_history <- TreatmentPatterns:::doEraDuration(
    treatment_history = treatment_history,
    minEraDuration = minEraDuration), "data.frame")
})
