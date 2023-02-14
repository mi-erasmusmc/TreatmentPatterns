library(testthat)
library(TreatmentPatterns)


test_that("void", {
  expect_error(
    TreatmentPatterns:::ensure_installed()
  )
})

test_that("minimal", {
  expect_output(
    TreatmentPatterns:::ensure_installed("base"),
    NA)
})