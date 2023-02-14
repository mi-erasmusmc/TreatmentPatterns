library(TreatmentPatterns)
library(testthat)

test_that("void", {
  expect_false(TreatmentPatterns:::is_installed())
})

test_that("minimal", {
  expect_true(TreatmentPatterns:::is_installed("base"))
})
