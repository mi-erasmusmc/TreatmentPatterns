library(testthat)
library(TreatmentPatterns)


test_that("void", {
  expect_error(TreatmentPatterns:::ensure_installed())
})

test_that("minimal", {
  expect_output(TreatmentPatterns:::ensure_installed(pkg = "base"), NA)
})

test_that("invalid input", {
  expect_error(TreatmentPatterns:::ensure_installed(pkg = "base999"))
  expect_error(TreatmentPatterns:::ensure_installed(pkg = NULL))
  expect_error(TreatmentPatterns:::ensure_installed(pkg = mtcars))
})