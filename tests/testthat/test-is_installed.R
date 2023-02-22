library(TreatmentPatterns)
library(testthat)

basePackage <- "base"

test_that("void", {
  expect_false(TreatmentPatterns:::is_installed())
})

test_that("minimal", {
  expect_true(TreatmentPatterns:::is_installed(pkg = basePackage))
  expect_true(TreatmentPatterns:::is_installed(pkg = basePackage, version = 1))
  expect_false(TreatmentPatterns:::is_installed(pkg = basePackage, version = 99999))
})

test_that("invalid input", {
  expect_false(TreatmentPatterns:::is_installed(pkg = NULL, version = 0))
  expect_equal(TreatmentPatterns:::is_installed(pkg = basePackage, version = NULL), NA)
})