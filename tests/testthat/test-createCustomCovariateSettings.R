library(TreatmentPatterns)
library(testthat)
library(CohortDiagnostics)

test_that("void", {
  expect_error(
    TreatmentPatterns:::createCustomCovariateSettings()
  )
})

test_that("minimal", {
  expect_s3_class(
    TreatmentPatterns:::createCustomCovariateSettings(
      getDefaultCovariateSettings()),
    "covariateSettings")
})
