# Libraries
library(testthat)
library(TreatmentPatterns)

# Variables
targetCohorts <- data.frame(cohortId = c(1),
                            cohortName = c('Hypertension'))

eventCohorts <- data.frame(
  cohortId = c(10, 11, 12, 13, 14),
  cohortName = c(
    'Hydrochlorothiazide',
    'Metorolol',
    'Amlodipine',
    'Lisinopril',
    'Losartan'
  )
)

test_that("Void", {
  expect_error(createCohortSettings())
})

test_that("Minimal", {
  expect_s3_class(
    createCohortSettings(targetCohorts = targetCohorts,
                         eventCohorts = eventCohorts),
    class = "cohortSettings"
  )
})

test_that("Assert targetCohorts", {
  expect_error(createCohortSettings(targetCohorts = "stuff"),
               c("Must be of type 'data.frame'"))
})

test_that("Assert eventCohorts", {
  expect_error(
    createCohortSettings(targetCohorts = targetCohorts,
                         eventCohorts = "stuff"),
    c("Must be of type 'data.frame'")
  )
})

# Minimal == All