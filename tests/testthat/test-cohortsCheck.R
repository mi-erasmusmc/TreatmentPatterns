# Libraries
library(testthat)
library(TreatmentPatterns)

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

cohortSettings <- createCohortSettings(
  targetCohorts, 
  eventCohorts)

test_that("Void", {
  expect_error(cohortsCheck())
})

test_that("eventCohorts", {
  expect_equal(cohortsCheck(eventCohorts),
               TRUE)
})

test_that("targetCohorts", {
  expect_equal(cohortsCheck(targetCohorts),
               TRUE)
})

