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
  expect_error(checkPathwaySettings())
})

test_that("Minimal", {
  expect_equal(checkPathwaySettings(cohortSettings),
               TRUE)
})

#optionalParam1

targetCohorts <- data.frame(cohortId = c(1),
                            cohortName = c('Hypertension'))

eventCohorts <- data.frame(
  cohortId = c("JAMES", 11, 12, 13, 14),
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

test_that("optionalParam1", {
  expect_error(checkPathwaySettings(cohortSettings))
})




