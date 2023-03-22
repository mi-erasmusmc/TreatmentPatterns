# Libraries
library(testthat)
library(TreatmentPatterns)

targetCohorts <- data.frame(cohortId = c(1),
                            cohortName = c("Hypertension"))

eventCohorts <- data.frame(
  cohortId = c(10, 11, 12, 13, 14),
  cohortName = c(
    "Hydrochlorothiazide",
    "Metorolol",
    "Amlodipine",
    "Lisinopril",
    "Losartan"
  )
)

cohortSettings <- createCohortSettings(
  targetCohorts,
  eventCohorts)

test_that("Void", {
  expect_error(TreatmentPatterns:::checkCohorts())
})

test_that("eventCohorts", {
  expect_equal(TreatmentPatterns:::checkCohorts(eventCohorts),
               TRUE)
})

test_that("targetCohorts", {
  expect_equal(TreatmentPatterns:::checkCohorts(targetCohorts),
               TRUE)
})
