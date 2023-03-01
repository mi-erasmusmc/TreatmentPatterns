library(TreatmentPatterns)
library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "examples", "SettingObjects", "createDummySettings.R"))

source(system.file(
  package = "TreatmentPatterns",
  "testing",
  "testParams.R"))

test_that("void", {
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory())
})

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments), "data.frame")
})

test_that("event cohorts", {
  eventCohorts <- TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments)
  expect_gt(nrow(eventCohorts), 0)
  expect_equal(
    nrow(eventCohorts[!(eventCohorts$event_cohort_id %in% eventCohortIds)]),
    0)
})

test_that("non-dataframe current_cohorts", {
  currentCohorts <- c(1, 2)
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments))
})

test_that("too few columns currentCohorts", {
  # remove end_date column
  currentCohorts <- currentCohorts[, c("cohort_id", "person_id", "start_date")]
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments))
})

test_that("wrong column names dataframe currentCohorts", {
  colnames(currentCohorts) <- c("cohort", "person", "startDate", "endDate")
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments))
})

test_that("non-character targetCohortId", {
  targetCohortId <- 7
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments))
})

test_that("multiple target cohorts", {
  targetCohortId <- c("6", "7")
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments))
})

test_that("non-character eventCohortIds", {
  eventCohortIds <- c(1, 2, 3, 4, 5, 6)
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments))
})

test_that("non-integer periodPriorToIndex", {
  periodPriorToIndex <- "A"
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments))
})

test_that("includeTreatments startDate", {
  eventCohorts <- TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = "startDate")
  extendedEventCohorts <- inner_join(
    eventCohorts,
    currentCohorts[currentCohorts$cohort_id == targetCohortId],
    by = c("person_id"))
  expect_equal(
    nrow(eventCohorts[extendedEventCohorts$event_start_date < 
                        extendedEventCohorts$start_date]),
    0)
})

test_that("includeTreatments endDate", {
  eventCohorts <- TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = "endDate")
  extendedEventCohorts <- inner_join(
    eventCohorts,
    currentCohorts[currentCohorts$cohort_id == targetCohortId],
    by = c("person_id"))
  expect_equal(
    nrow(eventCohorts[extendedEventCohorts$event_end_date < 
                        extendedEventCohorts$start_date]),
    0)
})

test_that("includeTreatments other", {
  expect_warning(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = "other"))
})

test_that("periodPriorToIndex", {
  periodPriorToIndex <- 365
  eventCohorts <- TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments)
  extendedEventCohorts <- inner_join(
    eventCohorts,
    currentCohorts[currentCohorts$cohort_id == targetCohortId],
    by = c("person_id"))
  expect_equal(
    nrow(eventCohorts[(extendedEventCohorts$event_start_date - 
                         extendedEventCohorts$event_start_date) >
                        periodPriorToIndex]), 
    0)
})
