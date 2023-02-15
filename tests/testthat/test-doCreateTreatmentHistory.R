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
    current_cohorts = current_cohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments), "data.frame")
})

test_that("event cohorts", {
  event_cohorts = TreatmentPatterns:::doCreateTreatmentHistory(
    current_cohorts = current_cohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments)
  expect_gt(nrow(event_cohorts), 0)
  expect_equal(nrow(event_cohorts[!(event_cohorts$event_cohort_id %in% eventCohortIds)]), 0)
})

test_that("includeTreatments startDate", {
  event_cohorts = TreatmentPatterns:::doCreateTreatmentHistory(
    current_cohorts = current_cohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = "startDate")
  extended_event_cohorts = inner_join(
    event_cohorts,
    current_cohorts[current_cohorts$cohort_id == targetCohortId],
    by=c('person_id'))
  expect_equal(nrow(event_cohorts[extended_event_cohorts$event_start_date < extended_event_cohorts$start_date]), 0)
})

test_that("includeTreatments endDate", {
  event_cohorts = TreatmentPatterns:::doCreateTreatmentHistory(
    current_cohorts = current_cohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = "endDate")
  extended_event_cohorts = inner_join(
    event_cohorts,
    current_cohorts[current_cohorts$cohort_id == targetCohortId],
    by=c('person_id'))
  expect_equal(nrow(event_cohorts[extended_event_cohorts$event_end_date < extended_event_cohorts$start_date]), 0)
})

test_that("includeTreatments other", {
  expect_warning(TreatmentPatterns:::doCreateTreatmentHistory(
    current_cohorts = current_cohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = "other"))
})

test_that("periodPriorToIndex", {
  periodPriorToIndex = 365
  event_cohorts = TreatmentPatterns:::doCreateTreatmentHistory(
    current_cohorts = current_cohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments)
  extended_event_cohorts = inner_join(
    event_cohorts, 
    current_cohorts[current_cohorts$cohort_id == targetCohortId],
    by=c('person_id'))
  expect_equal(nrow(event_cohorts[(extended_event_cohorts$event_start_date - extended_event_cohorts$event_start_date) > periodPriorToIndex]), 0)
})
