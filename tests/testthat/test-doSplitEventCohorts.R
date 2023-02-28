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
  expect_error(TreatmentPatterns:::doSplitEventCohorts())
})

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::doSplitEventCohorts(
    treatment_history = doEraDurationTH,
    splitEventCohorts = splitEventCohorts,
    splitTime = splitTime,
    outputFolder = saveSettings$outputFolder), "data.frame")
})

test_that("length splitEventCohorts != splitTime", {
  expect_error(doSplitEventCohorts(
    treatment_history = doEraDurationTH,
    splitEventCohorts = c(1, 2, 3),
    splitTime = c("30"),
    outputFolder = saveSettings$outputFolder))
})

out3 <- doSplitEventCohorts(
  treatment_history = doEraDurationTH,
  splitEventCohorts = c(1),
  splitTime = c("30"),
  outputFolder = saveSettings$outputFolder)

out1 <- TreatmentPatterns:::doSplitEventCohorts(
  treatment_history = doEraDurationTH,
  splitEventCohorts = NA,
  splitTime = splitTime,
  outputFolder = saveSettings$outputFolder)

out2 <- doSplitEventCohorts(
  treatment_history = doEraDurationTH,
  splitEventCohorts = c(1, 6),
  splitTime = c("30", "20"),
  outputFolder = saveSettings$outputFolder)

expect_false(
  6 %in% out2$event_cohort_id || 1 %in% out2$event_cohort_id)

split6 <- length(out2$event_cohort_id[out2$event_cohort_id > 60])
org6 <- length(out1$event_cohort_id[out1$event_cohort_id == 6])

split1 <- length(out2$event_cohort_id[
  out2$event_cohort_id > 10 &
    out2$event_cohort_id < 20])
org1 <- length(out1$event_cohort_id[out1$event_cohort_id == 1])

test_that("splitEventCohorts ignored", {
  expect_true(all(out1 == doEraDurationTH, TRUE, na.rm = TRUE))
})

test_that("splitEventCohorts <- c(1)", {
  expect_false(all(out1 == out3, TRUE, na.rm = TRUE))
})

test_that("check multiple splits", {
  expect_true(split1 == org1)
  expect_true(split6 == org6)
})

test_that("check write cohort.csv", {
  cohorts <- read.csv(paste0(saveSettings$outputFolder, "/cohort.csv"))
  
  ace <- length(cohorts$cohortName[grep(
    pattern = "Ace\\w+ \\(", x = cohorts$cohortName)])
  
  pen <- length(cohorts$cohortName[grep(
    pattern = "Pen\\w+ \\(", x = cohorts$cohortName)])
  expect_equal(ace, 2)
  expect_equal(pen, 2)
})
