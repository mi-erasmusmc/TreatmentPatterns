library(TreatmentPatterns)
library(testthat)

source(list.files(system.file(
  package = "TreatmentPatterns",
  "examples", "SettingObjects"), full.names = TRUE))

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParams.R"))

# === Compute prerequisites ====
doSetDurationTH <- doEraDurationTH

time1 <- Sys.time()

doSetDurationTH$event_cohort_id <- as.character(
  doSetDurationTH$event_cohort_id)

doSetDurationTH <- TreatmentPatterns:::selectRowsCombinationWindow(doSetDurationTH)

doSetDurationTH[
  SELECTED_ROWS == 1 & 
    (-GAP_PREVIOUS < combinationWindow & 
       !(-GAP_PREVIOUS == duration_era |
           -GAP_PREVIOUS == data.table::shift(duration_era, type = "lag"))), 
  switch := 1]

doSetDurationTH[
  SELECTED_ROWS == 1 &
    is.na(switch) &
    data.table::shift(event_end_date, type = "lag") <= event_end_date, 
  combination_FRFS := 1]

doSetDurationTH[
  SELECTED_ROWS == 1 &
    is.na(switch) &
    data.table::shift(event_end_date, type = "lag") >
    event_end_date, combination_LRFS := 1]

sumSwitchComb <- sum(
  sum(!is.na(doSetDurationTH$switch)), 
  sum(!is.na(doSetDurationTH$combination_FRFS)),
  sum(!is.na(doSetDurationTH$combination_LRFS)))

sumSelectedRows <- sum(doSetDurationTH$SELECTED_ROWS)

if (sumSwitchComb != sumSelectedRows) {
  warning(paste0(
    sum(doSetDurationTH$SELECTED_ROWS),
    ' does not equal total sum ',
    sum(!is.na(doSetDurationTH$switch)) + 
      sum(!is.na(doSetDurationTH$combination_FRFS)) + 
      sum(!is.na(doSetDurationTH$combination_LRFS))))
}

doSetDurationTH[
  , event_start_date_next := data.table::shift(event_start_date, type = "lead"),
  by = person_id]

doSetDurationTH[
  , event_end_date_previous := data.table::shift(event_end_date, type = "lag"),
  by = person_id]

doSetDurationTH[
  , event_end_date_next := data.table::shift(event_end_date, type = "lead"),
  by = person_id]

doSetDurationTH[
  , event_cohort_id_previous := data.table::shift(event_cohort_id, type = "lag"),
  by = person_id]

doSetDurationTH[data.table::shift(
  switch, 
  type = "lead") == 1,
  event_end_date := event_start_date_next]

add_rows_FRFS <- doSetDurationTH[combination_FRFS == 1, ]
add_rows_FRFS[, event_end_date := event_end_date_previous]

add_rows_FRFS[, event_cohort_id := paste0(
  event_cohort_id, "+", event_cohort_id_previous)]

doSetDurationTH[
  data.table::shift(combination_FRFS, type = "lead") == 1,
  c("event_end_date","check_duration") := list(event_start_date_next, 1)]

doSetDurationTH[
  combination_FRFS == 1,
  c("event_start_date", "check_duration") := list(
    event_end_date_previous, 1)]

doSetDurationTH[
  combination_LRFS == 1,
  event_cohort_id := paste0(
    event_cohort_id, "+", event_cohort_id_previous)]

add_rows_LRFS <- doSetDurationTH[
  data.table::shift(combination_LRFS, type = "lead") == 1, ]

add_rows_LRFS[
  , c("event_start_date", "check_duration") := list(
    event_end_date_next, 1)]

doSetDurationTH[
  data.table::shift(combination_LRFS, type = "lead") == 1,
  c("event_end_date", "check_duration") := list(event_start_date_next, 1)]

doSetDurationTH <- rbind(doSetDurationTH, add_rows_FRFS, fill = TRUE)
doSetDurationTH <- rbind(doSetDurationTH, add_rows_LRFS)

doSetDurationTH[
  , duration_era := difftime(
    event_end_date, event_start_date, units = "days")]

# === Tests ====
test_that("void", {
  expect_error(TreatmentPatterns:::doStepDuration())
})

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::doStepDuration(
    treatment_history = doSetDurationTH,
    minPostCombinationDuration = 30), "data.frame")
})

test_that("non numeric minPostCombinationDuration", {
  expect_error(
    TreatmentPatterns:::doStepDuration(
      treatment_history = doSetDurationTH,
      minPostCombinationDuration = "30"),
    "Must be of type 'numeric', not 'character'"
  )
})

test_that("non data.frame treatment_history", {
  expect_error(
    TreatmentPatterns:::doStepDuration(
      treatment_history = "doSetDurationTH",
      minPostCombinationDuration = "30"),
    "Must be of type 'data.frame', not 'character'"
  )
})

doStepDurationOut <- TreatmentPatterns:::doStepDuration(
  treatment_history = doSetDurationTH,
  minPostCombinationDuration = 30)

test_that("Max TH == Out", {
  expect_true(
    max(doStepDurationOut$duration_era) == max(doSetDurationTH$duration_era))
})

test_that("Min Out == 30 & != TH", {
  expect_false(
    min(doStepDurationOut$duration_era) == min(doSetDurationTH$duration_era) &&
      min(doStepDurationOut$duration_era) == 30)
})
