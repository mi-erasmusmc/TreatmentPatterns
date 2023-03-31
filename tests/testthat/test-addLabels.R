library(TreatmentPatterns)
library(testthat)

source(list.files(system.file(
  package = "TreatmentPatterns",
  "examples", "SettingObjects"), full.names = TRUE))

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParams.R"))

test_that("void", {
  expect_error(TreatmentPatterns:::addLabels())
})

test_that("minimal", {
  expect_s3_class(treatmentHistory <- TreatmentPatterns:::addLabels(
    doMaxPathLengthTH,
    saveSettings$outputFolder), "data.frame")
})

test_that("validate read file", {
  expect_s3_class(read.csv(
    file = file.path(
      saveSettings$outputFolder,
      "cohortsToCreate.csv")),
    "data.frame")
})

labels <- read.csv(
  file = file.path(
    saveSettings$outputFolder,
    "cohortsToCreate.csv"))

# convenrt event_cohort_id to character
labels["cohortId"] <- as.character(labels[, "cohortId"])

labels <- labels[labels$cohortType == "event", c("cohortId", "cohortName")]
colnames(labels) <- c("event_cohort_id", "event_cohort_name")

test_that("read file correctly", {
  expect_true(
    all(labels$event_cohort_id %in% treatmentHistory$event_cohort_id,
    TRUE))
})

treatmentHistoryMerged <- merge(
  x = doMaxPathLengthTH,
  y = labels,
  all.x = TRUE,
  by = "event_cohort_id")

test_that("Add events to TreatmentHistory", {
  expect_true(ncol(treatmentHistoryMerged) == 1 + ncol(doMaxPathLengthTH))
  expect_true(nrow(treatmentHistoryMerged) == nrow(doMaxPathLengthTH))
})

b <- sapply(
    X = treatmentHistoryMerged$event_cohort_id[
      is.na(treatmentHistoryMerged$event_cohort_name)],
    FUN = function(x) {
      # Revert search to look for longest concept_ids first

      for (l in seq_len(nrow(labels))) {
        # If treatment occurs twice in a combination (as monotherapy and as part
        # of fixed-combination) -> remove monotherapy occurrence
        if (any(grep(labels$event_cohort_name[l], x))) {
          x <- gsub(labels$event_cohort_id[l], "", x)
        } else {
          x <- gsub(labels$event_cohort_id[l], labels$event_cohort_name[l], x)
        }
      }
      return(x)
    })

test_that("validate NA's are overwritten", {
  expect_identical(
    sum(is.na(treatmentHistoryMerged$event_cohort_name)),
    length(b))
})

test_that("validate pattern \\++ to +", {
  expect_true(all(
    gsub(
      pattern = "(^\\++|\\++$)",
      replacement = "",
      x = c(
        "Acetaminophen+++",
        "+++Acetaminophen",
        "+++Acetaminophen+++"
      )
    ) == c("Acetaminophen", "Acetaminophen", "Acetaminophen"),
    TRUE
  ))
})
