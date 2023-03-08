library(TreatmentPatterns)
library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("Minimal", {
  expect_output(TreatmentPatterns:::outputSankeyDiagram(
    data = treatment_pathways[[1]],
    outputFolder = saveSettings$outputFolder,
    databaseName = saveSettings$databaseName,
    studyName = "Viral_Sinusitis",
    groupCombinations = TRUE
  ),
  "outputSankeyDiagram done")
  
  expect_true(file.exists(file.path(
    saveSettings$outputFolder,
    "Viral_Sinusitis",
    paste0("Eunomia", "_", "Viral_Sinusitis", "_all_sankeydiagram.html"))))
})

# data <- TreatmentPatterns:::groupInfrequentCombinations(
#   data = treatment_pathways[[1]], groupCombinations = TRUE)
# 
# data[is.na(data)] <- "Stopped"
# 
# data$event_cohort_name1 <- paste0("1. ", data$event_cohort_name1)
# data$event_cohort_name2 <- paste0("2. ", data$event_cohort_name2)
# data$event_cohort_name3 <- paste0("3. ", data$event_cohort_name3)
# 
# results1 <- data %>%
#   dplyr::group_by(event_cohort_name1, event_cohort_name2) %>%
#   dplyr::summarise(freq = sum(freq))
# 
# data
# 
# data %>%
#   dplyr::group_by(event_cohort_name2, event_cohort_name3) %>%
#   dplyr::summarise(freq = sum(freq))
# 
# colnames(results1) <- c("source", "target", "value")
# colnames(results2) <- c("source", "target", "value")
# links <- as.data.frame(rbind(results1, results2))
