
#' Only for OMOP-CDM FALSE: Import the target and event cohorts from a csv file in which the cohorts are stored. 
#' 
#' @param cohortLocation Location from where cohorts can be loaded.
#' @param outputFolder Name of local folder to place results; make sure to use forward slashes (/).
#' @export
importCohorts <- function(cohortLocation,
                          outputFolder) {
  
  # Check if directory exists
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)
  
  # Load cohorts in from file
  # Required columns: cohortId, personId, startDate, endDate
  data <- data.table(readr::read_csv(cohortLocation), col_types = list("i", "i", "D", "D"))
  
  # Load information cohorts to create
  pathToCsv <- paste0(instFolder,"/settings/cohorts_to_create.csv")
  cohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols())
  write.csv(cohortsToCreate, file.path(outputFolder, "cohort.csv"), row.names = FALSE)
  
  # Check number of subjects per cohort
  ParallelLogger::logInfo("Counting cohorts")
  counts <- data.frame(cohortDefinitionId = cohortsToCreate$cohortId)
  
  counts$cohortCount <- sapply(counts$cohortDefinitionId, function(c) {
    length(data$person_id[data$cohortId == c]) 
  })
  
  counts$personCount <- sapply(counts$cohortDefinitionId, function(c) {
    length(unique(data$person_id[data$cohortId == c]))
  })
  
  write.csv(counts, file.path(outputFolder, "cohort_counts.csv"), row.names = FALSE)
  
  # Check if all cohorts have non-zero count
  checkCohorts <- setdiff(cohortsToCreate$cohortId,counts$cohortDefinitionId)
  
  if(length(checkCohorts) != 0) {
    warning(paste0("Cohort definition ", paste0(checkCohorts, collapse = ","), " has zero count. "))
  }
  
  ParallelLogger::logInfo("importCohorts done.")
}

