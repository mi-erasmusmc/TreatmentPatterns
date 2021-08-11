
#' Optional, only for OMOP-CDM data: Perform baseline characterization of study/target population.
#'
#' @param dataSettings Settings object as created by createDataSettings().
#' @param characterizationSettings Settings object as created by createCharcterizationSettings().
#' @param saveSettings Settings object as created by createSaveSettings().
#'
#' @export
cohortCharacterization <- function(dataSettings, characterizationSettings, saveSettings) {
  
  # Check if directory exists and create if necessary
  if (!file.exists(file.path(saveSettings$outputFolder, "characterization")))
    dir.create(file.path(saveSettings$outputFolder, "characterization"), recursive = TRUE)
  
  # Connect to database
  connection <- DatabaseConnector::connect(dataSettings$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  # Get target cohort ids
  cohortsToCreate <- readr::read_csv(file.path(saveSettings$outputFolder, "settings", "cohorts_to_create.csv"), col_types = list("i","c","c","i","c"))
  targetCohortIds <- cohortsToCreate$cohortId[cohortsToCreate$cohortType == "target"]
  
  # Count cohorts
  cohortCounts <- getCohortCounts(connection = connection,
                                  cohortDatabaseSchema = dataSettings$cohortDatabaseSchema,
                                  cohortTable = dataSettings$cohortTable, 
                                  cohortIds = targetCohortIds)
  cohortCounts$databaseId <- saveSettings$databaseName
  
  # Add custom features
  baselineCovariates <- characterizationSettings$baselineCovariates
  customCovariates <- baselineCovariates[baselineCovariates$covariateId == "", ]
  
  if (nrow(customCovariates) != 0) {
    for (c in 1:length(customCovariates$covariateName)) {
      baselineCovariates[baselineCovariates$covariateName == customCovariates$covariateName[c], "covariateId"] <- as.character((999000+c)*1000+999)
    }
    customCovariateSettings <- createCustomCovariateSettings(list_covariates = customCovariates$covariateName)
    covariateSettings <- list(characterizationSettings$standardCovariateSettings, customCovariateSettings)
  } else {
    covariateSettings <- characterizationSettings$standardCovariateSettings
  }
  
  characteristics <- getCohortCharacteristics(connection = connection,
                                              cdmDatabaseSchema = dataSettings$cdmDatabaseSchema,
                                              cohortDatabaseSchema = dataSettings$cohortDatabaseSchema,
                                              cohortTable = dataSettings$cohortTable,
                                              cohortIds = targetCohortIds,
                                              covariateSettings = covariateSettings)
  
  exportCharacterization(characteristics = characteristics,
                         databaseId = saveSettings$databaseName,
                         incremental = FALSE,
                         covariateValueFileName = file.path(saveSettings$outputFolder, "characterization", "covariate_value.csv"),
                         covariateRefFileName = file.path(saveSettings$outputFolder, "characterization", "covariate_ref.csv"),
                         analysisRefFileName = file.path(saveSettings$outputFolder, "characterization", "analysis_ref.csv"),
                         counts = cohortCounts,
                         minCellCount = characterizationSettings$minCellCount)
  
  # Selection of standard results
  characterization <- readr::read_csv(file.path(saveSettings$outputFolder, "characterization", "covariate_value.csv"), col_types = list("i", "c", "d", "d", "c"))
  colnames(characterization) <- c("cohortId", "covariateId", "mean", "sd", "databaseId") 
  characterization <- merge(baselineCovariates[,c("covariateId", "covariateName")], characterization, by = "covariateId")
  
  # Add cohort counts
  characterization <- rbind(characterization, cbind(covariateId = "Custom", covariateName = "Number of persons", cohortId = cohortCounts$cohortId, mean = cohortCounts$cohortEntries, sd = NA, databaseId = saveSettings$databaseName))
  
  write.csv(characterization, file.path(saveSettings$outputFolder, "characterization", "characterization.csv"), row.names = FALSE)
  
  ParallelLogger::logInfo("cohortCharacterization done.")
}
