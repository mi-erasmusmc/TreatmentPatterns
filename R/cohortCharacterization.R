
#' Only for OMOP-CDM TRUE: Perform baseline characterization for the the target cohorts.
#'
#' @param connection Connection to database server.
#' @param connectionDetails    Only for OMOP-CDM TRUE: An object of type connectionDetails as created using the createConnectionDetails function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Only for OMOP-CDM TRUE: Schema name where your patient-level data resides. Note that for SQL Server, 
#'                             this should include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Only for OMOP-CDM TRUE: Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_results.dbo'.
#' @param cohortTable          Only for OMOP-CDM TRUE: The name of the table that will be created in the cohortDatabaseSchema.
#'                             This table will hold the target and event cohorts used in this study.
#' @param instFolder           Name of local folder to place all settings and cohorts; make sure to use forward slashes (/).   
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes (/).
#' @param databaseId           Unique identifier for database (can be the same as databaseName).
#' @param standardCovariateSettings An object of type covariateSettings as created using the createCovariateSettings function in the FeatureExtraction package.
#'
#' @export
cohortCharacterization <- function(connection,
                                   connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   instFolder,
                                   outputFolder,
                                   databaseId,
                                   standardCovariateSettings) {
  
  if (!file.exists(paste0(outputFolder, "/characterization")))
    dir.create(paste0(outputFolder, "/characterization"), recursive = TRUE)
  
  # Load pathway settings
  pathwaySettings <- data.frame(readr::read_csv(paste0(instFolder, "/settings/pathway_settings.csv"), col_types = readr::cols()))
  
  # For all different target populations
  settings <- colnames(pathwaySettings)[grepl("analysis", colnames(pathwaySettings))]
  targetCohortIds <- unique(as.numeric(pathwaySettings[pathwaySettings$param == "targetCohortId",-1]))
  minCellCount <- max(as.integer(pathwaySettings[pathwaySettings$param == "minCellCount",settings])) # Minimum number of subjects in the target cohort for a given eent in order to be counted in the pathway
  
  # Count cohorts
  cohortCounts <- getCohortCounts(connection = connection,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable, 
                                  cohortIds = targetCohortIds)
  
  cohortCounts <- cohortCounts %>% 
    dplyr::mutate(databaseId = !!databaseId)
  
  # Add standard features
  standardCovariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE, useDemographicsGender = TRUE, useDemographicsTimeInCohort = TRUE, useDemographicsPostObservationTime = TRUE, useConditionGroupEraAnyTimePrior = TRUE, useConditionGroupEraLongTerm = TRUE, useCharlsonIndex = TRUE)
  
  # Add custom features
  settings_characterization <- readr::read_csv(paste0(instFolder, "/settings/characterization_settings.csv"), col_types = list("c", "c", "c"))
  custom <- settings_characterization[settings_characterization$covariateId == "Custom", ]
  
  if (nrow(custom) != 0) {
    for (c in 1:length(custom$covariateName)) {
      settings_characterization[settings_characterization$covariateName == custom$covariateName[c], "covariateId"] <- as.character((999000+c)*1000+999)
    }
    customCovariateSettings <- createCustomCovariateSettings(list_covariates = custom$covariateName)
    covariateSettings <- list(standardCovariateSettings, customCovariateSettings)
  } else {
    covariateSettings <- standardCovariateSettings
  }
  
  characteristics <- getCohortCharacteristics(connection = connection,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              cohortDatabaseSchema = cohortDatabaseSchema,
                                              cohortTable = cohortTable,
                                              cohortIds = targetCohortIds,
                                              covariateSettings = covariateSettings)
  
  exportCharacterization(characteristics = characteristics,
                         databaseId = databaseId,
                         incremental = FALSE,
                         covariateValueFileName = file.path(paste0(outputFolder, "/characterization"), "covariate_value.csv"),
                         covariateRefFileName = file.path(paste0(outputFolder, "/characterization"), "covariate_ref.csv"),
                         analysisRefFileName = file.path(paste0(outputFolder, "/characterization"), "analysis_ref.csv"),
                         counts = cohortCounts,
                         minCellCount = minCellCount)
  
  # Selection of standard results
  characterization <- readr::read_csv(paste0(outputFolder, "/characterization/covariate_value.csv"), col_types = list("i", "c", "d", "d", "c"))
  colnames(characterization) <- c("cohortId", "covariateId", "mean", "sd", "databaseId") 
  characterization <- merge(settings_characterization[,c("covariateId", "covariateName")], characterization, by = "covariateId")
  
  # Add cohort counts
  characterization <- rbind(characterization, cbind(covariateId = "Custom", covariateName = "Number of persons", cohortId = cohortCounts$cohortId, mean = cohortCounts$cohortEntries, sd = NA, databaseId = databaseId))
  
  write.csv(characterization, paste0(outputFolder, "/characterization/characterization.csv"), row.names = FALSE)
  
  
  ParallelLogger::logInfo("cohortCharacterization done.")
}
