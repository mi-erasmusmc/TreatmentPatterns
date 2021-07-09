
#' This function will perform baseline characterization for the the target cohorts.
#'
#' @param connection Connection to database server.
#' @param connectionDetails An object of type \code{connectionDetails} as created using the
#'                             createConnectionDetails function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema   Schema name where your patient-level data resides if OMOP-CDM = TRUE.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the target and event cohorts used in this
#'                             study.
#' @param outputFolder          Name of local folder to place results; make sure to use forward slashes
#'                             (/).
#' @param databaseId           Unique identifier for database (can be the same as databaseName).
#' @param targetCohortIds      IDs to refer to target cohorts.
#' @param minCellCount         Minimum number of persons with a specific treatment pathway for the pathway to be included in analysis.
#' @param standardCovariateSettings ... .
#'
#' @export
cohortCharacterization <- function(connection,
                                   connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   outputFolder,
                                   instFolder,
                                   databaseId,
                                   targetCohortIds,
                                   minCellCount,
                                   standardCovariateSettings) {
  
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
  characterization <- readr::read_csv(paste0(outputFolder, "/characterization/covariate_value.csv"), col_types =list("i", "c", "d", "d", "c"))
  characterization <- merge(settings_characterization[,c("covariateId", "covariateName")], characterization, by = "covariateId")
  
  # Add cohort counts
  characterization <- rbind(characterization, cbind(covariate_id = "Custom", covariate_name = "Number of persons", cohort_id = cohortCounts$cohortId, mean = cohortCounts$cohortEntries, sd = NA, database_id = databaseId))
  
  write.csv(characterization, paste0(outputFolder, "/characterization/characterization.csv"), row.names = FALSE)
  
}
