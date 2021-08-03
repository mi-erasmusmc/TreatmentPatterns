
#' This is the main function which runs all parts of the treatment pathways analysis. The following tasks are performed sequentially:
#' 0) If input arguments given, use these to create study settings
#' 1) Target/event cohorts of interest are created
#' 2) Characterization of study/target population
#' 3) Treatment pathways are constructed
#' 4) Output is generated (sunburst plots and more)
#' 5) Launch shiny application to visualize the results
#' 
#' @param OMOP-CDM             Format of database 'Observational Medical Outcomes Partnership Common Data Model' = TRUE or 'Other' = FALSE.
#' @param connectionDetails    Only for OMOP-CDM TRUE: An object of type connectionDetails as created using the createConnectionDetails function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Only for OMOP-CDM TRUE: Schema name where your patient-level data resides. Note that for SQL Server, 
#'                             this should include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Only for OMOP-CDM TRUE: Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_results.dbo'.
#' @param databaseName         Name of the database that will appear in the results.
#' @param cohortTable          Only for OMOP-CDM TRUE: The name of the table that will be created in the cohortDatabaseSchema.
#'                             This table will hold the target and event cohorts used in this study.
#' @param rootFolder           Name of local folder to place all package output (instFolder, outputFolder, tempFolder if not given).
#' @param instFolder           Name of local folder to place all settings and cohorts; make sure to use forward slashes (/).   
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes (/).
#' @param tempFolder           Name of local folder to place intermediate results (not to be shared); make sure to use forward slashes (/).
#' @param cohortLocation       Only for OMOP-CDM FALSE: Location from where cohorts can be loaded.
#' @param targetCohorts        Data frame containing the study population of interest (cohortId, atlasId, cohortName).
#' @param eventCohorts         Data frame containing the events of interest (cohortId, atlasId, cohortName).
#' @param characterizationSettings Only for OMOP-CDM TRUE: Data frame containing the baseline characteristics of interest (covariateName, covariateId).
#' @param pathwaySettings      Data frame containing all pathway settings.
#' @param loadCohorts          Only for OMOP-CDM TRUE: Setting to retrieve cohort definitions from ATLAS WebApi.
#' @param baseUrl              Only for OMOP-CDM TRUE: The base URL for the WebApi instance, for example: "http://server.org:80/WebAPI".
#'                             Note, there is no trailing '/'. If trailing '/' is used, you may receive an error. 
#' @param generateCohorts      Only for OMOP-CDM TRUE: Setting to (re)generate cohortTable in the database.
#' @param flowChart            Only for OMOP-CDM TRUE: Setting to return numbers for flowchart with inclusion/exclusion criteria for ATLAS cohorts. 
#' @param standardCovariateSettings An object of type covariateSettings as created using the createCovariateSettings function in the FeatureExtraction package.
#' @param runCreateCohorts     Setting to run 1) Target/event cohorts of interest are created.
#' @param runCohortCharacterization  Only for OMOP-CDM TRUE: Setting to run 2) Characterization of study/target population.  
#' @param runConstructPathways Setting to run 3) Treatment pathways are constructed.
#' @param runGenerateResults        Setting to run 4) Output is generated (sunburst plots and more).
#' @param launchShiny           Setting to run 5) Launch shiny application to visualize the results.
#' @import data.table
#' @import magrittr
#' @export
#' @examples

executeTreatmentPatterns <- function(OMOP_CDM = TRUE,
                                     connectionDetails = NULL, # only for OMOP-CDM TRUE
                                     cdmDatabaseSchema,  # only for OMOP-CDM TRUE
                                     cohortDatabaseSchema,  # only for OMOP-CDM TRUE
                                     databaseName = "unknown_name",
                                     cohortTable = "treatmentpatterns_cohorts",  # only for OMOP-CDM TRUE
                                     rootFolder = getwd(),
                                     instFolder = paste0(rootFolder, "/inst"),
                                     outputFolder = paste0(rootFolder, "/output/", databaseName),
                                     tempFolder = paste0(rootFolder, "/temp/", databaseName),
                                     cohortLocation = paste0(instFolder, "/cohorts/input_cohorts.csv"), # only for OMOP-CDM FALSE
                                     targetCohorts = NULL,
                                     eventCohorts = NULL,
                                     characterizationSettings = NULL, # # only for OMOP-CDM TRUE
                                     pathwaySettings = NULL,
                                     loadCohorts = FALSE,  # only for OMOP-CDM TRUE
                                     baseUrl = NULL, # only for OMOP-CDM TRUE
                                     generateCohorts = TRUE,  # only for OMOP-CDM TRUE
                                     flowChart = TRUE, # only for OMOP-CDM TRUE
                                     standardCovariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE,
                                                                                                            useDemographicsGender = TRUE,
                                                                                                            useDemographicsTimeInCohort = TRUE,
                                                                                                            useDemographicsPostObservationTime = TRUE,
                                                                                                            useConditionGroupEraAnyTimePrior = TRUE,
                                                                                                            useConditionGroupEraLongTerm = TRUE,
                                                                                                            useCharlsonIndex = TRUE), # only for OMOP-CDM TRUE
                                     runCreateCohorts = TRUE,
                                     runCohortCharacterization = TRUE, # OMOP-CDM TRUE
                                     runConstructPathways = TRUE,
                                     runGenerateResults = TRUE,
                                     launchShiny = FALSE) {
  
  ParallelLogger::clearLoggers()
  ParallelLogger::addDefaultFileLogger(fileName = file.path(outputFolder, "treatmentpatterns_log.txt"),
                                       name = "TreatmentPatterns_Logger")
  ParallelLogger::logInfo(print(paste0("Running package version ", packageVersion("TreatmentPatterns"))))
  
  if (OMOP_CDM) {
    connection <- DatabaseConnector::connect(connectionDetails)
  }
  
  # 0) If input arguments given, use these to create study settings
  ParallelLogger::logInfo(print("Process input arguments"))
  TreatmentPatterns::createStudySettings(instFolder = instFolder,
                                         targetCohorts = targetCohorts,
                                         eventCohorts = eventCohorts,
                                         characterizationSettings = characterizationSettings,
                                         pathwaySettings = pathwaySettings) 
  
  # 1) Target/event cohorts of interest are created
  if (runCreateCohorts) {
    if (OMOP_CDM) { # only for OMOP-CDM TRUE
      ParallelLogger::logInfo(print("runCreateCohorts OMOP-CDM TRUE"))
      
      TreatmentPatterns::createCohorts(connection = connection,
                                       connectionDetails = connectionDetails,
                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                       cohortDatabaseSchema = cohortDatabaseSchema,
                                       cohortTable = cohortTable,
                                       instFolder = instFolder,
                                       outputFolder = outputFolder,
                                       loadCohorts = loadCohorts,
                                       baseUrl = baseUrl,
                                       generateCohorts = generateCohorts,
                                       flowChart = flowChart)
    } else { # for OMOP-CDM FALSE
      ParallelLogger::logInfo("runCreateCohorts OMOP-CDM FALSE")
      
      TreatmentPatterns::importCohorts(cohortLocation = cohortLocation,
                                       outputFolder = outputFolder)
    }
  }
  
  # 2) Optional: Characterization of study/target population
  if (runCohortCharacterization & OMOP_CDM) { # only for OMOP-CDM TRUE
    ParallelLogger::logInfo(print("runCohortCharacterization TRUE"))
    
    TreatmentPatterns::cohortCharacterization(connection = connection,
                                              connectionDetails = connectionDetails,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              cohortDatabaseSchema = cohortDatabaseSchema,
                                              cohortTable = cohortTable,
                                              instFolder = instFolder,
                                              outputFolder = outputFolder,
                                              databaseId = databaseName,
                                              targetCohortIds = targetCohortIds,
                                              minCellCount = minCellCount,
                                              standardCovariateSettings = standardCovariateSettings)
  }
  
  # 3) Treatment pathways are constructed
  if (runConstructPathways) {
    ParallelLogger::logInfo(print("runConstructPathways TRUE"))
    
    # Construct pathways for these cohorts
    TreatmentPatterns::constructPathways(OMOP_CDM = OMOP_CDM,
                                         connection = connection, # only for OMOP-CDM TRUE
                                         connectionDetails = connectionDetails, # only for OMOP-CDM TRUE
                                         cohortTable = cohortTable, # only for OMOP-CDM TRUE
                                         cohortDatabaseSchema = cohortDatabaseSchema, # only for OMOP-CDM TRUE
                                         cohortLocation = cohortLocation,  # only for OMOP-CDM FALSE
                                         databaseName = databaseName,
                                         studyName = studyName,
                                         outputFolder = outputFolder,
                                         tempFolder = tempFolder)
  }
  
  if (OMOP_CDM) {
    DatabaseConnector::disconnect(connection)
  }
  
  # 4) Output is generated (sunburst plots and more)
  if (runGenerateResults) {
    ParallelLogger::logInfo(print("runGenerateResults TRUE"))
    
    TreatmentPatterns::generateResults(databaseName = databaseName,
                                       studyName = studyName,
                                       outputFolder = outputFolder,
                                       tempFolder = tempFolder)
    
    # Zip output folder
    zipName <- file.path(rootFolder, paste0(databaseName, ".zip"))
    OhdsiSharing::compressFolder(file.path(outputFolder), zipName)
  }
  
  # 5) Launch shiny application to visualize the results
  if (launchShiny) {
    ParallelLogger::logInfo(print("launchShiny TRUE"))
    
    TreatmentPatterns::launchShinyApplication(zipFolder = rootFolder)
    
  }
  ParallelLogger::unregisterLogger("TreatmentPatterns_Logger")
  invisible(NULL)
}