
#' This is the main function which runs all different parts of the treatment pathways analysis.
#' 
#' @param OMOP-CDM             Format of 'Observational Medical Outcomes Partnership Common Data Model' = TRUE or 'Other' = FALSE
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data resides if OMOP-CDM = TRUE.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param databaseName         Name of the database that will appear in the results.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the target and event cohorts used in this
#'                             study.
#' @param rootFolder           ...
#' @param instFolder           ...
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/).
#' @param tempFolder           ...
#' @param cohortLocation       Location where cohorts are saved if OMOP-CDM = FALSE.
#' @param targetCohorts        ...
#' @param eventCohorts         ...
#' @param characterizationSettings ...
#' @param pathwaySettings        Object that contains all study settings inputted by the user.
#' @param loadCohorts          Setting to load cohorts from ATLAS.
#' @param baseUrl              The base URL for the WebApi instance, for example: "http://server.org:80/WebAPI".
#'                             Note, there is no trailing '/'. If trailing '/' is used, you may receive an error. 
#' @param generateCohorts      Setting to extract specified target/event cohorts from database.
#' @param flowChart            Setting to return numbers for flowchart with inclusion/exclusion criteria. 
#' @param runCreateCohorts     Setting to run part of the analysis where cohorts are created.
#' @param runCohortCharacterization   Setting to run part of the analysis where characterization of target cohorts is done.     
#' @param standardCovariateSettings .... 
#' @param runConstructPathways Setting to run part of the analysis where treatment pathways are constructed.
#' @param runGenerateResults        Setting to run part of the analysis where final result files and plots are generated.
#' @import data.table
#' @import magrittr
#' @export

executeTreatmentPatterns <- function(OMOP_CDM = TRUE,
                                     connectionDetails = NULL,
                                     cdmDatabaseSchema,
                                     cohortDatabaseSchema = cdmDatabaseSchema,
                                     databaseName = "unknown",
                                     cohortTable = "cohort",
                                     rootFolder,
                                     instFolder = paste0(rootFolder, "/inst"),
                                     outputFolder = paste0(rootFolder, "/output/", databaseName),
                                     tempFolder = paste0(rootFolder, "/temp/", databaseName),
                                     cohortLocation = paste0(instFolder, "/cohorts/input_cohorts.csv"),
                                     targetCohorts = NULL,
                                     eventCohorts = NULL,
                                     characterizationSettings = NULL,
                                     pathwaySettings = NULL,
                                     loadCohorts = FALSE,
                                     baseUrl = NULL,
                                     generateCohorts = TRUE,
                                     flowChart = TRUE,
                                     runCreateCohorts = TRUE,
                                     runCohortCharacterization = TRUE,
                                     standardCovariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE, useDemographicsGender = TRUE, useDemographicsTimeInCohort = TRUE, useDemographicsPostObservationTime = TRUE, useConditionGroupEraAnyTimePrior = TRUE, useConditionGroupEraLongTerm = TRUE, useCharlsonIndex = TRUE),
                                     runConstructPathways = TRUE,
                                     runGenerateResults = TRUE) {
  
  ParallelLogger::clearLoggers()
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "treatmentpatterns_log.txt"))
  ParallelLogger::logInfo(print(paste0("Running package version ", packageVersion("TreatmentPatterns"))))
  
  if (OMOP_CDM) {
    connection <- DatabaseConnector::connect(connectionDetails)
  }
  
  # 0) Process input arguments
  ParallelLogger::logInfo(print("Process input arguments"))
  createStudySettings(instFolder,
                      targetCohorts,
                      eventCohorts,
                      characterizationSettings,
                      pathwaySettings) 

  # 1) Target/event cohorts of interest are extracted from the database (defined using ATLAS or custom concept sets created in SQL inserted into cohort template) 
  if (runCreateCohorts) {
    if (OMOP_CDM) {
      ParallelLogger::logInfo(print("runCreateCohorts OMOP-CDM TRUE"))
      
      createCohorts(connection = connection,
                    connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable,
                    outputFolder = outputFolder,
                    instFolder = instFolder,
                    loadCohorts = loadCohorts,
                    baseUrl = baseUrl,
                    generateCohorts = generateCohorts,
                    flowChart = flowChart)
    } else {
      ParallelLogger::logInfo("runCreateCohorts OMOP-CDM FALSE")
      
      importCohorts(cohortLocation = cohortLocation, outputFolder = outputFolder)
    }
  }
  
  # 2) Optional: Characterization of study/target population
  if (runCohortCharacterization & OMOP_CDM) {
    ParallelLogger::logInfo(print("runCohortCharacterization TRUE"))
    
    cohortCharacterization(connection = connection,
                           connectionDetails = connectionDetails,
                           cdmDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema = cohortDatabaseSchema,
                           cohortTable = cohortTable,
                           outputFolder = outputFolder,
                           instFolder = instFolder,
                           databaseId = databaseName,
                           targetCohortIds = targetCohortIds,
                           minCellCount = minCellCount,
                           standardCovariateSettings = standardCovariateSettings)
  }
  
  # 3) Treatment pathways are constructed
  if (runConstructPathways) {
    ParallelLogger::logInfo(print("runConstructPathways TRUE"))
    
    # Construct pathways for these cohorts
    constructPathways(OMOP_CDM = OMOP_CDM,
                      connection = connection,
                      cohortTable = cohortTable,
                      cohortDatabaseSchema = cohortDatabaseSchema,
                      dbms = connectionDetails$dbms,
                      cohortLocation = cohortLocation,
                      databaseName = databaseName,
                      studyName = studyName,
                      outputFolder = outputFolder,
                      tempFolder = tempFolder)
  }
  
  if (OMOP_CDM) {
    DatabaseConnector::disconnect(connection)
  }
  
  # 4) Output is generated
  if (runGenerateResults) {
    ParallelLogger::logInfo(print("runGenerateResults TRUE"))
    
    generateResults(databaseName = databaseName,
                    studyName = studyName,
                    outputFolder = outputFolder,
                    tempFolder = tempFolder)
    
    # Zip output folder
    zipName <- file.path(getwd(), paste0(databaseName, ".zip"))
    OhdsiSharing::compressFolder(file.path(outputFolder), zipName)
    
  }
  
  invisible(NULL)
}