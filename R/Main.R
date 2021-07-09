
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
#' @param studySettings        ...
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
#' @param study_settings       Object that contains all study settings inputted by the user.
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
                                     targetCohorts = NULL, # new
                                     eventCohorts = NULL, # new
                                     characterizationSettings = NULL, # new
                                     studySettings = NULL, # new
                                     loadCohorts = FALSE,
                                     baseUrl = NULL,
                                     generateCohorts = TRUE,
                                     flowChart = TRUE,
                                     runCreateCohorts = TRUE,
                                     runCohortCharacterization = TRUE,
                                     standardCovariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE, useDemographicsGender = TRUE, useDemographicsTimeInCohort = TRUE, useDemographicsPostObservationTime = TRUE, useConditionGroupEraAnyTimePrior = TRUE, useConditionGroupEraLongTerm = TRUE, useCharlsonIndex = TRUE),
                                     runConstructPathways = TRUE,
                                     runGenerateResults = TRUE) {
  # Input checks
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)
  
  if (!file.exists(paste0(instFolder, "/settings"))) { 
    dir.create(paste0(instFolder, "/settings"), recursive = TRUE)
  } 
  
  if (!is.null(targetCohorts) & !is.null(eventCohorts)) {
    ParallelLogger::logInfo(print("Create cohorts_to_create.csv from arguments"))
    
    targetCohorts$cohortType <- 'target'
    eventCohorts$cohortType <- 'event'
    
    cohorts_to_create <- rbind(targetCohorts, eventCohorts)
    cohorts_to_create$cohortDefinition <- "ATLAS" # only possible to use ATLAS cohorts when using function arguments
    cohorts_to_create <- cohorts_to_create[,c('cohortId', 'cohortName', 'cohortDefinition', 'cohortType', 'atlasId')]
    write.csv(cohorts_to_create, paste0(instFolder, "/settings/cohorts_to_create.csv"), row.names = FALSE)
  } else if ((is.null(targetCohorts) & !is.null(eventCohorts))) {
    ParallelLogger::logWarn("File cohorts_to_create not created, targetCohorts missing")
  } else if ( (!is.null(targetCohorts) & is.null(eventCohorts))) {
    ParallelLogger::logWarn("File cohorts_to_create not created, eventCohorts missing")
  }
  
  if (!is.null(studySettings)) {
    ParallelLogger::logInfo(print("Create study_settings.csv from arguments"))
    study_settings <- transpose(studySettings)
    colnames(study_settings) <- paste0("analysis", 1:ncol(study_settings))
    study_settings <- cbind(param = colnames(studySettings), study_settings)
    write.csv(study_settings, paste0(instFolder, "/settings/study_settings.csv"), row.names = FALSE)
  }
  
  if (!is.null(characterizationSettings)) {
    ParallelLogger::logInfo(print("Create characterization_settings.csv from arguments"))
    write.csv(characterizationSettings, paste0(instFolder, "/settings/characterization_settings.csv"), row.names = FALSE)
  }

  # Load study settings and start analysis
  study_settings <- data.frame(readr::read_csv(paste0(instFolder, "/settings/study_settings.csv"), col_types = readr::cols()))
  
  if (OMOP_CDM) {
    connection <- DatabaseConnector::connect(connectionDetails)
  }
  
  ParallelLogger::clearLoggers()
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::logInfo(print(paste0("Running package version ", packageVersion("TreatmentPatterns"))))
  
  # Target/event cohorts of interest are extracted from the database (defined using ATLAS or custom concept sets created in SQL inserted into cohort template) 
  if (runCreateCohorts) {
    if (OMOP_CDM) {
      ParallelLogger::logInfo(print("runCreateCohorts OMOP-CDM TRUE"))
      
      # For all different target populations
      settings <- colnames(study_settings)[grepl("analysis", colnames(study_settings))]
      minCellCount <- max(as.integer(study_settings[study_settings$param == "minCellCount",settings])) # Minimum number of subjects in the target cohort for a given eent in order to be counted in the pathway
      
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
                    minCellCount = minCellCount,
                    flowChart = flowChart)
    } else {
      ParallelLogger::logInfo("runCreateCohorts Other TRUE")
      importCohorts(cohortLocation = cohortLocation, outputFolder = outputFolder)
    }
  }
  
  # Characterization of study/target population
  if (runCohortCharacterization & OMOP_CDM) {
    ParallelLogger::logInfo(print("runCohortCharacterization TRUE"))
    
    if (!file.exists(paste0(outputFolder, "/characterization")))
      dir.create(paste0(outputFolder, "/characterization"), recursive = TRUE)
    
    # For all different target populations
    settings <- colnames(study_settings)[grepl("analysis", colnames(study_settings))]
    targetCohortIds <- unique(as.numeric(study_settings[study_settings$param == "targetCohortId",-1]))
    minCellCount <- max(as.integer(study_settings[study_settings$param == "minCellCount",settings])) # Minimum number of subjects in the target cohort for a given eent in order to be counted in the pathway
    
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
  
  # Treatment pathways are constructed
  if (runConstructPathways) {
    ParallelLogger::logInfo(print("runConstructPathways TRUE"))
    
    # Load cohorts
    if (OMOP_CDM) {
      # Get cohorts from database and save as csv
      sql <- loadRenderTranslateSql(sql = "SELECT * FROM @resultsSchema.@cohortTable",
                                    dbms = connectionDetails$dbms,
                                    resultsSchema=cohortDatabaseSchema,
                                    cohortTable=cohortTable)
      all_data <- data.table::as.data.table(DatabaseConnector::querySql(connection, sql))
      
    } else {
      # Load cohorts in from file
      # Required columns: cohortId, personId, startDate, endDate
      all_data <- data.table::as.data.table(readr::read_csv(cohortLocation), col_types = list("i", "i", "D", "D"))
    }
    colnames(all_data) <- c("cohort_id", "person_id", "start_date", "end_date")   
    
    constructPathways(all_data = all_data,
                      study_settings = study_settings,
                      databaseName = databaseName,
                      studyName = studyName,
                      outputFolder = outputFolder,
                      tempFolder = tempFolder)
  }
  
  if (OMOP_CDM) {
    DatabaseConnector::disconnect(connection)
  }
  
  # Output is generated
  if (runGenerateResults) {
    ParallelLogger::logInfo(print("runGenerateResults TRUE"))
    
    generateResults(study_settings = study_settings,
                    databaseName = databaseName,
                    studyName = studyName,
                    outputFolder = outputFolder,
                    tempFolder = tempFolder)
    
    write.csv(study_settings, file.path(outputFolder, "settings.csv"), row.names = FALSE)
    
    # Zip output folder
    zipName <- file.path(getwd(), paste0(databaseName, ".zip"))
    OhdsiSharing::compressFolder(file.path(outputFolder), zipName)
    
  }
  
  invisible(NULL)
}