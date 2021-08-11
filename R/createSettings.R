
# TODO: check loading of files from settings / if understandable this way
# TODO: add class object and check if settings of right class in input script

#' Create data settings.
#'
#' @param OMOP-CDM             Format of database 'Observational Medical Outcomes Partnership Common Data Model' = TRUE or 'Other' = FALSE.
#' @param connectionDetails    Only for OMOP-CDM TRUE: An object of type connectionDetails as created using the createConnectionDetails function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Only for OMOP-CDM TRUE: Schema name where your patient-level data resides. Note that for SQL Server, 
#'                             this should include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Only for OMOP-CDM TRUE: Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_results.dbo'.
#' @param cohortTable          Only for OMOP-CDM TRUE: The name of the table that will be created in the cohortDatabaseSchema.
#'                             This table will hold the target and event cohorts used in this study.
#' @param cohortLocation       Only for OMOP-CDM FALSE: Location from where cohorts can be loaded.               
#' @return
#' @export
#'
#' @examples
createDataSettings <- function(OMOP_CDM = "TRUE",
                               connectionDetails = DatabaseConnector::createConnectionDetails(dbms = Sys.getenv('dbms'),
                                                                                              server = Sys.getenv('server'),
                                                                                              user = Sys.getenv('user'),
                                                                                              password = Sys.getenv('password'),
                                                                                              port = Sys.getenv('port')),
                               cdmDatabaseSchema = NULL,
                               cohortDatabaseSchema = NULL,
                               cohortTable = "treatmentpatterns_cohorts",
                               cohortLocation = NULL) {
  
  if (OMOP_CDM) {
    tryCatch(connection <- DatabaseConnector::connect(connectionDetails = connectionDetails),
             error = function(e){print(paste0("Problem with database connection: ", e))})
    on.exit(DatabaseConnector::disconnect(connection))
    
    if (is.null(cdmDatabaseSchema)) {
      stop('Need to specify cdmDatabaseSchema.')  
    }
    
    if (is.null(cohortDatabaseSchema)) {
      stop('Need to specify cohortDatabaseSchema.')  
    }
    
  } else {
    if (is.null(cohortLocation)) {
      stop('Need to specify cohortLocation.')  
    }
    
  }
  
  dataSettings <- list(OMOP_CDM = OMOP_CDM,
                       connectionDetails = connectionDetails,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortTable,
                       cohortLocation = cohortLocation)
  
  return(dataSettings)
}

#' Create cohort settings.
#'
#' @param cohortsToCreate_location Optional: Location of saved cohortsToCreate object.
#' @param targetCohorts        Data frame containing the study population of interest (cohortId, cohortName, atlasId, conceptSet).
#' @param eventCohorts         Data frame containing the events of interest (cohortId, cohortName, atlasId, conceptSet).
#' @param loadCohorts          Setting to retrieve cohort definitions with atlasId from ATLAS WebApi.
#' @param baseUrl              The base URL for the WebApi instance, for example: "http://server.org:80/WebAPI".
#'                             Note, there is no trailing '/'. If trailing '/' is used, you may receive an error. 
#' @param generateCohorts      Setting to (re)generate cohortTable in the database.
#' @param includeDescendants   Whether to include all descendants of Custom cohorts defined using conceptSet.
#'
#' @return
#' @export
createCohortSettings <- function(cohortsToCreate_location = NULL,
                                 targetCohorts = NULL,
                                 eventCohorts = NULL,
                                 loadCohorts = FALSE,
                                 cohortsFolder = NULL,
                                 baseUrl = NULL,
                                 generateCohorts = TRUE,
                                 includeDescendants = TRUE) {
  
  # If cohortsToCreate_location given, load settings from data
  if (!is.null(cohortsToCreate_location)) {
    cohortsToCreate <- readr::read_csv(cohortsToCreate_location, col_types = readr::cols())
  } else if (!is.null(targetCohorts) & !is.null(eventCohorts)) { # Otherwise create cohortsToCreate from targetCohorts and eventCohorts
    
    if (!("atlasId" %in% colnames(targetCohorts))) {
      targetCohorts$atlasId <- NA
    }
    
    if (!("conceptSet" %in% colnames(targetCohorts))) {
      targetCohorts$conceptSet <- NA
    }
    
    if (!("atlasId" %in% colnames(eventCohorts))) {
      eventCohorts$atlasId <- NA
    }
    
    if (!("conceptSet" %in% colnames(eventCohorts))) {
      eventCohorts$conceptSet <- NA
    }
    
    targetCohorts$cohortType <- 'target'
    eventCohorts$cohortType <- 'event'
    
    cohortsToCreate <- rbind(targetCohorts, eventCohorts)
    
  } else if ((is.null(targetCohorts) & !is.null(eventCohorts))) {
    stop("targetCohorts missing")
    
  } else if ( (!is.null(targetCohorts) & is.null(eventCohorts))) {
    stop("eventCohorts missing")
  }
  
  cohortsToCreate <- cohortsToCreate[,c('cohortId', 'cohortName', 'cohortType', 'atlasId', 'conceptSet')] # col_types = list("i","c","c","i","c")
  
  if (!loadCohorts & is.null(cohortsFolder)) {
    warning("cohortsFolder missing, location is assumed to be saveSettings$outputFolder/cohorts")
  }
  
  if (loadCohorts & is.null(baseUrl)) {
    stop("baseUrl missing")
  }
  
  cohortSettings <- list(cohortsToCreate = cohortsToCreate,
                         loadCohorts = loadCohorts,
                         cohortsFolder = cohortsFolder,
                         baseUrl = baseUrl,
                         generateCohorts = generateCohorts,
                         includeDescendants = includeDescendants)
  
  return(cohortSettings)
}

#' Create characterization settings (optional, only for OMOP-CDM data ).
#'
#' @param baselineCovariates_location Optional: Location of saved baselieCovariates object.
#' @param baselineCovariates Data frame containing the baseline characteristics of interest (covariateName, covariateId).
#' @param standardCovariateSettings An object of type covariateSettings as created using the createCovariateSettings function in the FeatureExtraction package.
#'
#' @return
#' @export
#'
#' @examples
createCharacterizationSettings <- function(baselineCovariates_location = NULL,
                                           baselineCovariates = data.frame(covariateName = c('Male', 'Age',  'Charlson comorbidity index score'),
                                                                           covariateId = c(8507001, 1002, 1901)),
                                           standardCovariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE,
                                                                                                                  useDemographicsGender = TRUE,
                                                                                                                  useDemographicsTimeInCohort = TRUE,
                                                                                                                  useDemographicsPostObservationTime = TRUE,
                                                                                                                  useConditionGroupEraAnyTimePrior = TRUE,
                                                                                                                  useConditionGroupEraLongTerm = TRUE,
                                                                                                                  useCharlsonIndex = TRUE),
                                           minCellCount = 5) {
  
  # If baselineCovariates_location given, load settings from data
  if (!is.null(baselineCovariates_location)) {
    baselineCovariates <- readr::read_csv(baselineCovariates_location, col_types = list("c", "c"))
  } 
  
  characterizationSettings <- list(baselineCovariates = baselineCovariates,
                                   standardCovariateSettings = standardCovariateSettings,
                                   minCellCount = minCellCount)
  
  # TODO: think about how to add custom covariates in here
  
  return(characterizationSettings)
}

#' Create pathway settings.
#'
#' @param pathwaySettings_location Optional: Location of saved pathwaySettings object.
#' @param pathwaySettings_list Create (list of pathway settings) with addPathwaySettings()
#' (e.g.pathwaySettings_list <- addPathwaySettings() or pathwaySettings_list <- list(addPathwaySettings(), addPathwaySettings())). . 
#' @param targetCohortId Target cohort ID of current study settings.
#' @param eventCohortIds Event cohort IDs of current study settings.
#'
#' @return
#' @export
#'
#' @examples
createPathwaySettings <- function(pathwaySettings_location = NULL,
                                  pathwaySettings_list = NULL,
                                  targetCohortId = NULL,
                                  eventCohortIds = NULL) {
  
  # If pathwaySettings_location given, load settings from data
  if (!is.null(pathwaySettings_location)) {
    pathwaySettings <- data.frame(readr::read_csv(pathwaySettings_location, col_types = readr::cols()))
    
  } else if (!is.null(pathwaySettings_list)) {
    pathwaySettings_all <- do.call("rbind", pathwaySettings_list)
    
    pathwaySettings <- data.table::transpose(pathwaySettings_all)
    colnames(pathwaySettings) <- paste0("analysis", 1:ncol(pathwaySettings))
    pathwaySettings <- cbind(param = colnames(pathwaySettings_all), pathwaySettings)
    
  } else if (!is.null(targetCohortId) & !is.null(eventCohortIds)) {
    # TODO: check if input numeric
    
    pathwaySettings_default <- addPathwaySettings(targetCohortId = targetCohortId,
                                                  eventCohortIds = eventCohortIds)
    
    pathwaySettings <- data.table::transpose(pathwaySettings_default)
    colnames(pathwaySettings) <- paste0("analysis", 1:ncol(pathwaySettings))
    pathwaySettings <- cbind(param = colnames(pathwaySettings_default), pathwaySettings)
  } else {
    stop("input missing, isert 1) pathwaySettings_location, 2) pathwaySettings_list, or 3) targetCohortId and eventCohortIds")
  }
  
  return(pathwaySettings)
}


#' Add set of pathway settings.
#'
#' @param studyName Name identifying the set of study parameters.
#' @param targetCohortId Target cohort ID of current study settings.
#' @param eventCohortIds Event cohort IDs of current study settings.
#' @param includeTreatmentsPriorToIndex # Number of days prior to the index date of the target cohort that event cohorts are allowed to start
#' @param minEraDuration  # Minimum time an event era should last to be included in analysis
#' @param splitEventCohorts # Specify event cohort to split in acute (< 30 days) and therapy (>= 30 days)
#' @param eraCollapseSize  # Window of time between which two eras of the same event cohort are collapsed into one era
#' @param combinationWindow # Window of time two event cohorts need to overlap to be considered a combination treatment
#' @param minStepDuration # Minimum time an event era before or after a generated combination treatment should last to be included in analysis
#' @param filterTreatments  # Select first occurrence of / changes between / all event cohorts
#' @param maxPathLength  # Maximum number of steps included in treatment pathway (max 5)
#' @param minCellCount # Minimum number of persons with a specific treatment pathway for the pathway to be included in analysis
#' @param minCellMethod # Select to completely remove / sequentially adjust (by removing last step as often as necessary) treatment pathways below minCellCount
#' @param groupCombinations # Select to group all non-fixed combinations in one category 'other’ in the sunburst plot
#' @param addNoPaths # Select to include untreated persons without treatment pathway in the sunburst plot
#'
#' @return
#' @export
addPathwaySettings <- function(studyName = c("default"),
                               targetCohortId,
                               eventCohortIds,
                               includeTreatmentsPriorToIndex = 0,
                               minEraDuration = 0,
                               splitEventCohorts = "",
                               eraCollapseSize = 0,
                               combinationWindow = 30, 
                               minStepDuration = 30,
                               filterTreatments = "First",
                               maxPathLength = 5, 
                               minCellCount = 0,
                               minCellMethod = "Remove",
                               groupCombinations = 10,
                               addNoPaths = FALSE) {
  
  # TODO: check targetCohortId and eventCohortIds given
  
  # TODO: remove spaces from file / arguments
  
  # TODO: if a parameter changed -> change name default
  
  settings <- data.frame(studyName = studyName,
                         targetCohortId = targetCohortId,
                         eventCohortIds = paste(eventCohortIds, collapse = ","),
                         includeTreatmentsPriorToIndex = includeTreatmentsPriorToIndex,
                         minEraDuration = minEraDuration,
                         splitEventCohorts = splitEventCohorts,
                         eraCollapseSize = eraCollapseSize,
                         combinationWindow = combinationWindow, 
                         minStepDuration = minStepDuration,
                         filterTreatments = filterTreatments,
                         maxPathLength = maxPathLength, 
                         minCellCount = minCellCount,
                         minCellMethod = minCellMethod,
                         groupCombinations = groupCombinations,
                         addNoPaths = addNoPaths)    
  
  return(settings)
}

#' Create save settings.
#'
#' @param databaseName         Name of the database that will appear in the results.
#' @param rootFolder           Name of local folder to place all package output (outputFolder, tempFolder if not given).
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes (/).
#' @param tempFolder           Name of local folder to place intermediate results (not to be shared); make sure to use forward slashes (/).
#'
#' @export
createSaveSettings <- function(databaseName = "unknown_name",
                               rootFolder = getwd(),
                               outputFolder = paste0(rootFolder, "/output"),
                               tempFolder = paste0(rootFolder, "/temp")) {
  
  saveSettings <- list(databaseName = databaseName,
                       rootFolder = rootFolder,
                       outputFolder = outputFolder,
                       tempFolder = tempFolder)
  
  return(saveSettings)
}
