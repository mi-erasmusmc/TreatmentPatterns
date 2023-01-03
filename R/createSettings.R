createDataSettingsChecks <- function(
    omopCDM,
    connectionDetails,
    cdmDatabaseSchema,
    resultSchema,
    cohortTable) {
  # Check omopCDM
  checkmate::checkLogical(
    x = omopCDM,
    len = 1,
    null.ok = FALSE)
  
  # Check connectionDetails
  checkmate::checkClass(
    connectionDetails,
    "connectionDetails")
  
  checkmate::checkCharacter(
    x = connectionDetails$dbms,
    len = 1,
    null.ok = FALSE)
  
  # check cdmDatabaseSchema
  checkmate::checkCharacter(
    cdmDatabaseSchema,
    null.ok = FALSE,
    len = 1)
  
  # check resultSchema
  checkmate::checkCharacter(
    resultSchema,
    null.ok = FALSE,
    len = 1)
  
  # cohortTable
  checkmate::checkCharacter(
    cohortTable,
    null.ok = FALSE,
    len = 1)
  
  return(TRUE)
}

#' createDataSettings
#' 
#' Create a dataSettings object containing information about how to connect to
#' a database. 
#'
#' @param omopCDM
#'     Format of database 'Observational Medical Outcomes Partnership Common
#'     Data Model' = TRUE or 'Other' = FALSE.
#' 
#' @param connectionDetails
#'     Only for omopCDM TRUE: An object of type connectionDetails as created
#'     using the createConnectionDetails function in the DatabaseConnector
#'     package.
#' 
#' @param cdmDatabaseSchema
#'     Only for omopCDM TRUE: Schema name where your patient-level data
#'     resides. Note that for SQL Server, this should include both the database
#'     and schema name, for example 'cdm_data.dbo'.
#'     
#' @param cohortDatabaseSchema
#'     Only for omopCDM TRUE: Schema name where intermediate data can be
#'     stored. You will need to have write priviliges in this schema. Note that
#'     for SQL Server, this should include both the database and schema name,
#'     for example cdm_results.dbo'.
#'     
#' @param cohortTable
#'     Only for omopCDM TRUE: The name of the table that will be created in
#'     the cohortDatabaseSchema. This table will hold the target and event
#'     cohorts used in this study.
#'     
#' @return
#'     Object dataSettings.
#' @export
createDataSettings <- function(
    omopCDM = TRUE,
    connectionDetails,
    cdmDatabaseSchema,
    resultSchema,
    cohortTable = "treatmentpatterns_cohorts") {
  
  check <- createDataSettingsChecks(
    omopCDM,
    connectionDetails,
    cdmDatabaseSchema,
    resultSchema,
    cohortTable)
  
  if (check) {
    dataSettings <- list(
      omopCDM = omopCDM,
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = resultSchema,
      cohortTable = cohortTable,
      cohortLocation = cohortLocation)
    
    class(dataSettings) <- 'dataSettings'
    
    return(dataSettings)
  }
}


#' cohortsCheck
#' 
#' Checks the validity of targetCohorts and eventCohorts parameters of
#' createCohortSettings
#'
#' @param cohorts eventCohorts or targetCohorts
#'
#' @return TRUE if checkmate checks pass
cohortsCheck <- function(cohorts) {
  # Check validity of data.frame inputs
  checkmate::checkSubset(
    x = names(cohorts),
    choices = c("cohortId", "cohortName"))
  
  checkmate::testDataFrame(
    cohorts,
    any.missing = FALSE,
    types = c("numeric", "character"))
  
  return(TRUE)
}

#' Create cohort settings.
#'
#' Create a cohortsSettings object, containing information about the target and event cohorts.
#' A cohort ID and name are required to specify the target and event cohorts.
#' The cohortId and cohortName are the ID and Name specified while creating cohorts with i.e. CohortGenerator.
#'
#' @param targetCohorts
#'     Data frame containing the study population of interest
#'     cohortId = "Unique ID number", cohortName = "Descriptive name cohort".
#'     
#' @param eventCohorts
#'     Data frame containing the events of interest
#'     cohortId = "Unique ID number", cohortName = "Descriptive name cohort".
#'
#' @return
#'     Object cohortSettings.
#'     
#' @export
#' 
#' @example
#' cohortSettings <- createCohortSettings(
#'   targetCohorts = data.frame(
#'     cohortId = c(1),
#'     cohortName = c("a")),
#'   eventCohorts = data.frame(
#'     cohortId = c(2, 3),
#'     cohortName = c("b", "c")))
createCohortSettings <- function(targetCohorts, eventCohorts) {
  # Create cohortsToCreate from targetCohorts and eventCohorts
  if (cohortsCheck(targetCohorts) && cohortsCheck(eventCohorts)) {
    targetCohorts$cohortType <- 'target'
    eventCohorts$cohortType <- 'event'
    cohortsToCreate <- rbind(targetCohorts, eventCohorts)
  }
  
  # Why numeric to int?
  cohortsToCreate$cohortId <- as.integer(cohortsToCreate$cohortId)
  
  cohortSettings <- list(
    cohortsToCreate = cohortsToCreate)
  class(cohortSettings) <- 'cohortSettings'
  
  return(cohortSettings)
}

createPathwaySettingsCheck <- function(
    cohortSettings,
    pathwaySettingsLocation,
    pathwaySettingsList) {
  
  # Check cohortSettings
  checkmate::checkClass(
    x = cohortSettings,
    classes = "cohortSettings")
  
  checkmate::checkDataFrame(
    x = cohortSettings$cohortsToCreate,
    types = c("integer", "character", "character"),
    ncols = 3,
    any.missing = FALSE)
  
  checkmate::checkSubset(
    x = names(c$cohortsToCreate),
    choices = c("cohortId", "cohortName", "cohortType"))
  
  # check pathwaySettingsLocation
  checkmate
  
  return(TRUE)
}

#' Create pathway settings.
#'
#' @param pathwaySettings_location
#'     Optional: Location of saved pathwaySettings object.
#'     
#' @param pathwaySettings_list 
#'     Create (list of pathway settings) with addPathwaySettings()
#'     (e.g.pathwaySettings_list = addPathwaySettings() or
#'     pathwaySettings_list = list(addPathwaySettings(),
#'     addPathwaySettings())).
#'     
#'
#' @return
#'     Object pathwaySettings.
#'     
#' @export
createPathwaySettings <- function(
    cohortSettings,
    pathwaySettingsLocation = NULL,
    pathwaySettingsList = NULL,
    ...) {
  
  # Check
  
  targetCohorts <- cohortSettings$cohortsToCreate %>%
    filter(cohortType == "target")
  
  eventCohorts <- cohortSettings$cohortsToCreate %>%
    filter(cohortType == "event")
  
  # If pathwaySettings_location given, load settings from data
  if (!is.null(pathwaySettings_location)) {
    print("Loading settings from pathwaySettings_location")
    
    pathwaySettings <- data.frame(readr::read_csv(
      file = pathwaySettings_location,
      col_types = readr::cols()))
    
    print(paste0(
      "Loaded ",
      ncol(pathwaySettings) - 1,
      " sets of pathway settings: ",
      paste0(colnames(pathwaySettings), collapse =  ",")))
    
    # TODO: add check if colnames correct (param, analysis 1, 2, etc. )
    
  } else if (!is.null(pathwaySettings_list)) {
    pathwaySettings_all <- do.call("rbind", pathwaySettings_list)
    
    pathwaySettings <- data.table::transpose(pathwaySettings_all)
    colnames(pathwaySettings) <- paste0("analysis", 1:ncol(pathwaySettings))
    
    pathwaySettings <- cbind(
      param = colnames(pathwaySettings_all), 
      pathwaySettings)
    
  } else if (!is.null(targetCohortId) & !is.null(eventCohortIds)) {
    pathwaySettings_default <- addPathwaySettings(
      studyName = c("default"),
      targetCohortId = targetCohortId,
      eventCohortIds = eventCohortIds, 
      ...)
    
    pathwaySettings <- data.table::transpose(pathwaySettings_default)
    colnames(pathwaySettings) <- paste0("analysis", 1:ncol(pathwaySettings))
    pathwaySettings <- cbind(
      param = colnames(pathwaySettings_default), 
      pathwaySettings)
  } else {
    stop(paste(
      "Input missing, insert 1) pathwaySettings_location,",
      "2) pathwaySettings_list, or 3) targetCohortId and eventCohortIds"))
  }
  
  pathwaySettings <- list(all_settings = pathwaySettings)
  class(pathwaySettings) <- 'pathwaySettings'
  
  return(pathwaySettings)
}


#' Add set of pathway settings.
#'
#' addPathwaySettings defines and returns a data.frame of settings specified in the function call.
#'
#' @param studyName
#'     Name identifying the set of study parameters.
#'     
#' @param targetCohortId
#'     Target cohort ID of current study settings.
#'     
#' @param eventCohortIds
#'     Event cohort IDs of current study settings.
#'     
#' @param includeTreatments
#'     Include treatments starting ('startDate') or ending ('endDate') after
#'     target cohort start date.
#'     
#' @param periodPriorToIndex
#'     Number of days prior to the index date of the target cohort that event
#'     cohorts are allowed to start.
#'     
#' @param minEraDuration
#'     Minimum time an event era should last to be included in analysis.
#'     
#' @param splitEventCohorts
#'     Specify event cohort to split in acute (< X days) and therapy 
#'     (>= X days).
#'     
#' @param splitTime
#'     Specify number of days (X) at which each of the split event cohorts
#'     should be split in acute and therapy.
#'     
#' @param eraCollapseSize
#'     Window of time between which two eras of the same event cohort are
#'     collapsed into one era.
#'     
#' @param combinationWindow
#'     Window of time two event cohorts need to overlap to be considered a
#'     combination treatment.
#'     
#' @param minPostCombinationDuration
#'     Minimum time an event era before or after a generated combination
#'     treatment should last to be included in analysis.
#'     
#' @param filterTreatments
#'     Select first occurrence of ("First") / changes between ("Changes') / all
#'     event cohorts ("All").
#'     
#' @param maxPathLength
#'     Maximum number of steps included in treatment pathway (max 5).
#'     
#' @param minCellCount
#'     Minimum number of persons with a specific treatment pathway for the
#'     pathway to be included in analysis.
#'     
#' @param minCellMethod
#'     Select to completely remove / sequentially adjust (by removing last step
#'     as often as necessary) treatment pathways below minCellCount.
#'     
#' @param groupCombinations
#'     Select to group all non-fixed combinations in one category 'other’ in
#'     the sunburst plot.
#'     
#' @param addNoPaths
#'     Select to include untreated persons without treatment pathway in the
#'     sunburst plot
#'
#' @export
addPathwaySettings <- function(
    studyName = "name_unknown", # c("default")
    targetCohortId,
    eventCohortIds,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30, 
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5, 
    minCellCount = 5,
    minCellMethod = "Remove",
    groupCombinations = 10,
    addNoPaths = FALSE) {
  
  if (!length(targetCohortId)>0 | !is.numeric(targetCohortId)) {
    stop("targetCohortId should be numeric value")
  }
  # TODO: check if analysis also works with multiple targetCohortIds at once
  
  if (!length(eventCohortIds)>0 | !is.numeric(eventCohortIds)) {
    stop("eventCohortIds should be numeric values")
  }
  
  if (maxPathLength > 5) {
    stop("MaxPathLength > 5 is currently not supported")
  }
  
  settings <- data.frame(
    studyName = studyName,
    targetCohortId = targetCohortId,
    eventCohortIds = paste(eventCohortIds, collapse = ","),
    includeTreatments = includeTreatments,
    periodPriorToIndex = periodPriorToIndex,
    minEraDuration = minEraDuration,
    splitEventCohorts = splitEventCohorts,
    splitTime = splitTime,
    eraCollapseSize = eraCollapseSize,
    combinationWindow = combinationWindow,
    minPostCombinationDuration = minPostCombinationDuration,
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
#' @param databaseName
#'     Name of the database that will appear in the results.
#'     
#' @param rootFolder
#'     Name of local folder to place all package output (outputFolder,
#'     tempFolder if not given).
#'     
#' @param outputFolder
#'     Name of local folder to place results; make sure to use forward slashes
#'     (/).
#'     
#' @param tempFolder
#'     Name of local folder to place intermediate results (not to be shared);
#'     make sure to use forward slashes (/).
#'
#' @return
#'     Object saveSettings.
#'     
#' @export
createSaveSettings <- function(
    databaseName = "unknown_name",
    rootFolder,
    outputFolder = file.path(rootFolder, "output"),
    tempFolder = file.path(rootFolder, "temp")) {
  
  outputFolder <- file.path(outputFolder, databaseName)
  
  # Change relative path to absolute path
  rootFolder <- stringr::str_replace(
    string = rootFolder,
    pattern = "^[.]", replacement = getwd())
  
  outputFolder <- stringr::str_replace(
    string = outputFolder,
    pattern = "^[.]", replacement = getwd())
  
  tempFolder <- stringr::str_replace(
    string = tempFolder,
    pattern = "^[.]",
    replacement = getwd())
  
  saveSettings <- list(
    databaseName = databaseName,
    rootFolder = rootFolder,
    outputFolder = outputFolder,
    tempFolder = tempFolder)
  
  class(saveSettings) <- 'saveSettings'
  
  return(saveSettings)
}
