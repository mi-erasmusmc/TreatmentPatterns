createDataSettingsChecks <- function(
    omopCDM,
    connectionDetails,
    cdmDatabaseSchema,
    resultSchema,
    cohortTable) {
  # Check omopCDM\
  checkmate::assert(
    checkmate::checkLogical(
      x = omopCDM,
      len = 1,
      null.ok = FALSE)
  )
  
  # Check connectionDetails
  checkmate::assert(
    checkmate::checkClass(
      connectionDetails,
      "connectionDetails"),
    checkmate::checkCharacter(
      x = connectionDetails$dbms,
      len = 1,
      null.ok = FALSE),
    combine = "and"
  )
  
  # check cdmDatabaseSchema
  checkmate::assert(
    checkmate::checkCharacter(
      cdmDatabaseSchema,
      null.ok = FALSE,
      len = 1)
  )
  
  # check resultSchema
  checkmate::assert(
    checkmate::checkCharacter(
      resultSchema,
      null.ok = FALSE,
      len = 1)
  )
  
  # cohortTable
  checkmate::assert(
    checkmate::checkCharacter(
      cohortTable,
      null.ok = FALSE,
      len = 1)
  )
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
      cohortTable = cohortTable)
    
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
  checkmate::assert(
    checkmate::checkSubset(
      x = names(cohorts),
      choices = c("cohortId", "cohortName")),
    checkmate::checkDataFrame(
      cohorts,
      any.missing = FALSE,
      types = c("numeric", "character")),
    combine = "and"
  )
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
    cohortSettings) {
  # Check cohortSettings
  checkmate::assert(
    checkmate::checkClass(
      x = cohortSettings,
      classes = "cohortSettings"),
    checkmate::checkDataFrame(
      x = cohortSettings$cohortsToCreate,
      types = c("integer", "character", "character"),
      ncols = 3,
      any.missing = FALSE),
    checkmate::checkSubset(
      x = names(cohortSettings$cohortsToCreate),
      choices = c("cohortId", "cohortName", "cohortType")),
    combine = "and"
  )
  return(TRUE)
}

#' readInPathwaySettings
#' 
#' Reads in the pathwaySettings from a csv-file.
#'
#' @param filePath Path to csv-file containing the pathwaySettings. 
#'
#' @return
#' @export
#'
#' @examples
#' readInPathwaySettings(file.path(system.file(
#'   package = "TreatmentPatterns"),
#'  "examples", "OMOP CDM", "inst", "settings", "pathway_settings.csv"))
readInPathwaySettings <- function(filePath) {
  # Read File
  pathwaySettings <- read.csv(file = filePath)
  
  # Check Contents
  checkmate::assert(
    checkmate::checkDataFrame(pathwaySettings, nrows = 15))
  
  # Check if all analysis column names adhere to pattern
  analysisCheck <- all(grepl(
    # (\\d)? optional digits
    pattern = "analysis(\\d)?",
    # Remove first column param of names with [-1]
    x = names(pathwaySettings)[-1]), TRUE)
  
  if (!analysisCheck) {
    stop("analysis column names do not adhere to 'analysis1', 'analysis12', 'analysis123', ... pattern")
  } else {
    pathwaySettings <- list(all_settings = pathwaySettings)
    class(pathwaySettings) <- 'pathwaySettings'
    
    return(pathwaySettings)
  }
}

#' createPathwaySettings
#' 
#' Create pathway settings.
#'
#' @param cohortSettings cohortSettings object
#' @param ...
#'   Any addPathwaySettings parameter:
#'   1. studyName
#'   2. targetCohortId
#'   3. eventCohortIds
#'   4. includeTreatments
#'   5. periodPriorToIndex
#'   6. minEraDuration
#'   7. splitEventCohorts
#'   8. splitTime
#'   9. eraCollapseSize
#'   10. combinationWindow
#'   11. minPostCombinationDuration
#'   12. filterTreatments
#'   13. maxPathLength
#'   14. minCellCount
#'   15. minCellMethod
#'   16. groupCombinations
#'   17. addNoPaths
#'
#' @return
#'     Object pathwaySettings.
#'     
#' @export
#' @example 
#' createPathwaySettings(
#'   cohortSettings = cohortSettings,
#'   studyName = "MyStudyName")
createPathwaySettings <- function(cohortSettings, ...) {
  # Check
  check <- createPathwaySettingsCheck(
    cohortSettings)
  
  if (check) {
    targetCohorts <- cohortSettings$cohortsToCreate %>%
      dplyr::filter(cohortType == "target")
    
    eventCohorts <- cohortSettings$cohortsToCreate %>%
      dplyr::filter(cohortType == "event")
      
    # Create default pathwaySettings template
    pathwaySettingsDefault <- addPathwaySettings(
      studyName = c("default"),
      targetCohortId = targetCohorts$cohortId,
      eventCohortIds = eventCohorts$cohortId,
      ...)
    
    # Transpose
    pathwaySettings <- data.table::transpose(pathwaySettingsDefault)
    
    # Add colnames analysis1, analysis2, ...
    colnames(pathwaySettings) <- paste0(
      "analysis", seq_len(ncol(pathwaySettings)))
    
    # Add param names to pathwaySettings 
    pathwaySettings <- cbind(
      param = colnames(pathwaySettingsDefault), 
      pathwaySettings)
    
    pathwaySettings <- list(all_settings = pathwaySettings)
    class(pathwaySettings) <- 'pathwaySettings'
    
    return(pathwaySettings)
  }
}

addPathwaySettingsCheck <- function(
    studyName,
    targetCohortId,
    eventCohortIds,
    includeTreatments,
    periodPriorToIndex,
    minEraDuration,
    splitEventCohorts,
    splitTime,
    eraCollapseSize,
    combinationWindow, 
    minPostCombinationDuration,
    filterTreatments,
    maxPathLength, 
    minCellCount,
    minCellMethod,
    groupCombinations,
    addNoPaths) {
  
  # studyName
  checkmate::assert(
    checkmate::checkCharacter(
      x = studyName,
      len = 1))
  
  # targetCohortId
  checkmate::assert(checkmate::checkNumeric(
    x = targetCohortId,
    min.len = 1,
    unique = TRUE,
    null.ok = FALSE))
  
  # eventCohortIds
  checkmate::assert(checkmate::checkNumeric(
    x = eventCohortIds,
    min.len = 1,
    unique = TRUE,
    null.ok = FALSE))
  
  # includeTreatments
  checkmate::assert(
    checkmate::checkCharacter(
      x = includeTreatments,
      len = 1),
    checkmate::checkSubset(
      x = includeTreatments,
      choices = c("startDate", "endDate")),
    combine = "and")
  
  # periodPriorToIndex
  checkmate::assert(checkmate::checkNumeric(
    x = periodPriorToIndex,
    # lower = 0, # Can it be negative?
    len = 1,
    finite = TRUE,
    null.ok = FALSE))
  
  # minEraDuration
  checkmate::assert(checkmate::checkNumeric(
    x = minEraDuration,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE))
  
  # splitEventCohorts
  checkmate::assert(checkmate::checkCharacter(
    x = splitEventCohorts,
    len = 1))
  
  # splitTime
  checkmate::assert(checkmate::checkNumeric(
    x = splitTime,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE))
  
  # eraCollapseSize
  checkmate::assert(checkmate::checkNumeric(
    x = eraCollapseSize,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE))
  
  # combinationWindow
  checkmate::assert(checkmate::checkNumeric(
    x = combinationWindow,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE))
  
  # minPostCombinationDuration
  checkmate::assert(checkmate::checkNumeric(
    x = minPostCombinationDuration,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE))
  
  # filterTreatments
  checkmate::assert(
    checkmate::checkCharacter(
      x = filterTreatments,
      len = 1),
    checkmate::checkSubset(
      x = filterTreatments,
      choices = c("First", "Changes", "All")),
    combine = "and")
  
  # maxPathLength
  checkmate::assert(checkmate::checkNumeric(
    x = maxPathLength,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE))
  
  # minCellCount
  checkmate::assert(checkmate::checkNumeric(
    x = minCellCount,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE))
  
  # minCellMethod
  # Not used in ConstructPathways.R
  checkmate::assert(
    checkmate::checkCharacter(
      x = filterTreatments,
      len = 1))
  
  # groupCombinations
  checkmate::assert(checkmate::checkNumeric(
    x = groupCombinations,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE))
  
  # addNoPaths
  checkmate::assert(checkmate::checkLogical(
    x = addNoPaths,
    any.missing = FALSE,
    len = 1))
  
  return(TRUE)
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
#'     Specify event cohort ID's to split in acute (< X days) and therapy 
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
  
  check <- addPathwaySettingsCheck(
    studyName,
    targetCohortId,
    eventCohortIds,
    includeTreatments,
    periodPriorToIndex,
    minEraDuration,
    splitEventCohorts,
    splitTime,
    eraCollapseSize,
    combinationWindow, 
    minPostCombinationDuration,
    filterTreatments,
    maxPathLength, 
    minCellCount,
    minCellMethod,
    groupCombinations,
    addNoPaths)
  
  if (check) {
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
}

checkSaveSettings <- function(
    databaseName,
    rootFolder,
    outputFolder,
    tempFolder) {

  # databaseName
  checkmate::assert(checkmate::checkCharacter(
    x = databaseName,
    len = 1,
    any.missing = FALSE))
  
  # rootFolder
  checkmate::assert(checkmate::checkDirectory(
    x = rootFolder,
    access = "wx"))
  
  # outputFolder
  checkmate::assert(checkmate::checkDirectory(
    x = outputFolder,
    access = "wx"))
  
  # tempFolder
  checkmate::assert(checkmate::checkDirectory(
    x = tempFolder,
    access = "wx"))
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
  
  check <- checkSaveSettings(
    databaseName,
    rootFolder,
    outputFolder,
    tempFolder)
  
  rootFolder <- getwd()
  
  if (check) {
    outputFolder <- file.path(outputFolder, databaseName)
    
    # Change relative path to absolute path
    rootFolder <- normalizePath(rootFolder)
    
    # Suppress Warnings, as nothing will be written yet.
    outputFolder <- suppressWarnings(normalizePath(outputFolder))
    tempFolder <- suppressWarnings(normalizePath(tempFolder))
    
    saveSettings <- list(
      databaseName = databaseName,
      rootFolder = rootFolder,
      outputFolder = outputFolder,
      tempFolder = tempFolder)
    
    class(saveSettings) <- 'saveSettings'
    
    return(saveSettings)
  }
}
