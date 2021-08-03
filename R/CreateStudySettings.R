
#' Transforms and saves input arguments given by user in instFolder/settings.
#'
#' @param instFolder Name of local folder to place all settings and cohorts; make sure to use forward slashes (/).   
#' @param targetCohorts Data frame containing the study population of interest (cohortId, atlasId, cohortName).
#' @param eventCohorts Data frame containing the events of interest (cohortId, atlasId, cohortName).
#' @param characterizationSettings Data frame containing the baseline characteristics of interest (covariateName, covariateId).
#' @param pathwaySettings Data frame containing all pathway settings.
#'
#' @export
createStudySettings <- function(instFolder,
                                targetCohorts = NULL,
                                eventCohorts = NULL,
                                characterizationSettings = NULL,
                                pathwaySettings = NULL) {
  # Check if directory exists
  if (!file.exists(paste0(instFolder, "/settings"))) { 
    dir.create(paste0(instFolder, "/settings"), recursive = TRUE)
  } 
  
  # If input arguments given, use these to create study settings
  # Create file settings/cohorts_to_create
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

  # Create file settings/characterization_settings
  if (!is.null(characterizationSettings)) {
    ParallelLogger::logInfo(print("Create characterization_settings.csv from arguments"))
    write.csv(characterizationSettings, paste0(instFolder, "/settings/characterization_settings.csv"), row.names = FALSE)
  }
  
  # Create file settings/pathway_settings
  if (!is.null(pathwaySettings)) {
    ParallelLogger::logInfo(print("Create study_settings.csv from arguments"))
    pathway_settings <- transpose(pathwaySettings)
    colnames(pathway_settings) <- paste0("analysis", 1:ncol(pathway_settings))
    pathway_settings <- cbind(param = colnames(pathwaySettings), pathway_settings)
    write.csv(pathway_settings, paste0(instFolder, "/settings/pathway_settings.csv"), row.names = FALSE)
  }
  
  ParallelLogger::logInfo("createStudySettings done.")
}
