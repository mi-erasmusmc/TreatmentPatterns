#' cohortsCheck
#' 
#' Checks the validity of targetCohorts and eventCohorts parameters of
#' createCohortSettings
#'
#' @param cohorts eventCohorts or targetCohorts
#'
#' @import checkmate
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
#' @examples
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