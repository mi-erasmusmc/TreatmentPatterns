#' checkPathwaySettings
#' 
#' Check whether an object has the correct CohortSettings and dataframe class. The function checks it has the specified amount of columns, and if they are integer and character. Also checks if the dataframe columns have the correct name. 
#'
#' @param cohortSettings cohortSettings object
#'
#' @return TRUE if all assertions pass
#'  
#'
#' @examples \dontrun{
#'   checkPathwaySettings(
#'     cohortSettings = cohortSettings
#'     )}
checkPathwaySettings <- function(
    cohortSettings) {
  # Check cohortSettings
  checkmate::assert(
    checkmate::checkClass(
      x = cohortSettings,
      classes = "cohortSettings"),
    checkmate::checkDataFrame(
      x = cohortSettings$cohortsToCreate,
      types = c("integer",
                "character",
                "character"),
      ncols = 3,
      any.missing = FALSE),
    checkmate::checkSubset(
      x = names(cohortSettings$cohortsToCreate),
      choices = c("cohortId",
                  "cohortName",
                  "cohortType")),
    combine = "and"
  )
  return(TRUE)
}

#' createPathwaySettings
#'
#' Create pathway settings.
#'
#' @param cohortSettings cohortSettings object
#' @param ...
#'   Any addPathwaySettings parameter:
#'   1. studyName
#'   2. includeTreatments
#'   3. periodPriorToIndex
#'   4. minEraDuration
#'   5. splitEventCohorts
#'   6. splitTime
#'   7. eraCollapseSize
#'   8. combinationWindow
#'   9. minPostCombinationDuration
#'   10. filterTreatments
#'   11. maxPathLength
#'   12. minCellCount
#'   13. minCellMethod
#'   14. groupCombinations
#'   15. addNoPaths
#'
#' @importFrom data.table transpose
#' @importFrom dplyr filter
#' @importFrom utils globalVariables
#' 
#' @return Object pathwaySettings.
#'     
#' @export
#' @examples
#' targetCohorts <- data.frame(
#'   cohortName = c("targetCohort"),
#'   cohortId = c(1))
#'
#' eventCohorts <- data.frame(
#'   cohortName = c("eventCohort1", "eventCohort2"),
#'   cohortId = c(2, 3))
#'
#' cohortSettings <- TreatmentPatterns::createCohortSettings(
#'   targetCohorts = targetCohorts,
#'   eventCohorts = eventCohorts)
#'
#' createPathwaySettings(
#'   cohortSettings = cohortSettings,
#'   studyName = "MyStudyName")
createPathwaySettings <- function(cohortSettings, ...) {
  # Check
  check <- checkPathwaySettings(
    cohortSettings)

  if (exists("studyName")) {
    studyName <- studyName
  } else {
    studyName <- "default"
  }

  if (check) {
    targetCohorts <- cohortSettings$cohortsToCreate %>%
      dplyr::filter(cohortType == "target")

    eventCohorts <- cohortSettings$cohortsToCreate %>%
      dplyr::filter(cohortType == "event")

    # Create default pathwaySettings template
    pathwaySettingsDefault <- addPathwaySettings(
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
    class(pathwaySettings) <- "pathwaySettings"

    return(pathwaySettings)
  }
}
utils::globalVariables("cohortType")
