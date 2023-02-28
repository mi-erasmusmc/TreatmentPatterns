#' addPathwayAnalysis
#'
#' Adds a pathway analysis, a combination of target and outcome cohorts, to a pathwaySettings object.
#'
#' @param pathwaySettings A pathwaySettings object
#' @param eventCohortIds Event Cohort ID's
#' @param targetCohortIds Target Cohort ID's
#' @param ... Optional addPathwaySettings parameters i.e. studyName. Excluding
#' eventCohortId and targetCohortIds
#'
#' @importFrom dplyr bind_cols
#' @importFrom data.table transpose
#'
#' @return pathwaySettings
#' @export
#'
#' @examples
#' # Create cohortSettings
#' cohortSettings <- createCohortSettings(
#'   targetCohorts = data.frame(
#'     cohortId = c(1),
#'     cohortName = c("a")
#'   ),
#'   eventCohorts = data.frame(
#'     cohortId = c(2, 3),
#'     cohortName = c("b", "c")
#'   )
#' )
#'
#' # Create pathwaySettings
#' pathwaySettings <- createPathwaySettings(cohortSettings)
#'
#' addPathwayAnalysis(
#'   pathwaySettings = pathwaySettings,
#'   targetCohortIds = c(10),
#'   eventCohortIds = c(11, 12),
#'   studyName = "Second study"
#' )
addPathwayAnalysis <- function(pathwaySettings,
                               targetCohortIds,
                               eventCohortIds, ...) {
  analysis <- addPathwaySettings(
    eventCohortIds = eventCohortIds,
    targetCohortId = targetCohortIds,
    ...
  )

  analysisParams <- data.table::transpose(analysis)

  names(analysisParams) <- paste0(
    "analysis", length(names(pathwaySettings$all_settings))
  )

  pathwaySettings$all_settings <- dplyr::bind_cols(
    pathwaySettings$all_settings,
    analysisParams
  )
  return(pathwaySettings)
}
