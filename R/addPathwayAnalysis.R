#' addPathwayAnalysis
#'
#' @param pathwaySettings pathwaySettings object
#' @param eventCohortIds Event Cohort IDs
#' @param targetCohortIds Target Cohort IDs
#' @param ... Optional addPathwaySettings parameters. i.e. studyName.
#'
#' @import dplyr
#'
#' @return pathwaySettings
#' @export
#'
#' @examples
#' # Create cohortSettings
#' cohortSettings <- createCohortSettings(
#'   targetCohorts = data.frame(
#'     cohortId = c(1),
#'     cohortName = c("a")),
#'   eventCohorts = data.frame(
#'     cohortId = c(2, 3),
#'     cohortName = c("b", "c")))
#' 
#' # Create pathwaySettings
#' pathwaySettings <- createPathwaySettings(cohortSettings)
#' 
#' addPathwayAnalysis(
#'   pathwaySettings = pathwaySettings,
#'   targetCohortIds = c(10),
#'   eventCohortIds = c(11, 12),
#'   studyName = "Second study")
addPathwayAnalysis <- function(
    pathwaySettings, targetCohortIds, eventCohortIds, ...) {
  analysis <- addPathwaySettings(
    eventCohortIds = eventCohortIds,
    targetCohortId = targetCohortIds,
    ...)
  
  analysisParams <- data.table::transpose(analysis)
  
  names(analysisParams) <- paste0(
    "analysis", length(names(pathwaySettings$all_settings)))
  
  pathwaySettings$all_settings <- dplyr::bind_cols(
    pathwaySettings$all_settings,
    analysisParams)
  return(pathwaySettings)
}