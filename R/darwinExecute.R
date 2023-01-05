#' Title
#'
#' @param targetCohort 
#' @param cohortPath 
#' @param dataSettings 
#' @param pathwaySettings 
#' @param characterizatonSettings 
#' @param saveSettings 
#'
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
darwinExecute <- function(
    targetCohort,
    cohortPath,
    dataSettings,
    pathwaySettings,
    characterizatonSettings,
    saveSettings) {
    # Assuming database is in accordance with the OMOP CDM and that there are
    # defined cohorts.
    
    targetCohorts <- cohortTable %>%
      dplyr::filter(.data$cohortName == targetCohort) %>%
      dplyr::select(.data$cohortName, .data$cohortId)
    
    eventCohorts <- cohortTable %>%
      dplyr::filter(.data$cohortName != targetCohort) %>%
      dplyr::select(.data$cohortName, .data$cohortId)
    
    # Create cohortSettings
    cohortSettings <- createCohortSettings(
      targetCohorts = targetCohorts,
      eventCohorts = eventCohorts)

    # Step 3
    TreatmentPatterns::constructPathways(
      dataSettings = dataSettings,
      pathwaySettings = pathwaySettings,
      saveSettings = saveSettings)

    # Step 4
    TreatmentPatterns::generateOutput(
      saveSettings = saveSettings)
    
    # Step 5
    TreatmentPatterns::launchResultsExplorer(saveSettings)
    
  }
