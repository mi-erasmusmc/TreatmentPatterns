#' executeTreatmentPatterns
#'
#' This is the main function which runs all parts of the treatment pathways
#' analysis. The following tasks are performed sequentially: 1) Construct
#' treatment pathways, 2) Generate output (sunburst plots, Sankey diagrams and
#' more), 3) Launch shiny application to visualize the results. 
#'
#' @param dataSettings dataSettings object
#' @param pathwaySettings pathwaySettings object
#' @param saveSettings saveSettings object
#'
#' @return NULL
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Select Viral Sinusitis Cohort
#'   targetCohort <- cohortsGenerated %>% 
#'     filter(cohortName == "Viral Sinusitis") %>%
#'     select(cohortId, cohortName)
#'
#'   # Select everything BUT Viral Sinusitis cohorts
#'   eventCohorts <- cohortsGenerated %>% 
#'     filter(cohortName != "Viral Sinusitis") %>%
#'     select(cohortId, cohortName)
#' 
#'   saveSettings <- TreatmentPatterns::createSaveSettings(
#'     databaseName = "Eunomia",
#'     rootFolder = getwd(),
#'     outputFolder = file.path(getwd(), "output", "Eunomia"))
#'   
#'   cohortSettings <- TreatmentPatterns::createCohortSettings(
#'     targetCohorts = targetCohort,
#'      eventCohorts = eventCohorts)
#' 
#'   pathwaySettings <- createPathwaySettings(
#'     cohortSettings = cohortSettings,
#'     studyName = "Viral_Sinusitis")
#'   
#'   executeTreatmentPatterns(
#'   dataSettings = dataSettings,
#'   pathwaySettings = pathwaySettings,
#'   saveSettings = saveSettings)
#' }
executeTreatmentPatterns <- function(
    dataSettings,
    pathwaySettings,
    saveSettings) {
  
  # 3) Construct treatment pathways
  TreatmentPatterns::constructPathways(
    dataSettings = dataSettings,
    pathwaySettings = pathwaySettings,
    saveSettings = saveSettings)
  
  # 4) Generate output (sunburst plots, Sankey diagrams and more)
  TreatmentPatterns::generateOutput(saveSettings)
  
  # 5) Launch shiny application to visualize the results
  TreatmentPatterns::launchResultsExplorer(saveSettings)
}
