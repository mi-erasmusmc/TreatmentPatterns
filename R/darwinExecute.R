#' darwinExecute
#'
#' @param dataSettings dataSettings object
#' @param pathwaySettings pathwaySettings object
#' @param saveSettings saveSettings object
#'
#' @import dplyr
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
#'   darwinExecute(
#'   dataSettings = dataSettings,
#'   pathwaySettings = pathwaySettings,
#'   saveSettings = saveSettings)
#' }
darwinExecute <- function(
    dataSettings,
    pathwaySettings,
    saveSettings) {
    
  # 3) Construct treatment pathways
  TreatmentPatterns::constructPathways(
    dataSettings = dataSettings,
    pathwaySettings = pathwaySettings,
    saveSettings = saveSettings)
  
  # 4) Generate output (sunburst plots, Sankey diagrams and more)
  ParallelLogger::logInfo(print("=== Generate outputs ===="))
  TreatmentPatterns::generateOutput(saveSettings)
  
  # 5) Launch shiny application to visualize the results
  TreatmentPatterns::launchResultsExplorer(saveSettings)
  }
