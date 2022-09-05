
#' This is the main function which runs all parts of the treatment pathways analysis. The following tasks are performed sequentially:
#' 1) Create target/event cohorts of interest,
#' 2) Optional, only for OMOP-CDM data: Perform baseline characterization of study/target population,
#' 3) Construct treatment pathways,
#' 4) Generate output (sunburst plots, Sankey diagrams and more),
#' 5) Launch shiny application to visualize the results.
#' 
#' @param dataSettings Settings object as created by createDataSettings().
#' @param cohortSettings Settings object as created by createCohortSettings().
#' @param characterizationSettings Optional, only for OMOP-CDM data: Settings object as created by createCharcterizationSettings().
#' @param pathwaySettings Settings object as created by createPathwaySettings().
#' @param saveSettings Settings object as created by createSaveSettings().
#' @param runCreateCohorts     Setting to run 1) createCohorts().
#' @param runCohortCharacterization  Optional, only for OMOP-CDM data: Setting to run 2) cohortCharacterization().  
#' @param runConstructPathways Setting to run 3) constructPathways().
#' @param runGenerateOutput        Setting to run 4) generateOutput().
#' @param launchShiny           Setting to run 5) launchResultsExplorer().
#'
#' @import data.table
#' @import magrittr
#' @export
executeTreatmentPatterns <- function(dataSettings,
                                     cohortSettings,
                                     characterizationSettings = NULL,
                                     pathwaySettings,
                                     saveSettings,
                                     runCreateCohorts = TRUE,
                                     runCohortCharacterization = TRUE, # Optional, only for OMOP-CDM data
                                     runConstructPathways = TRUE,
                                     runGenerateOutput = TRUE,
                                     launchShiny = TRUE)
{
  # Check if directory exists and create if necessary
  if (!dir.exists(saveSettings$outputFolder))
    dir.create(file.path(saveSettings$outputFolder), recursive = TRUE)
  

  # Add logger
  ParallelLogger::clearLoggers()
  ParallelLogger::addDefaultFileLogger(fileName = file.path(saveSettings$outputFolder, "treatmentpatterns_log.txt"),
                                       name = "TreatmentPatterns_Logger")
  ParallelLogger::logInfo(print(paste0("Running package version ", packageVersion("TreatmentPatterns"))))
  # TODO: check connection database!
  
  # 1) Create target/event cohorts of interest
  if (runCreateCohorts) {
    ParallelLogger::logInfo(print("runCreateCohorts TRUE"))
    TreatmentPatterns::createCohorts(dataSettings, cohortSettings, saveSettings)
  }
  
  # 2) Optional, only for OMOP-CDM data: Perform baseline characterization of study/target population
  if (runCohortCharacterization & dataSettings$OMOP_CDM) {
    ParallelLogger::logInfo(print("runCohortCharacterization TRUE"))
    TreatmentPatterns::cohortCharacterization(dataSettings, characterizationSettings, saveSettings)
  }
  
  # 3) Construct treatment pathways
  if (runConstructPathways) {
    ParallelLogger::logInfo(print("runConstructPathways TRUE"))
    TreatmentPatterns::constructPathways(dataSettings, pathwaySettings, saveSettings)
  }
  
  # 4) Generate output (sunburst plots, Sankey diagrams and more)
  if (runGenerateOutput) {
    ParallelLogger::logInfo(print("runGenerateOutput TRUE"))
    TreatmentPatterns::generateOutput(saveSettings)
  }
  
  # 5) Launch shiny application to visualize the results
  if (launchShiny) {
    ParallelLogger::logInfo(print("launchShiny TRUE"))
    TreatmentPatterns::launchResultsExplorer(saveSettings)
  }
  
  ParallelLogger::unregisterLogger("TreatmentPatterns_Logger")
  invisible(NULL)
}