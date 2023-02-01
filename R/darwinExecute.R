#' darwinExecute
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
