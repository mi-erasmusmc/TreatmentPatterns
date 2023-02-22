#' loadPathwaySettings
#' 
#' Reads in the pathwaySettings from a csv-file.
#'
#' @param filePath Path to csv-file containing the pathwaySettings. 
#'
#' @import checkmate
#' @import dplyr
#'
#' @return pathwaySettings
#' @export
#'
#' @examples
#' loadPathwaySettings(file.path(system.file(
#'   package = "TreatmentPatterns"),
#'  "examples", "CDM", "settings", "pathway_settings.csv"))
loadPathwaySettings <- function(filePath) {
  # Read File
  pathwaySettings <- read.csv(file = filePath)
  
  # Check Contents
  checkmate::assert(
    checkmate::checkDataFrame(pathwaySettings, nrows = 15))
  
  # Check if all analysis column names adhere to pattern
  analysisCheck <- all(grepl(
    # (\\d)? optional digits
    pattern = "analysis(\\d)?",
    # Remove first column param of names with [-1]
    x = names(pathwaySettings)[-1]), TRUE)
  
  if (!analysisCheck) {
    stop("analysis column names do not adhere to 'analysis1', 'analysis12', 'analysis123', ... pattern")
  } else {
    pathwaySettings <- list(all_settings = pathwaySettings)
    class(pathwaySettings) <- 'pathwaySettings'
    
    return(pathwaySettings)
  }
}