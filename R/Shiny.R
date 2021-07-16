
#' This function processes the zipped result folder(s) and launches the shiny application.
#'
#' @param rootFolder 
#'
#' @return
#' @export
launchShinyApplication <- function(rootFolder) {
  # ensure_installed("shiny")
  # ensure_installed("shinydashboard")
  # ensure_installed("shinymanager")
  # ensure_installed("reshape2")
  # ensure_installed("ggplot2")
  # ensure_installed("data.table")
  # ensure_installed("DT")
  # ensure_installed("R.utils")
  
  appDir <- paste0(system.file(package = "TreatmentPatterns"), "/shiny")
  shinyFilesLocation <- paste0(rootFolder, "/filesShiny")

  unzipFiles(rootFolder, unzipMainFolder = paste0(shinyFilesLocation, "/output"))
  addSunburstFiles(filesLocation = shinyFilesLocation)
  
  shinySettings <- list(
    shinyFilesLocation = shinyFilesLocation
  )
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm("shinySettings", envir = .GlobalEnv))
  
  shiny::runApp(appDir = appDir)
}

addSunburstFiles <- function(filesLocation) {
  R.utils::copyDirectory(paste0(from = system.file(package = "TreatmentPatterns"), "/shiny/sunburst"), to = paste0(filesLocation,"/sunburst"))
}

# Borrowed and adapted function from CohortDiagnostics.
unzipFiles <- function(rootFolder, unzipMainFolder) {
    zipFiles <-
      dplyr::tibble(
        zipFile = list.files(
          rootFolder,
          pattern = ".zip",
          full.names = TRUE,
          recursive = TRUE
        ),
        unzipFolder = ""
      )
    ParallelLogger::logInfo("Merging ", nrow(zipFiles), " zip files.")
    
    if (!file.exists(unzipMainFolder))
      dir.create(path = unzipMainFolder, recursive = TRUE)
    
    for (i in 1:nrow(zipFiles)) {
      ParallelLogger::logInfo("- Unzipping ", basename(zipFiles$zipFile[i]))
      unzipFolder <-
        file.path(unzipMainFolder, sub(".zip", "", basename(zipFiles$zipFile[i])))
      if (!file.exists(unzipFolder))
        dir.create(unzipFolder)
      zip::unzip(zipFiles$zipFile[i], exdir = unzipFolder)
      zipFiles$unzipFolder[i] <- unzipFolder
    }
}

