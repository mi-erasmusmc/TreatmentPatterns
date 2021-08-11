
#' Processes the zipped result folder(s) and launch the shiny application.
#' 
#' @param saveSettings Settings object as created by createSaveSettings().
#' @param zipFolder Name of local folder containing zip file(s).
#'
#' @return
#' @export
launchResultsExplorer <- function(saveSettings = NULL, zipFolder) {
  # ensure_installed("shiny")
  # ensure_installed("shinydashboard")
  # ensure_installed("shinymanager")
  # ensure_installed("reshape2")
  # ensure_installed("ggplot2")
  # ensure_installed("data.table")
  # ensure_installed("DT")
  # ensure_installed("R.utils")
  
  # TODO: test if this works
  if (!is.null(saveSettings)) {
    zipFolder <- saveSettings$rootFolder
  }
  
  appDir <- file.path(system.file(package = "TreatmentPatterns"), "shiny")
  shinyFilesLocation <- file.path(zipFolder, "filesShiny")

  unzipFiles(zipFolder, unzipMainFolder = file.path(shinyFilesLocation, "output"))
  addSunburstFiles(filesLocation = shinyFilesLocation)
  
  shinySettings <- list(
    shinyFilesLocation = shinyFilesLocation
  )
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm("shinySettings", envir = .GlobalEnv))
  
  shiny::runApp(appDir = appDir)
}

addSunburstFiles <- function(filesLocation) {
  R.utils::copyDirectory(file.path(from = system.file(package = "TreatmentPatterns"), "shiny", "sunburst"), to = file.path(filesLocation,"sunburst"))
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

