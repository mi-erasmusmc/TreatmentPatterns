
#' Processes the zipped result folder(s) and launch the shiny application.
#' 
#' @param saveSettings Settings object as created by createSaveSettings().
#' @param zipFolder Name of local folder containing zip file(s).
#'
#' @return
#' @export
launchResultsExplorer <- function(saveSettings = NULL, zipFolder = NULL) {
  ensure_installed("shiny")
  ensure_installed("shinydashboard")
  ensure_installed("reshape2")
  ensure_installed("ggplot2")
  ensure_installed("data.table")
  ensure_installed("DT")

  # Check if inputs correct
  if (is.null(zipFolder) & !is.null(saveSettings)) {
    if (!class(saveSettings)%in%c('saveSettings')){
      stop('Incorrect class for saveSettings')
    } else {
      zipFolder <- saveSettings$rootFolder
    }
  } else if (is.null(zipFolder) & is.null(saveSettings)) {
    stop('Input zipFolder or saveSettings')
  }

  if(length(list.dirs(file.path(zipFolder, "output"), recursive = FALSE, full.names = FALSE))==0) {
    print("Unzip files in zipFolder and move to output folder")
    unzipFiles(zipFolder, unzipMainFolder = file.path(zipFolder, "output"))
  } else (
    print("Output folder is not empty, existing files used, please empty to use files from zipFolder")
  )

  shinySettings <- list(zipFolder = zipFolder)
  
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm("shinySettings", envir = .GlobalEnv))
  
  appDir <- file.path(system.file(package = "TreatmentPatterns"), "shiny")
  shiny::runApp(appDir = appDir)
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

