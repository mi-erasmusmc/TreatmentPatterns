#' launchResultsExplorer
#' 
#' Processes the zipped result folder(s) and launch the shiny application.
#' 
#' @param saveSettings
#'     Settings object as created by createSaveSettings().
#' @param zipFolder
#'     Name of local folder containing zip file(s) to move to outputFolder.
#' @param outputFolder
#'     Name of local folder containing output.
#'
#' @importFrom shiny runApp
#' @importFrom utils head menu read.csv write.csv
#' @importFrom rlang :=
#'
#' @export
launchResultsExplorer <- function(
    saveSettings = NULL,
    zipFolder = NULL,
    outputFolder = NULL) {
  ensure_installed("shiny")
  ensure_installed("shinydashboard")
  ensure_installed("reshape2")
  ensure_installed("ggplot2")
  ensure_installed("data.table")
  ensure_installed("DT")
  
  # Check if inputs correct
  if (!is.null(saveSettings)) {
    if (!class(saveSettings) %in% c('saveSettings')) {
      stop('Incorrect class for saveSettings')
    } else {
      outputFolder <- stringr::str_remove(
        saveSettings$outputFolder,
        pattern = paste0("/", saveSettings$databaseName))
    }
  } else if (!is.null(zipFolder)) {
    if (length(list.dirs(
      file.path(zipFolder, "output"),
      recursive = FALSE,
      full.names = FALSE
    )) == 0) {
      print("Unzip files in zipFolder and move to /output")
      unzipFiles(zipFolder, unzipMainFolder = file.path(zipFolder, "output"))
      outputFolder <- file.path(zipFolder, "output")
    } else if (dir.exists(file.path(zipFolder, "output"))) {
      print("Files already unzipped, use existing results in /output")
      outputFolder <- file.path(zipFolder, "output")
    }
  }
  else if (is.null(outputFolder) &
           is.null(zipFolder) & is.null(saveSettings)) {
    stop('Input outputFolder, zipFolder or saveSettings')
  }
  
  outputFolder <- stringr::str_replace(
    outputFolder,
    pattern = "^[.]",
    replacement = getwd())
  
  shinySettings <- list(outputFolder = outputFolder)
  
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm("shinySettings", envir = .GlobalEnv))
  
  appDir <-
    file.path(system.file(package = "TreatmentPatterns"), "shiny")
  shiny::runApp(appDir = appDir)
  }


#' unzipFiles
#'
#' Borrowed and adapted function from CohortDiagnostics.
#'
#' @param rootFolder
#'     Root folder
#' @param unzipMainFolder
#'     Unzip main folder
#' 
#' @importFrom dplyr tibble
#' @import zip
#' 
#' @return NULL
unzipFiles <- function(rootFolder, unzipMainFolder) {
  zipFiles <- dplyr::tibble(
    zipFile = list.files(
      rootFolder,
      pattern = ".zip",
      full.names = TRUE,
      recursive = TRUE),
    unzipFolder = "")
  ParallelLogger::logInfo("Merging ", nrow(zipFiles), " zip files.")
  
  if (!file.exists(unzipMainFolder)) {
    dir.create(path = unzipMainFolder, recursive = TRUE)
  }

  for (i in 1:nrow(zipFiles)) {
    ParallelLogger::logInfo("- Unzipping ", basename(zipFiles$zipFile[i]))
    unzipFolder <- file.path(
      unzipMainFolder,
      sub(".zip", "", basename(zipFiles$zipFile[i])))
    if (!file.exists(unzipFolder)) {
      dir.create(unzipFolder)
    }
    zip::unzip(zipFiles$zipFile[i], exdir = unzipFolder)
    zipFiles$unzipFolder[i] <- unzipFolder
  }
}