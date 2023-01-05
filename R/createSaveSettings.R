#' checkSaveSettings
#'
#' Checks parameters of createSaveSettings.
#'
#' @param databaseName 
#' @param rootFolder 
#' @param outputFolder 
#' @param tempFolder 
#' 
#' @import checkmate
#'
#' @return TRUE if all assertions pass
checkSaveSettings <- function(
    databaseName,
    rootFolder,
    outputFolder,
    tempFolder) {

  # databaseName
  checkmate::assert(checkmate::checkCharacter(
    x = databaseName,
    len = 1,
    any.missing = FALSE))
  
  # rootFolder
  checkmate::assert(checkmate::checkDirectory(
    x = rootFolder,
    access = "wx"))
  
  # outputFolder
  checkmate::assert(checkmate::checkPathForOutput(
    x = outputFolder))
  
  # tempFolder
  checkmate::assert(checkmate::checkPathForOutput(
    x = tempFolder))
  
  return(TRUE)
}

#' createSaveSettings
#' 
#' Creates saveSettings object.
#'
#' @param databaseName
#'     Name of the database that will appear in the results.
#'     
#' @param rootFolder
#'     Name of local folder to place all package output (outputFolder,
#'     tempFolder if not given).
#'     
#' @param outputFolder
#'     Name of local folder to place results; make sure to use forward slashes
#'     (/).
#'     
#' @param tempFolder
#'     Name of local folder to place intermediate results (not to be shared);
#'     make sure to use forward slashes (/).
#'
#' @return
#'     Object saveSettings.
#'     
#' @export
createSaveSettings <- function(
    databaseName = "unknown_name",
    rootFolder,
    outputFolder = file.path(rootFolder, "output"),
    tempFolder = file.path(rootFolder, "temp")) {
  
  check <- checkSaveSettings(
    databaseName,
    rootFolder,
    outputFolder,
    tempFolder)
  
  rootFolder <- getwd()
  
  if (check) {
    outputFolder <- file.path(outputFolder, databaseName)
    
    # Change relative path to absolute path
    rootFolder <- normalizePath(rootFolder)
    
    # Suppress Warnings, as nothing will be written yet.
    outputFolder <- suppressWarnings(normalizePath(outputFolder))
    tempFolder <- suppressWarnings(normalizePath(tempFolder))
    
    saveSettings <- list(
      databaseName = databaseName,
      rootFolder = rootFolder,
      outputFolder = outputFolder,
      tempFolder = tempFolder)
    
    class(saveSettings) <- 'saveSettings'
    
    return(saveSettings)
  }
}