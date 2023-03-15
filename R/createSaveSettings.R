#' checkSaveSettings
#' 
#' Checks parameters for createSaveSettings.
#'
#' @param databaseName
#'   Name of the database that will appear in the results.
#'
#' @param rootFolder
#'   Name of local folder to place all package output (outputFolder,
#'     tempFolder if not given).
#'
#' @param outputFolder
#'   Name of local folder to place package output.
#'
#' @param tempFolder
#'   Name of local temporal folder.
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

  if (!is.null(outputFolder)) {
    # outputFolder
    checkmate::assert(checkmate::check_character(
      outputFolder))
  }

  if (!is.null(tempFolder)) {
    # tempFolder
    checkmate::assert(checkmate::checkPathForOutput(
      tempFolder, overwrite = TRUE))
  }
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
#'     Name of local folder to place package output.
#' @param tempFolder
#'     Temp folder.
#'
#' @return
#'     Object saveSettings.
#'
#' @export
#'
#' @examples
#' tmpOutputFolder <- tempdir()
#' 
#' createSaveSettings(
#'    rootFolder = "./",
#'    databaseName = "Eunomia",
#'    outputFolder = tmpOutputFolder)
createSaveSettings <- function(
    databaseName = "unknown_name",
    rootFolder,
    outputFolder,
    tempFolder = NULL) {

  tempFolder <- tempdir()

  check <- checkSaveSettings(
    databaseName,
    rootFolder,
    outputFolder,
    tempFolder)

  if (check) {
    saveSettings <- list(
      databaseName = databaseName,
      rootFolder = normalizePath(rootFolder),
      outputFolder = normalizePath(outputFolder),
      tempFolder = normalizePath(tempFolder))

    class(saveSettings) <- "saveSettings"

    return(saveSettings)
  }
}
