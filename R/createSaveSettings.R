#' checkSaveSettings
#' 
#' Checks parameters for createSaveSettings.
#'
#' @param env Environment containging all the function environment variables.
#'
#' @return TRUE if all assertions pass
checkSaveSettings <- function(env) {
  # databaseName
  checkmate::assert(checkmate::checkCharacter(
    x = env$databaseName,
    len = 1,
    any.missing = FALSE))

  # rootFolder
  checkmate::assert(checkmate::checkDirectory(
    x = env$rootFolder,
    access = "wx"))

  if (!is.null(env$outputFolder)) {
    # outputFolder
    checkmate::assert(checkmate::check_character(
      env$outputFolder))
  }

  if (!is.null(env$tempFolder)) {
    # tempFolder
    checkmate::assert(checkmate::checkPathForOutput(
      env$tempFolder, overwrite = TRUE))
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

  check <- checkSaveSettings(environment())

  if (check) {
    saveSettings <- list(
      databaseName = databaseName,
      rootFolder = normalizePath(rootFolder, mustWork = FALSE),
      outputFolder = normalizePath(outputFolder, mustWork = FALSE),
      tempFolder = normalizePath(tempFolder, mustWork = FALSE))

    class(saveSettings) <- "saveSettings"

    return(saveSettings)
  }
}
