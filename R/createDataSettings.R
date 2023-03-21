#' checkDataSettings
#'
#' Check createDataSettings parameters.
#'
#' @param env Environment containging all the function environment variables.
#'
#' @return TRUE if all the assertions pass.
checkDataSettings <- function(env) {
  # Check connectionDetails
  checkmate::assert(
    checkmate::checkClass(
      env$connectionDetails,
      "ConnectionDetails"),
    checkmate::checkCharacter(
      x = env$connectionDetails$dbms,
      len = 1,
      null.ok = FALSE),
    combine = "and"
  )

  # check cdmDatabaseSchema
  checkmate::assert(
    checkmate::checkCharacter(
      env$cdmDatabaseSchema,
      null.ok = FALSE,
      len = 1)
  )

  # check resultSchema
  checkmate::assert(
    checkmate::checkCharacter(
      env$resultSchema,
      null.ok = FALSE,
      len = 1)
  )

  # cohortTable
  checkmate::assert(
    checkmate::checkCharacter(
      env$cohortTable,
      null.ok = FALSE,
      len = 1)
  )
  return(TRUE)
}

#' createDataSettings
#'
#' Create a dataSettings object containing information about how to connect to
#' a database.
#'
#' @param connectionDetails
#'     Only for omopCDM TRUE: An object of type connectionDetails as created
#'     using the createConnectionDetails function in the DatabaseConnector
#'     package.
#'
#' @param cdmDatabaseSchema
#'     Only for omopCDM TRUE: Schema name where your patient-level data
#'     resides. Note that for SQL Server, this should include both the database
#'     and schema name, for example 'cdm_data.dbo'.
#'
#' @param cohortTable
#'     Only for omopCDM TRUE: The name of the table that will be created in
#'     the cohortDatabaseSchema. This table will hold the target and event
#'     cohorts used in this study.
#'     
#' @param resultSchema
#'     Schema name of results. 
#'
#' @return
#'     Object dataSettings.
#' @export
createDataSettings <- function(
    connectionDetails,
    cdmDatabaseSchema,
    resultSchema,
    cohortTable = "treatmentpatterns_cohorts") {

  check <- checkDataSettings(environment())

  if (check) {
    dataSettings <- list(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      resultSchema = resultSchema,
      cohortTable = cohortTable)

    class(dataSettings) <- "dataSettings"

    return(dataSettings)
  }
}
