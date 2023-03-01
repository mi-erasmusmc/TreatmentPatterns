#' checkDataSettings
#'
#' Check createDataSettings parameters.
#'
#' @param connectionDetails connectionDetails object.
#' @param cdmDatabaseSchema Schema name of CDM.
#' @param resultSchema Schema name of results.
#' @param cohortTable Cohort table name.
#'
#' @import checkmate
#'
#' @return TRUE if all the assertions pass.
checkDataSettings <- function(
    connectionDetails,
    cdmDatabaseSchema,
    resultSchema,
    cohortTable) {

  # Check connectionDetails
  checkmate::assert(
    checkmate::checkClass(
      connectionDetails,
      "ConnectionDetails"),
    checkmate::checkCharacter(
      x = connectionDetails$dbms,
      len = 1,
      null.ok = FALSE),
    combine = "and"
  )

  # check cdmDatabaseSchema
  checkmate::assert(
    checkmate::checkCharacter(
      cdmDatabaseSchema,
      null.ok = FALSE,
      len = 1)
  )

  # check resultSchema
  checkmate::assert(
    checkmate::checkCharacter(
      resultSchema,
      null.ok = FALSE,
      len = 1)
  )

  # cohortTable
  checkmate::assert(
    checkmate::checkCharacter(
      cohortTable,
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

  check <- checkDataSettings(
    connectionDetails,
    cdmDatabaseSchema,
    resultSchema,
    cohortTable)

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
