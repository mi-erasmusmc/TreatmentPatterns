#' extractFile
#' 
#' Extract table with specific name from database server.
#'
#' @param connection DatabaseConnector connection object
#' @param tableName Name of table
#' @param resultsSchema Schema of results
#' @param dbms Name of dbms to use
#' 
#' @return data the extracted table as a data.frame
#' @export
#' 
#' @examples
#' \dontrun{
#'   con <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
#'   extractFile(con, "person", "main", "sqlite")
#'}
extractFile <- function(connection, tableName, resultsSchema, dbms) {
  # Assertions
  checkmate::checkClass(connection, "DatabaseConnectorDbiConnection")
  checkmate::checkCharacter(tableName, len = 1)
  checkmate::checkCharacter(resultsSchema, len = 1)
  checkmate::checkCharacter(dbms, len = 1)
  
  parameterizedSql <- "SELECT * FROM @resultsSchema.@tableName"
  renderedSql <- SqlRender::render(
    parameterizedSql,
    resultsSchema = resultsSchema,
    tableName = tableName)

  translatedSql <- SqlRender::translate(
    renderedSql,
    targetDialect = dbms)
  DatabaseConnector::querySql(connection, translatedSql)
}


#' writeCohortTable
#' 
#' Writes the cohortTable from the database to a specified path in saveSettings.
#'
#' @param saveSettings saveSettings object
#' @param tableName Name of the cohort table in the database.
#' @param dataSettings dataSettings object
#' @param cohortSettings cohortSettings object
#'
#' @return NULL
#' @export
writeCohortTable <- function(saveSettings, cohortSettings, dataSettings, tableName) {
  # Write cohortTable
  fs::dir_create(file.path(saveSettings$outputFolder))
  
  write.csv(
    x = cohortSettings$cohortsToCreate,
    file = paste0(saveSettings$outputFolder, "/cohortsToCreate.csv"))
  
  con <- DatabaseConnector::connect(dataSettings$connectionDetails)
  # Disconnect from database on exit
  on.exit(DatabaseConnector::disconnect(con))
  
  # Extract files from DB, write to outputFolder
  tbl <- extractFile(
    connection = con,
    tableName = tableName,
    resultsSchema = dataSettings$resultSchema,
    dbms = dataSettings$connectionDetails$dbms)
  
  write.csv(
    tbl,
    file.path(
      saveSettings$outputFolder,
      paste0(tableName, ".csv")),
    row.names = FALSE)
}
