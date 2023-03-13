#' extractFile
#' 
#' Extract table with specific name from database server.
#'
#' @param connection DatabaseConnector connection object
#' @param tableName Name of table
#' @param resultsSchema Schema of results
#' @param dbms Name of dbms to use
#'
#' @import checkmate
#' @importFrom DatabaseConnector querySql
#' @import SqlRender
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
