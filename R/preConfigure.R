#' preConfigure
#'
#' @param saveSettings saveSettings object
#' @param cohortSettings cohortSettings object
#' @param dataSettings dataSettings object
#' @param cohortTableNames cohortTableNames
#'
#' @return NULL
#' @export
preConfigure <- function(saveSettings, cohortSettings, dataSettings, cohortTableNames) {
  # Write cohortTable
  fs::dir_create(file.path(saveSettings$outputFolder, "settings"))
  
  write.csv(
    x = cohortSettings$cohortsToCreate,
    file = file.path(saveSettings$outputFolder, "settings", "cohorts_to_create.csv"),
    row.names = FALSE)
  
  con <- DatabaseConnector::connect(dataSettings$connectionDetails)
  # Disconnect from database on exit
  on.exit(DatabaseConnector::disconnect(con))
  
  # Extract files from DB, write to outputFolder
  invisible(lapply(cohortTableNames, function(tableName) {
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
  }))
}
