#' loadRenderTranslateSql
#'
#' @param sql 
#' @param oracleTempSchema 
#' @param dbms 
#' @param warnOnMissingParameters 
#' @param output 
#' @param outputFile 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
loadRenderTranslateSql <- function(
    sql,
    oracleTempSchema = NULL,
    dbms = "postgresql",
    warnOnMissingParameters = TRUE,
    output = FALSE,
    outputFile,
    ...) {
  if (grepl('.sql', sql)) {
    pathToSql <- sql
    parameterizedSql <- readChar(pathToSql, file.info(pathToSql)$size)[1]
  } else {
    parameterizedSql <- sql
  }
  
  renderedSql <- SqlRender::render(
    sql = parameterizedSql,
    warnOnMissingParameters = warnOnMissingParameters,
    ...)
  
  renderedSql <- SqlRender::translate(
    sql = renderedSql,
    targetDialect = dbms,
    oracleTempSchema = oracleTempSchema)
  
  if (output == TRUE) {
    SqlRender::writeSql(renderedSql, outputFile)
    writeLines(paste("Created file '", outputFile, "'", sep = ""))
  }
  return(renderedSql)
}


#' extractFile
#'
#' @param connection 
#' @param tableName 
#' @param resultsSchema 
#' @param dbms 
#'
#' @return
#' @export
#'
#' @examples
extractFile <- function(connection, tableName, resultsSchema, dbms) {
  parameterizedSql <- "SELECT * FROM @resultsSchema.@tableName"
  renderedSql <- SqlRender::render(
    parameterizedSql,
    resultsSchema = resultsSchema,
    tableName = tableName)
  
  translatedSql <- SqlRender::translate(
    renderedSql,
    targetDialect = dbms)
  data <- DatabaseConnector::querySql(connection, translatedSql)
}

exportCSV <- function(saveSettings, cohortsToCreate, cohortTableNames, targetCohortName) {
  # export csv
  if (!file.exists(file.path(saveSettings$outputFolder, "settings"))) {
    dir.create(
      path = file.path(saveSettings$outputFolder, "settings"),
      recursive = TRUE)
  }
  
  cohorts_to_create <- cohortsToCreate %>% 
    select(.data$cohortId, .data$cohortName) %>%
    mutate(cohortType = case_when(.data$cohortName == .env$targetCohortName ~ "target",
                                  .data$cohortName != .env$targetCohortName ~ "event"))
  
  write.csv(
    x = cohorts_to_create,
    file = file.path(saveSettings$outputFolder, "settings", "cohorts_to_create.csv"),
    row.names = FALSE)
  
  invisible(lapply(cohortTableNames, function(file) {
    con <- DatabaseConnector::connect(dataSettings$connectionDetails)
    on.exit(DatabaseConnector::disconnect(con))
    
    tbl <- extractFile(
      tableName = file,
      connection = con,
      resultsSchema = dataSettings$cohortDatabaseSchema,
      dbms = dataSettings$connectionDetails$dbms
    )
    
    write.csv(
      x = tbl,
      file.path(saveSettings$outputFolder, paste0(file, ".csv")),
      row.names = FALSE
    )
  }))
  
}

#' createCohortsCG
#'
#' Create cohorts with CohortGenerator
#'
#' @param path Path to folder containing .JSON or .SQL cohort files.
#' @param type Default: "JSON" types: "JSON" or "SQL"
#' @param dataSettings TreatmentPatterns dataSettings object
#' @param countOutput Default: FALSE Logical value to return table of count data
#'
#' @export
createCohortsCG <-
  function(path,
           targetCohortName,
           dataSettings,
           saveSettings,
           type = "JSON") {
    connection <- DatabaseConnector::connect(dataSettings$connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
    
    CohortFiles <- list.files(path, full.names = TRUE)
    
    cohortsToCreate <-
      dplyr::bind_rows(lapply(seq_len(length(CohortFiles)), function(i) {
        cohortFileName <- CohortFiles[i]
        cohortName <-
          tools::file_path_sans_ext(basename(cohortFileName))
        
        fileContent <- readChar(cohortFileName,
                                file.info(cohortFileName)$size)
        
        if (type == "JSON") {
          if (as.logical(dplyr::ends_with(vars = tolower(cohortFileName), match = "json"))) {
            cohortExpression <- CirceR::cohortExpressionFromJson(fileContent)
            cohortSql <- CirceR::buildCohortQuery(
              expression = cohortExpression,
              options = CirceR::createGenerateOptions(generateStats = FALSE)
            )
          } else {
            warning("File extension did not match .json")
          }
        } else if (type == "SQL") {
          if (as.logical(dplyr::ends_with(vars = tolower(cohortFileName), match = "sql"))) {
            cohortSql <- fileContent
          } else {
            warning("File extension did not match .sql")
          }
        } else {
          stop("Pick of on the following types: 'JSON' or 'SQL'")
        }
        data.frame(
          cohortId = i,
          cohortName = cohortName,
          sql = cohortSql,
          stringsAsFactors = FALSE
        )
      }))
    
    cohortTableNames <-
      CohortGenerator::getCohortTableNames(cohortTable = dataSettings$cohortTable)
    
    CohortGenerator::createCohortTables(
      connectionDetails = dataSettings$connectionDetails,
      cohortDatabaseSchema = dataSettings$cdmDatabaseSchema,
      cohortTableNames = cohortTableNames
    )
    
    # Generate the cohorts
    cohortsGenerated <-
      CohortGenerator::generateCohortSet(
        connectionDetails = dataSettings$connectionDetails,
        cdmDatabaseSchema = dataSettings$cdmDatabaseSchema,
        cohortDatabaseSchema = dataSettings$cohortDatabaseSchema,
        cohortTableNames = cohortTableNames,
        cohortDefinitionSet = cohortsToCreate
      )
    
    # Insert tables
    sql <- loadRenderTranslateSql(
      sql = file.path(system.file(
        package = "TreatmentPatterns"),
        "SQL","CreateInclusionStatsTables.sql"),
      dbms = dataSettings$connectionDetails$dbms,
      cohort_database_schema = dataSettings$cohortDatabaseSchema,
      cohort_inclusion_table = "cohort_inclusion",
      cohort_inclusion_result_table = "cohort_inclusion_result",
      cohort_inclusion_stats_table =  "cohort_inclusion_stats",
      cohort_summary_stats_table =  "cohort_summary_stats",
      cohort_censor_stats_table = "cohort_censor_stats")
    
    DatabaseConnector::executeSql(
      connection = connection,
      sql = sql,
      progressBar = FALSE,
      reportOverallTime = FALSE)
    
    exportCSV(
      saveSettings = saveSettings,
      cohortsToCreate = cohortsToCreate,
      cohortTableNames = cohortTableNames,
      targetCohortName = targetCohortName)
  }
