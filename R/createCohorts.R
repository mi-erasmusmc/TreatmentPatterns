
#' Only for OMOP-CDM TRUE: Extract target and event cohorts from the database using the definitions included in the package.
#' Cohorts can be defined 1) in ATLAS (recommended) or 2) by creating custom concept sets in combination with a SQL cohort template (advanced).
#'
#' @param connection           Connection to database server.
#' @param connectionDetails    An object of type connectionDetails as created using the createConnectionDetails function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data resides. Note that for SQL Server, 
#'                             this should include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Only for OMOP-CDM TRUE: Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_results.dbo'.
#' @param cohortTable          The name of the table that will be created in the cohortDatabaseSchema.
#'                             This table will hold the target and event cohorts used in this study.
#' @param instFolder           Name of local folder to place all settings and cohorts; make sure to use forward slashes (/).              
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes (/).
#' @param loadCohorts          etting to retrieve cohort definitions from ATLAS WebApi.
#' @param baseUrl              The base URL for the WebApi instance, for example: "http://server.org:80/WebAPI".
#'                             Note, there is no trailing '/'. If trailing '/' is used, you may receive an error. 
#' @param generateCohorts      Setting to (re)generate cohortTable in the database.
#' @param flowChart            Setting to return numbers for flowchart with inclusion/exclusion criteria for ATLAS cohorts. 
#' @export

createCohorts <- function(connection,
                          connectionDetails,
                          cdmDatabaseSchema,
                          cohortDatabaseSchema,
                          cohortTable,
                          instFolder,
                          outputFolder,
                          loadCohorts = FALSE,
                          baseUrl = NULL,
                          generateCohorts = TRUE,
                          flowChart = TRUE) {
  
  # Check if directory exists
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)
  
  # Load information cohorts to create
  pathToCsv <- paste0(instFolder, "/settings/cohorts_to_create.csv")
  cohortsToCreate <- readr::read_csv(pathToCsv, col_types = list("i","c","c","c","i"))
  write.csv(cohortsToCreate, file.path(outputFolder, "cohort.csv"), row.names = FALSE)

  if (generateCohorts) {
    # Create study cohort table structure
    ParallelLogger::logInfo("Creating table for the cohorts")
    sql <- loadRenderTranslateSql(sql = paste0(system.file(package = "TreatmentPatterns"),"/SQL/CreateCohortTable.sql"),
                                  dbms = connectionDetails$dbms,
                                  cohort_database_schema = cohortDatabaseSchema,
                                  cohort_table = cohortTable)
    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
    
    # Create inclusion rule statistics tables
    ParallelLogger::logInfo("Creating inclusion rule statistics tables")
    sql <- loadRenderTranslateSql(sql = paste0(system.file(package = "TreatmentPatterns"),"/SQL/CreateInclusionStatsTables.sql"),
                                  dbms = connectionDetails$dbms,
                                  cohort_database_schema = cohortDatabaseSchema,
                                  cohort_inclusion_table = "cohort_inclusion",
                                  cohort_inclusion_result_table = "cohort_inclusion_result",
                                  cohort_inclusion_stats_table =  "cohort_inclusion_stats",
                                  cohort_summary_stats_table =  "cohort_summary_stats",
                                  cohort_censor_stats_table = "cohort_censor_stats")
    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
    
    # In case of custom definitions: load custom definitions
    pathToCsv <- paste0(instFolder,"/settings/eventcohorts_custom.csv")
    if (file.exists(pathToCsv)) {
      custom_definitions <- readr::read_csv(pathToCsv, col_types = readr::cols())
    }
    
    # Instantiate cohorts
    ParallelLogger::logInfo("Insert cohort of interest into the cohort table")
    
    for (i in 1: nrow(cohortsToCreate)) {
      writeLines(paste0("Creating cohort:", cohortsToCreate$cohortName[i], " ", cohortsToCreate$cohortDefinition[i]))
      
      if (cohortsToCreate$cohortDefinition[i] == "ATLAS") {
        if (loadCohorts) {
          writeLines(paste("Inserting cohort:", cohortsToCreate$cohortName[i]))
          ROhdsiWebApi::insertCohortDefinitionInPackage(cohortId = cohortsToCreate$atlasId[i], 
                                                        name = cohortsToCreate$cohortName[i], 
                                                        jsonFolder = paste0(instFolder,"/cohorts/JSON"),
                                                        sqlFolder = paste0(instFolder,"/cohorts/SQL"),
                                                        baseUrl = baseUrl, 
                                                        generateStats = TRUE)
        }
        
        # Populate cohort_inclusion table with names of the rules
        cohortDefinition <- RJSONIO::fromJSON(content = paste0(instFolder,"/cohorts/JSON/", cohortsToCreate$cohortName[i], ".json"), digits = 23)
        
        inclusionRules <- tidyr::tibble()
        nrOfRules <- length(cohortDefinition$InclusionRules)
        
        if (nrOfRules > 0) {
          for (r in 1:nrOfRules) {
            inclusionRules <- dplyr::bind_rows(inclusionRules, tidyr::tibble(cohortDefinitionId = cohortsToCreate$cohortId[i],
                                                                             ruleSequence = r - 1,
                                                                             name = cohortDefinition$InclusionRules[[r]]$name))
          }
          
          DatabaseConnector::insertTable(connection = connection,
                                         tableName = paste(cohortDatabaseSchema,
                                                           "cohort_inclusion",
                                                           sep = "."),
                                         data = inclusionRules,
                                         dropTableIfExists = FALSE,
                                         createTable = FALSE,
                                         tempTable = FALSE,
                                         camelCaseToSnakeCase = TRUE)
        }
        
        # Generate cohort
        sql <- loadRenderTranslateSql(sql = paste0(instFolder, "/cohorts/SQL/",cohortsToCreate$cohortName[i], ".sql"),
                                      dbms = connectionDetails$dbms,
                                      cdm_database_schema = cdmDatabaseSchema,
                                      results_database_schema = cohortDatabaseSchema,
                                      vocabulary_database_schema = cdmDatabaseSchema,
                                      target_database_schema = cohortDatabaseSchema,
                                      target_cohort_table = cohortTable,
                                      target_cohort_id = cohortsToCreate$cohortId[i])
        DatabaseConnector::executeSql(connection, sql)
        
      } else if (cohortsToCreate$cohortDefinition[i] == "Custom") {
        
        # Load in concept sets (later: change to -> generate sql to form concept sets)
        concept_set <- custom_definitions[custom_definitions$cohortName == cohortsToCreate$cohortName[i],"conceptSet"]
        concept_set <- paste0("(", substr(concept_set, 2, nchar(concept_set)-1), ")")
        
        if (is.null(concept_set))
        {
          warning("Concept set is empty")
        }
        
        # Insert concept set in SQL template to create cohort
        sql <- loadRenderTranslateSql(sql = paste0(system.file(package = "TreatmentPatterns"), "/SQL/CohortDrugTemplate.sql"),
                                      dbms = connectionDetails$dbms,
                                      cdm_database_schema = cdmDatabaseSchema,
                                      vocabulary_database_schema = cdmDatabaseSchema,
                                      target_database_schema = cohortDatabaseSchema,
                                      target_cohort_table = cohortTable,
                                      target_cohort_id = cohortsToCreate$cohortId[i],
                                      concept_set = concept_set)
        DatabaseConnector::executeSql(connection, sql)
        
      } else {
        warning("Cohort definition not implemented, specify ATLAS or Custom")
      }
    }
  }
  
  # Check number of subjects per cohort
  ParallelLogger::logInfo("Counting cohorts")
  counts <- getCohortCounts(connection = connection,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            cohortTable = cohortTable,
                            cohortIds = "")
  write.csv(counts, file.path(outputFolder, "cohort_counts.csv"), row.names = FALSE)
  
  # Check if all target cohorts have non-zero count
  checkCohorts <- setdiff(cohortsToCreate$cohortId,counts$cohortDefinitionId)
  
  if(length(checkCohorts) != 0) {
    warning(paste0("Cohort definition ", paste0(checkCohorts, collapse = ","), " has zero count. "))
  }
  
  # Return numbers for flowchart with inclusion/exclusion criteria
  if(flowChart) {
    cohort_inclusion <- extractFile(connection, "cohort_inclusion", cohortDatabaseSchema, connectionDetails$dbms)
    write.csv(cohort_inclusion, file.path(outputFolder, "cohort_inclusion.csv"), row.names = FALSE)
    
    cohort_inclusion_result <- extractFile(connection, "cohort_inclusion_result", cohortDatabaseSchema, connectionDetails$dbms)
    write.csv(cohort_inclusion_result, file.path(outputFolder, "cohort_inclusion_result.csv"), row.names = FALSE)
    
    cohort_inclusion_stats <- extractFile(connection, "cohort_inclusion_stats", cohortDatabaseSchema, connectionDetails$dbms)
    write.csv(cohort_inclusion_stats, file.path(outputFolder, "cohort_inclusion_stats.csv"), row.names = FALSE)

    cohort_summary_stats <- extractFile(connection, "cohort_summary_stats", cohortDatabaseSchema, connectionDetails$dbms)
    write.csv(cohort_summary_stats, file.path(outputFolder, "cohort_summary_stats.csv"), row.names = FALSE)
    
    cohort_censor_stats <- extractFile(connection, "cohort_censor_stats", cohortDatabaseSchema, connectionDetails$dbms)
    write.csv(cohort_censor_stats, file.path(outputFolder, "cohort_censor_stats.csv"), row.names = FALSE)
  }
  
  ParallelLogger::logInfo("createCohorts done.")
}
