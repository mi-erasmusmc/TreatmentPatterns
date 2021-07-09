
#' This function will create the target and event cohorts following the definitions included in
#' this package if OMOP-CDM = TRUE.
#'
#' @param connection           Connection to database server.
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data resides if OMOP-CDM = TRUE.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the target and event cohorts used in this
#'                             study.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/).
#' @param loadCohorts          Setting to load cohorts from ATLAS.
#' @param baseUrl              The base URL for theWebApi instance, for example: "http://server.org:80/WebAPI".
#'                             Note, there is no trailing '/'. If trailing '/' is used, you may receive an error.   
#' @param generateCohorts      Setting to extract specified target/event cohorts from database.
#' @param minCellCount         Minimum number of persons with a specific treatment pathway for the pathway to be included in analysis.
#' @param flowChart            Setting to return numbers for flowchart with inclusion/exclusion criteria.        
#' @export

createCohorts <- function(connection,
                          connectionDetails,
                          cdmDatabaseSchema,
                          cohortDatabaseSchema,
                          cohortTable,
                          outputFolder,
                          instFolder,
                          loadCohorts = FALSE,
                          baseUrl,
                          generateCohorts = TRUE,
                          minCellCount,
                          flowChart = TRUE) {
  
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
                                  cohort_summary_stats_table =  "cohort_summary_stats")
    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
    
    # In case of custom definitions: load custom definitions
    pathToCsv <- paste0(instFolder,"/settings/eventcohorts_custom.csv")
    custom_definitions <- readr::read_csv(pathToCsv, col_types = readr::cols())
    
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
  }
  
}

#' This function will import the target and event cohorts following the path included in
#' this package if OMOP-CDM = FALSE.
#' 
#' @param cohortLocation Location where cohorts are saved if OMOP-CDM = FALSE.
#' @param outputFolder Name of local folder to place results; make sure to use forward slashes (/).
#' @export
importCohorts <- function(cohortLocation, outputFolder) {
  
  # Load cohorts in from file
  # Required columns: cohortId, personId, startDate, endDate
  data <- data.table(readr::read_csv(cohortLocation), col_types = list("i", "i", "D", "D"))
  
  # Load information cohorts to create
  pathToCsv <- paste0(instFolder,"/settings/cohorts_to_create.csv")
  cohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols())
  write.csv(cohortsToCreate, file.path(outputFolder, "cohort.csv"), row.names = FALSE)
  
  # Check number of subjects per cohort
  ParallelLogger::logInfo("Counting cohorts")
  counts <- data.frame(cohortDefinitionId = cohortsToCreate$cohortId)
  
  counts$cohortCount <- sapply(counts$cohortDefinitionId, function(c) {
    length(data$person_id[data$cohortId == c]) 
  })
  
  counts$personCount <- sapply(counts$cohortDefinitionId, function(c) {
    length(unique(data$person_id[data$cohortId == c]))
  })
  
  write.csv(counts, file.path(outputFolder, "cohort_counts.csv"), row.names = FALSE)
  
  # Check if all cohorts have non-zero count
  checkCohorts <- setdiff(cohortsToCreate$cohortId,counts$cohortDefinitionId)
  
  if(length(checkCohorts) != 0) {
    warning(paste0("Cohort definition ", paste0(checkCohorts, collapse = ","), " has zero count. "))
  }
  
  
}

