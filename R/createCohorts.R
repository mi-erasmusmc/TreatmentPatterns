

#' Create target/event cohorts of interest.
#'
#' @param dataSettings Settings object as created by createDataSettings().
#' @param cohortSettings Settings object as created by createCohortSettings().
#' @param saveSettings Settings object as created by createSaveSettings().
#'
#' @export
createCohorts <- function(dataSettings, cohortSettings, saveSettings) {
  
  # Check if inputs correct
  if(!class(dataSettings)%in%c('dataSettings')){
    stop('Incorrect class for dataSettings')
  } 
  
  if(!class(cohortSettings)%in%c('cohortSettings')){
    stop('Incorrect class for cohortSettings')
  } 
  
  if(!class(saveSettings)%in%c('saveSettings')){
    stop('Incorrect class for saveSettings')
  } 
  
  # Check if directory exists and create if necessary
  if (!file.exists(file.path(saveSettings$outputFolder, "settings")))
    dir.create(file.path(saveSettings$outputFolder, "settings"), recursive = TRUE)
  
  # Save cohorts to create
  cohortsToCreate <- cohortSettings$cohortsToCreate
  write.csv(cohortsToCreate, file.path(saveSettings$outputFolder, "settings", "cohorts_to_create.csv"), row.names = FALSE)
  
  # For OMOP-CDM TRUE: Extract target and event cohorts from the database using the definitions included in the package.
  # Cohorts can be defined 1) in ATLAS (recommended) or 2) by creating custom concept sets in combination with a SQL cohort template (advanced).
  if (dataSettings$OMOP_CDM) {
    
    # TODO: check if cohortSettings is not NULL!
    
    # Connect to database
    connection <- DatabaseConnector::connect(dataSettings$connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
    
    # Generate cohorts in database
    if (cohortSettings$generateCohorts) {
      
      # Create study cohort table structure
      ParallelLogger::logInfo("Creating table for the cohorts")
      sql <- loadRenderTranslateSql(sql = file.path(system.file(package = "TreatmentPatterns"),"SQL","CreateCohortTable.sql"),
                                    dbms = dataSettings$connectionDetails$dbms,
                                    cohort_database_schema = dataSettings$cohortDatabaseSchema,
                                    cohort_table = dataSettings$cohortTable)
      DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)

      # Create inclusion rule statistics tables
      ParallelLogger::logInfo("Creating inclusion rule statistics tables")
      sql <- loadRenderTranslateSql(sql = file.path(system.file(package = "TreatmentPatterns"),"SQL","CreateInclusionStatsTables.sql"),
                                    dbms = dataSettings$connectionDetails$dbms,
                                    cohort_database_schema = dataSettings$cohortDatabaseSchema,
                                    cohort_inclusion_table = "cohort_inclusion",
                                    cohort_inclusion_result_table = "cohort_inclusion_result",
                                    cohort_inclusion_stats_table =  "cohort_inclusion_stats",
                                    cohort_summary_stats_table =  "cohort_summary_stats",
                                    cohort_censor_stats_table = "cohort_censor_stats")
      DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)

      # Load (and save) location cohorts
      cohortsFolder <- cohortSettings$cohortsFolder
      if (is.null(cohortsFolder)) {
        cohortsFolder <- file.path(saveSettings$outputFolder, "cohorts")
      }
      
      # Instantiate cohorts
      ParallelLogger::logInfo("Insert cohort of interest into the cohort table")
      
      # TODO: add error messages if cohorts not there etc.
      for (i in 1:nrow(cohortsToCreate)) {
        if (!is.na(cohortsToCreate$atlasId[i]) & cohortsToCreate$atlasId[i] != "") { # If atlasId is not NA/empty -> standard ATLAS definition
          writeLines(paste0("Creating ATLAS cohort: ", cohortsToCreate$cohortName[i]))
          
          if (cohortSettings$loadCohorts) {
            writeLines(paste("Inserting cohort:", cohortsToCreate$cohortName[i]))
            
            if (ROhdsiWebApi::isValidId(ids = cohortsToCreate$atlasId[i], baseUrl = cohortSettings$baseUrl, category = 'cohort'))
            {
              ROhdsiWebApi::insertCohortDefinitionInPackage(cohortId = cohortsToCreate$atlasId[i], 
                                                            name = cohortsToCreate$cohortName[i], 
                                                            jsonFolder = file.path(cohortsFolder, "JSON"),
                                                            sqlFolder = file.path(cohortsFolder, "SQL"),
                                                            baseUrl = cohortSettings$baseUrl, 
                                                            generateStats = TRUE)
            } else {
              stop (paste0("atlasId missing or incorrect for ", cohortsToCreate$cohortName[i]))
            }
          }
          
          # Populate cohort_inclusion table with names of the rules
          tryCatch(cohortDefinition <- RJSONIO::fromJSON(content = file.path(cohortsFolder, "JSON", paste0(cohortsToCreate$cohortName[i], ".json")), digits = 23),
                   error = function(e) {ParallelLogger::logInfo(print(paste0("Problem with JSON file cohort definition, check location of cohorts 'cohortSettings$cohortFolder' or load cohorts from ATLAS with 'cohortSettings$loadCohorts = TRUE': ", e)))})
          
          inclusionRules <- tidyr::tibble()
          nrOfRules <- length(cohortDefinition$InclusionRules)
          
          if (nrOfRules > 0) {
            for (r in 1:nrOfRules) {
              inclusionRules <- dplyr::bind_rows(inclusionRules, tidyr::tibble(cohortDefinitionId = cohortsToCreate$cohortId[i],
                                                                               ruleSequence = r - 1,
                                                                               name = cohortDefinition$InclusionRules[[r]]$name))
            }
            
            DatabaseConnector::insertTable(connection = connection,
                                           tableName = paste0(dataSettings$cohortDatabaseSchema, ".cohort_inclusion"),
                                           data = inclusionRules,
                                           dropTableIfExists = FALSE,
                                           createTable = FALSE,
                                           tempTable = FALSE,
                                           camelCaseToSnakeCase = TRUE)
          }
          
          # Generate cohort
          sql <- loadRenderTranslateSql(sql = file.path(cohortsFolder, "SQL", paste0(cohortsToCreate$cohortName[i], ".sql")),
                                        dbms = dataSettings$connectionDetails$dbms,
                                        cdm_database_schema = dataSettings$cdmDatabaseSchema,
                                        results_database_schema = dataSettings$cohortDatabaseSchema,
                                        vocabulary_database_schema = dataSettings$cdmDatabaseSchema,
                                        target_database_schema = dataSettings$cohortDatabaseSchema,
                                        target_cohort_table = dataSettings$cohortTable,
                                        target_cohort_id = cohortsToCreate$cohortId[i])
          DatabaseConnector::executeSql(connection, sql)
        } else if (!is.na(cohortsToCreate$conceptSet[i]) & cohortsToCreate$conceptSet[i] != "") { # If conceptSet is not NA/empty -> standard ATLAS definition
          writeLines(paste0("Creating Custom cohort: ", cohortsToCreate$cohortName[i]))
          
          # Load in concept sets
          concept_set <- cohortsToCreate$conceptSet[i]
          concept_set <- gsub(";", ",", concept_set)
          concept_set <- paste0("(", concept_set, ")")
          
          # Insert concept set in SQL template to create cohort
          sql <- loadRenderTranslateSql(sql = file.path(system.file(package = "TreatmentPatterns"), "SQL", "CohortDrugTemplate.sql"),
                                        dbms = dataSettings$connectionDetails$dbms,
                                        cdm_database_schema = dataSettings$cdmDatabaseSchema,
                                        vocabulary_database_schema = dataSettings$cdmDatabaseSchema,
                                        target_database_schema = dataSettings$cohortDatabaseSchema,
                                        target_cohort_table = dataSettings$cohortTable,
                                        target_cohort_id = cohortsToCreate$cohortId[i],
                                        concept_set = concept_set,
                                        include_descendants = cohortSettings$includeDescendants)
          DatabaseConnector::executeSql(connection, sql)
          
        } else {
          warning(paste0("Cohort definition not given, please add atlasId or conceptSet for ", cohortsToCreate$cohortName[i]))
        }
      }
    }
    
    
    # Return numbers for flowchart with inclusion/exclusion criteria
    # TODO: check which of these overviews necessary, remove others
    cohort_inclusion <- extractFile(connection, "cohort_inclusion", dataSettings$cohortDatabaseSchema, dataSettings$connectionDetails$dbms)
    write.csv(cohort_inclusion, file.path(saveSettings$outputFolder, "cohort_inclusion.csv"), row.names = FALSE)
    
    cohort_inclusion_result <- extractFile(connection, "cohort_inclusion_result", dataSettings$cohortDatabaseSchema, dataSettings$connectionDetails$dbms)
    write.csv(cohort_inclusion_result, file.path(saveSettings$outputFolder, "cohort_inclusion_result.csv"), row.names = FALSE)
    
    cohort_inclusion_stats <- extractFile(connection, "cohort_inclusion_stats", dataSettings$cohortDatabaseSchema, dataSettings$connectionDetails$dbms)
    write.csv(cohort_inclusion_stats, file.path(saveSettings$outputFolder, "cohort_inclusion_stats.csv"), row.names = FALSE)
    
    cohort_summary_stats <- extractFile(connection, "cohort_summary_stats", dataSettings$cohortDatabaseSchema, dataSettings$connectionDetails$dbms)
    write.csv(cohort_summary_stats, file.path(saveSettings$outputFolder, "cohort_summary_stats.csv"), row.names = FALSE)
    
    cohort_censor_stats <- extractFile(connection, "cohort_censor_stats", dataSettings$cohortDatabaseSchema, dataSettings$connectionDetails$dbms)
    write.csv(cohort_censor_stats, file.path(saveSettings$outputFolder, "cohort_censor_stats.csv"), row.names = FALSE)
    
    # Check number of subjects per cohort
    ParallelLogger::logInfo("Counting cohorts")
    counts <- getCohortCounts(connection = connection,
                              cohortDatabaseSchema = dataSettings$cohortDatabaseSchema,
                              cohortTable = dataSettings$cohortTable,
                              cohortIds = "")
    write.csv(counts, file.path(saveSettings$outputFolder, "cohort_counts.csv"), row.names = FALSE)
    
  } else { # For OMOP-CDM FALSE: Import the target and event cohorts from a csv file in which the cohorts are stored. 
    
    # Load cohorts in from file
    # Required columns: cohortId, personId, startDate, endDate
    data <- data.table(readr::read_csv(dataSettings$cohortLocation), col_types = list("d", "d", "D", "D"))
    
    if (!all(c("cohortId", "personId", "startDate", "endDate") %in% colnames(data))) {
      stop('Incorrect input for cohorts')
    }
    
    # Check number of subjects per cohort
    ParallelLogger::logInfo("Counting cohorts")
    counts <- data.frame(cohortDefinitionId = cohortsToCreate$cohortId)
    counts$cohortCount <- sapply(counts$cohortDefinitionId, function(c) {
      length(data$person_id[data$cohortId == c]) 
    })
    counts$personCount <- sapply(counts$cohortDefinitionId, function(c) {
      length(unique(data$person_id[data$cohortId == c]))
    })
    write.csv(counts, file.path(saveSettings$outputFolder, "cohort_counts.csv"), row.names = FALSE)
  }
  
  # Check if all cohorts have non-zero count
  checkCohorts <- setdiff(cohortsToCreate$cohortId,counts$cohortId)
  
  if(length(checkCohorts) != 0) {
    warning(paste0("Cohort definition ", paste0(checkCohorts, collapse = ","), " has zero count. "))
  }
  
  ParallelLogger::logInfo("createCohorts done.")
}
