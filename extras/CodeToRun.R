# ------------------------------------------------------------------------
# Study settings
# ------------------------------------------------------------------------

## Select database format (Format of 'Observational Medical Outcomes Partnership Common Data Model' = TRUE or 'Other' = FALSE)
OMOP_CDM <- TRUE 

## Analysis settings
debugSqlFile <- "treatment_patterns.dsql"
cohortTable <- "treatment_patterns_cohorts"

runCreateCohorts <- TRUE
runCohortCharacterization <- TRUE # functionality only available for OMOP_CDM
runConstructPathways <- TRUE
runGenerateResults <- TRUE

## Load settings
study_settings <- data.frame(readr::read_csv("inst/Settings/study_settings.csv", col_types = readr::cols()))

# ------------------------------------------------------------------------
# If OMOP-CDM = TRUE -> enter all database credentials, ELSE enter database name
# ------------------------------------------------------------------------

if (OMOP_CDM) {
  user <- 'todo'
  password <- 'todo'
  cdmDatabaseSchemaList <- 'todo'
  cohortSchema <- 'todo'
  oracleTempSchema <- NULL
  databaseList <- 'todo' # name of the data source
  
  dbms <- 'todo'
  server <- 'todo'
  port <- 'todo'
  
  # Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = 'todo')
  
  cohortLocation <- NULL
  
  baseUrl <- 'todo'
  
  # Optional: specify where the temporary files will be created:
  # options(andromedatempdir = "...")
  
  # Connect to the server
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = password,
                                                                  port = port)
  
  connection <- DatabaseConnector::connect(dbms = dbms,connectionDetails = connectionDetails)
  
} else {
  connection <- NULL
  connectionDetails <- NULL
  cdmDatabaseSchema <- NULL
  cohortDatabaseSchema <- NULL
  baseUrl <- NULL
  
  databaseName <- 'todo'
  cohortLocation <- "inst/Settings/input_cohorts.csv"
}

outputFolder <- paste0(getwd(),"/shiny/output")

# ------------------------------------------------------------------------
# Run the study
# ------------------------------------------------------------------------

for (sourceId in 1:length(cdmDatabaseSchemaList)) {
  
  if (OMOP_CDM) {
    cdmDatabaseSchema <- cdmDatabaseSchemaList[sourceId]
    cohortDatabaseSchema <- cohortSchema
    databaseName <- databaseList[sourceId]
  }
  
  databaseId <- databaseName
  print(paste("Executing against", databaseName))
  
  outputFolderDB <- paste0(outputFolder, "/", databaseName)
  
  time0 <- Sys.time()
  executeTreatmentPatterns(
    OMOP_CDM = OMOP_CDM,
    connection = connection,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = paste0(cohortTable, "_", databaseName),
    outputFolder = outputFolderDB,
    databaseId = databaseId,
    databaseName = databaseName,
    baseUrl = baseUrl,
    cohortLocation = cohortLocation,
    runCreateCohorts = runCreateCohorts,
    runCohortCharacterization = runCohortCharacterization,
    runConstructPathways = runConstructPathways,
    runGenerateResults = runGenerateResults,
    study_settings = study_settings
  )
  time5 <- Sys.time()
  ParallelLogger::logInfo(paste0("Time needed to execute study for this database ", difftime(time5, time0, units = "mins")))
}


