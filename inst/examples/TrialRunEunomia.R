# === Pre log stuff ====
library(CohortGenerator)
library(TreatmentPatterns)
library(dplyr)
library(data.table)

fs::dir_delete("output")
fs::dir_delete("temp")

# === Logging ====
# Add logger
ParallelLogger::clearLoggers()
ParallelLogger::addDefaultFileLogger(
  fileName = file.path(
    "dev/",
    "treatmentpatterns_log.txt"),
  name = "TreatmentPatterns_Logger")

ParallelLogger::logInfo(print(paste0(
  "Running package version ",
  packageVersion("TreatmentPatterns"))))

# === Create dataSettings ====
ParallelLogger::logInfo(print("=== Create dataSettings ===="))
dataSettings <- createDataSettings(
  connectionDetails = Eunomia::getEunomiaConnectionDetails(),
  cdmDatabaseSchema = "main",
  resultSchema = "main",
  cohortTable = "cohort_table")

# === Create cohorts ====
ParallelLogger::logInfo(print("=== Create cohorts ===="))
# Create empty cohort definition
cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()

# List JSON files for Viral Sinusitis
cohortJsonFiles <- list.files(
  "inst/examples/OMOP CDM/inst/cohorts/Viral Sinusitis/JSON/",
  full.names = TRUE)

# add cohort definition per file
for (i in seq_len(length(cohortJsonFiles))) {
  cohortJsonFileName <- cohortJsonFiles[i]
  cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
  # Here we read in the JSON in order to create the SQL
  # using [CirceR](https://ohdsi.github.io/CirceR/)
  # If you have your JSON and SQL stored differenly, you can
  # modify this to read your JSON/SQL files however you require
  cohortJson <- readChar(cohortJsonFileName, file.info(
    cohortJsonFileName)$size)

  cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)

  cohortSql <- CirceR::buildCohortQuery(
    cohortExpression,
    options = CirceR::createGenerateOptions(generateStats = FALSE))
  cohortsToCreate <- rbind(
    cohortsToCreate,
    data.frame(
      cohortId = i,
      cohortName = cohortName,
      sql = cohortSql,
      stringsAsFactors = FALSE))
}

# Create the cohort tables to hold the cohort generation results
cohortTableNames <- CohortGenerator::getCohortTableNames(
  cohortTable = dataSettings$cohortTable)

CohortGenerator::createCohortTables(
  connectionDetails = dataSettings$connectionDetails,
  cohortDatabaseSchema = dataSettings$resultSchema,
  cohortTableNames = cohortTableNames)

# Generate the cohorts
cohortsGenerated <- CohortGenerator::generateCohortSet(
  connectionDetails = dataSettings$connectionDetails,
  cdmDatabaseSchema = dataSettings$cdmDatabaseSchema,
  cohortDatabaseSchema = dataSettings$resultSchema,
  cohortTableNames = cohortTableNames,
  cohortDefinitionSet = cohortsToCreate)


# === Cohort data prep ====
ParallelLogger::logInfo(print("=== Cohort data prep ===="))
# Select Viral Sinusitis Cohort
targetCohort <- cohortsGenerated %>%
  filter(cohortName == "Viral Sinusitis") %>%
  select(cohortId, cohortName)

# Select everything BUT Viral Sinusitis cohorts
eventCohorts <- cohortsGenerated %>%
  filter(cohortName != "Viral Sinusitis") %>%
  select(cohortId, cohortName)

# === saveSettings ====
ParallelLogger::logInfo(print("=== saveSettings ===="))
fs::dir_create("output")
fs::dir_create("temp")

saveSettings <- TreatmentPatterns::createSaveSettings(
  databaseName = "Eunomia",
  rootFolder = getwd(),
  outputFolder = file.path(getwd(), "output", "Eunomia"))

# === cohortSettings ====
ParallelLogger::logInfo(print("=== cohortSettings ===="))
cohortSettings <- TreatmentPatterns::createCohortSettings(
  targetCohorts = targetCohort,
  eventCohorts = eventCohorts)

# Write cohortTable
fs::dir_create(file.path(saveSettings$outputFolder, "settings"))

write.csv(
  x = cohortSettings$cohortsToCreate,
  file = file.path(
    saveSettings$outputFolder, "settings", "cohorts_to_create.csv"),
  row.names = FALSE)

# Connect to database
con <- DatabaseConnector::connect(dataSettings$connectionDetails)

# Extract files from DB, write to outputFolder
invisible(lapply(cohortTableNames, function(tableName) {
  tbl <- TreatmentPatterns:::extractFile(
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

# Disconnect from database
DatabaseConnector::disconnect(con)

# === pathwaySettings ====
ParallelLogger::logInfo(print("=== pathwaySettings ===="))
pathwaySettings <- createPathwaySettings(
  cohortSettings = cohortSettings,
  studyName = "Viral_Sinusitis")

# add analysis
pathwaySettings <- addPathwayAnalysis(
  pathwaySettings = pathwaySettings,
  targetCohortIds = targetCohort$cohortId,
  eventCohortIds = eventCohorts$cohortId[-1],
  studyName = "One drug less")

# === ConstructPathways ====
ParallelLogger::logInfo(print("=== ConstructPathways ===="))
# 1) Create target/event cohorts of interest
# > Done with CohortGenerator

# 2) Optional, only for OMOP-CDM data: Perform baseline characterization of
#    study/target population
# > Out of scope

# 3) Construct treatment pathways
TreatmentPatterns::constructPathways(
  dataSettings = dataSettings,
  pathwaySettings = pathwaySettings,
  saveSettings = saveSettings)

# 4) Generate output (sunburst plots, Sankey diagrams and more)
ParallelLogger::logInfo(print("=== Generate outputs ===="))
TreatmentPatterns::generateOutput(saveSettings)

ParallelLogger::unregisterLogger("TreatmentPatterns_Logger")
