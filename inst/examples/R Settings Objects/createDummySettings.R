dataSettings <- TreatmentPatterns::createDataSettings(
  connectionDetails = Eunomia::getEunomiaConnectionDetails(),
  cdmDatabaseSchema = "main",
  resultSchema = "main",
  cohortTable = "cohort_table")

cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()

cohortJsonFiles <- list.files(
  "inst/examples/OMOP CDM/inst/cohorts/Viral Sinusitis/JSON/",
  full.names = TRUE)

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

# Select Viral Sinusitis Cohort
targetCohort <- cohortsGenerated %>% 
  filter(cohortName == "Viral Sinusitis") %>%
  select(cohortId, cohortName)

# Select everything BUT Viral Sinusitis cohorts
eventCohorts <- cohortsGenerated %>% 
  filter(cohortName != "Viral Sinusitis") %>%
  select(cohortId, cohortName)

saveSettings <- TreatmentPatterns::createSaveSettings(
  databaseName = "Eunomia",
  rootFolder = getwd(),
  outputFolder = file.path(getwd(), "output", "Eunomia"))

cohortSettings <- TreatmentPatterns::createCohortSettings(
  targetCohorts = targetCohort,
  eventCohorts = eventCohorts)

pathwaySettings <- TreatmentPatterns::createPathwaySettings(
  cohortSettings = cohortSettings,
  studyName = "Viral_Sinusitis")

