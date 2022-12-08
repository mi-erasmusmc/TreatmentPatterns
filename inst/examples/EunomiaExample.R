library(TreatmentPatterns)

conDetails <- Eunomia::getEunomiaConnectionDetails()

dataSettings <- createDataSettings(
  OMOP_CDM = TRUE,
  connectionDetails = conDetails,
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main",
  cohortTable = "treatmentpatterns_cohorts"
)

cohortSettings <-
  createCohortSettings(
    targetCohorts = data.frame(
      cohortId = c(1),
      atlasId = c(1781027),
      cohortName = c("Viral sinusitis"),
      conceptSet = ""
    ),
    eventCohorts = data.frame(
      cohortId = c(10, 11, 12, 13, 14),
      atlasId = c(1781028, 1781029, 1781030, 1781031, 1781032),
      cohortName = c(
        "Acetaminophen",
        "Amoxicillin",
        "Aspirin",
        "Clavulanate",
        "Penicillin V"
      ),
      conceptSet = c("", "", "", "", "")
    ),
    baseUrl = "http://api.ohdsi.org:8080/WebAPI",
    loadCohorts = TRUE
  )

characterizationSettings <- createCharacterizationSettings(
  baselineCovariates = data.frame(
    covariateName = c('Male', 'Age',
                      'Charlson comorbidity index score'),
    covariateId = c(8507001, 1002, 1901)
  ),
  returnCovariates = "selection"
)

# specify different sets of pathway settings, with adjusted settings
pathwaySettings <- createPathwaySettings(targetCohortId = 1,
                                         eventCohortIds = c(10, 11, 12, 13, 14))

saveSettings <- createSaveSettings(databaseName = "Eunomia",
                                   rootFolder = getwd())

TreatmentPatterns::executeTreatmentPatterns(
  dataSettings = dataSettings,
  cohortSettings = cohortSettings,
  characterizationSettings = characterizationSettings,
  pathwaySettings = pathwaySettings,
  saveSettings = saveSettings,
)

con <- DatabaseConnector::connect(conDetails)

sql <- "
SELECT * FROM treatmentpatterns_cohorts"

res <- DatabaseConnector::renderTranslateQuerySql(con, sql)

dim(res)

library(dplyr)

res %>% group_by(.data$COHORT_DEFINITION_ID) %>% tally()
