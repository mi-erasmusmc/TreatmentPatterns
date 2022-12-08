library(TreatmentPatterns)

conDetails <- Eunomia::getEunomiaConnectionDetails()

dataSettings <- createDataSettings(
  OMOP_CDM = TRUE,
  connectionDetails = conDetails,
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main",
  cohortTable = "treatmentpatterns_cohorts"
)

saveSettings <- createSaveSettings(databaseName = "Eunomia",
                                   rootFolder = getwd())

createCohortsCG(
  path = "D:/Users/mvankessel/Desktop/Viral_sinusitis/JSON BACKUP/",
  targetCohortName = "EunomiaCG",
  dataSettings = dataSettings,
  saveSettings = saveSettings)
