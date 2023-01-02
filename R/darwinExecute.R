darwinExecute <- function(
    targetCohort,
    cohortPath,
    dataSettings,
    pathwaySettings,
    characterizatonSettings,
    saveSettings) {
    # Assuming database is in accordance with the OMOP CDM and that there are
    # defined cohorts.
    
    targetCohorts <- cohortTable %>%
      dplyr::filter(.data$cohortName == targetCohort) %>%
      dplyr::select(.data$cohortName, .data$cohortId)
    
    eventCohorts <- cohortTable %>%
      dplyr::filter(.data$cohortName != targetCohort) %>%
      dplyr::select(.data$cohortName, .data$cohortId)
    
    # Create cohortSettings
    cohortSettings <- createCohortSettings(
      targetCohorts = targetCohorts,
      eventCohorts = eventCohorts)

    # Step 3
    TreatmentPatterns::constructPathways(
      dataSettings = dataSettings,
      pathwaySettings = pathwaySettings,
      saveSettings = saveSettings)

    # Step 4
    TreatmentPatterns::generateOutput(
      saveSettings = saveSettings)
    
    # Step 5
    TreatmentPatterns::launchResultsExplorer(saveSettings)
    
  }

cohortPath <- "D:/Users/mvankessel/Desktop/Viral_sinusitis/JSON BACKUP/"

saveSettings <- createSaveSettings(databaseName = "MyCoolDatabase", rootFolder = ".", outputFolder = "output")
dataSettings <- createDataSettings(
  omopCDM = TRUE,
  connectionDetails = Eunomia::getEunomiaConnectionDetails(),
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main",
  cohortTable = "treatmentpatterns_cohorts"
)
characterizationSettings <- createCharacterizationSettings(
  baselineCovariates = data.frame(
    covariateName = c('Male', 'Age',
                      'Charlson comorbidity index score'),
    covariateId = c(8507001, 1002, 1901)
  ),
  returnCovariates = "selection"
)

# pathwaySettings <- createPathwaySettings(targetCohortId = 7,
#                                          eventCohortIds = c(1, 2, 3, 4, 5, 6))
# 
# # cohortSettings <- createCohortSettings(targetCohorts = targetCohorts, eventCohorts = eventCohorts)
# 
# TreatmentPatterns::cohortCharacterization(
#   dataSettings = dataSettings,
#   characterizationSettings = characterizationSettings,
#   saveSettings = saveSettings)
# 
# TreatmentPatterns::constructPathways(
#   dataSettings = dataSettings,
#   pathwaySettings = pathwaySettings,
#   saveSettings = saveSettings)
# 
# TreatmentPatterns::generateOutput(
#   saveSettings = saveSettings)
# 
# TreatmentPatterns::launchResultsExplorer(
#   saveSettings = saveSettings)
# 
# darwinExecute(cohortPath = "D:/Users/mvankessel/Desktop/Viral_sinusitis/JSON BACKUP/", 
#               dataSettings = dataSettings,
#               cohortSettings = cohortSettings,
#               pathwaySettings = pathwaySettings,
#               characterizatonSettings = characterizationSettings,
#               saveSettings = saveSettings)
# 
# connection <- DatabaseConnector::connect(dataSettings$connectionDetails)
# 
# DatabaseConnector::renderTranslateQuerySql(connection, "SELECT COUNT(*) FROM treatmentpatterns_cohorts GROUP BY")
# 

