dataSettings <- createDataSettings(
  OMOP_CDM = TRUE,
  connectionDetails = Eunomia::getEunomiaConnectionDetails(),
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main",
  cohortTable = "treatmentpatterns_cohorts"
)

targetCohorts <- data.frame(
  cohortId = c(1),
  atlasId = c(1777380),
  cohortName = c('Hypertension'),
  conceptSet = ""
)

eventCohorts <- data.frame(
  cohortId = c(10, 11, 12, 13, 14),
  atlasId = c(1777381, 1777382, 1777383, 1777384, 1777385),
  cohortName = c(
    'Hydrochlorothiazide',
    'Metorolol',
    'Amlodipine',
    'Lisinopril',
    'Losartan'
  ),
  conceptSet = c("", "", "", "", "")
)

cohortSettings <- createCohortSettings(
  targetCohorts = targetCohorts,
  eventCohorts = eventCohorts,
  baseUrl = "http://api.ohdsi.org:8080/WebAPI",
  loadCohorts = TRUE
)

covariateNames <-
  c('Male', 'Age', 'Charlson comorbidity index score')
covariateIds <- c(8507001, 1002, 1901)

characterizationSettings <-
  createCharacterizationSettings(
    baselineCovariates =
      data.frame(covariateName = covariateNames,
                 covariateId = covariateIds),
    returnCovariates = "selection"
  )

pathwaySettings <- createPathwaySettings(targetCohortId = 1,
                                         eventCohortIds = c(10, 11, 12, 13, 14))

saveSettings <- createSaveSettings(databaseName = "Eunomia",
                                   rootFolder = getwd())