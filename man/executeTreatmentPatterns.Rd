% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/executeTreatmentPatterns.R
\name{executeTreatmentPatterns}
\alias{executeTreatmentPatterns}
\title{This is the main function which runs all parts of the treatment pathways analysis. The following tasks are performed sequentially:
1) Create target/event cohorts of interest,
2) Optional, only for OMOP-CDM data: Perform baseline characterization of study/target population,
3) Construct treatment pathways,
4) Generate output (sunburst plots, Sankey diagrams and more),
5) Launch shiny application to visualize the results.}
\usage{
executeTreatmentPatterns(
  dataSettings,
  cohortSettings,
  characterizationSettings = NULL,
  pathwaySettings,
  saveSettings,
  runCreateCohorts = TRUE,
  runCohortCharacterization = TRUE,
  runConstructPathways = TRUE,
  runGenerateOutput = TRUE,
  launchShiny = TRUE
)
}
\arguments{
\item{dataSettings}{Settings object as created by createDataSettings().}

\item{cohortSettings}{Settings object as created by createCohortSettings().}

\item{characterizationSettings}{Optional, only for OMOP-CDM data: Settings object as created by createCharcterizationSettings().}

\item{pathwaySettings}{Settings object as created by createPathwaySettings().}

\item{saveSettings}{Settings object as created by createSaveSettings().}

\item{runCreateCohorts}{Setting to run 1) createCohorts().}

\item{runCohortCharacterization}{Optional, only for OMOP-CDM data: Setting to run 2) cohortCharacterization().}

\item{runConstructPathways}{Setting to run 3) constructPathways().}

\item{runGenerateOutput}{Setting to run 4) generateOutput().}

\item{launchShiny}{Setting to run 5) launchResultsExplorer().}
}
\description{
This is the main function which runs all parts of the treatment pathways analysis. The following tasks are performed sequentially:
1) Create target/event cohorts of interest,
2) Optional, only for OMOP-CDM data: Perform baseline characterization of study/target population,
3) Construct treatment pathways,
4) Generate output (sunburst plots, Sankey diagrams and more),
5) Launch shiny application to visualize the results.
}
