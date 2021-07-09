# TreatmentPatterns Package

This R package contains the resources for performing a treatment pathway analysis of a study population of interest in observational databases. The package partially relies on the Observational Medical Outcomes Partnership Common Data Model (OMOP CDM), but the main parts of the package are also usable with different data formats.

## Features

- Extracts the necessary data from a database in OMOP CDM format or directly imports the cohorts from a csv file.
- Performs baseline characterization of the study population of interest (based on [CohortDiganostics](https://github.com/ohdsi/cohortdiagnostics), only for databases in OMOP CDM format).
- Treatment pathways are constructed consisting of specified events of interest (e.g. prescriptions of drugs, therapies, other treatments) for specified target cohorts (study populations of interest).
- Aggregate output statistics are generated to describe the treatment sequence observed in the study population of interest including sunburst plots, Sankey diagrams, percentage(s) of patients treated (with a certain treatment), and average duration of event cohorts. 


## Screenshots

<table>
<tr valign="bottom">
<td width = 50%>

<img src="..."/>

</td>
<td width = 50%>
  
<img src="..."/>

</td>
</tr><tr>
<td>Sunburst plot</td><td>Sankey diagram</td>
</tr>
</table>


## Installation

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including RTools and Java.

2. In R, use the following commands to download and install TreatmentPatterns:

  ```r
  install.packages("remotes")
  remotes::install_github("mi-erasmusmc/TreatmentPatterns")
  ```

## User Documentation
Available documentation includes:
- Vignette: [Designing a study using the TreatmentPatterns package](https://github.com/mi-erasmusmc/TreatmentPatterns/blob/master/docs/SetUpNewStudy.pdf)
- Vignette: [Executing a study using the TreatmentPatterns package](...)
- Package manual: [TreatmentPatterns.pdf](https://github.com/mi-erasmusmc/TreatmentPatterns/blob/master/docs/TreatmentPatterns_0.2.0.pdf)

## Support
We use the <a href="https://github.com/mi-erasmusmc/TreatmentPatterns/issues">GitHub issue tracker</a> for all bugs/issues/enhancements.

### Development status
TreatmentPatterns is under development.
