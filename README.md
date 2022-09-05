# TreatmentPatterns Package

This R package contains the resources for performing a treatment pathway analysis of a study population of interest in observational databases. The package partially relies on the Observational Medical Outcomes Partnership Common Data Model (OMOP CDM), but the main parts of the package are also usable with different data formats.

Reference: Markus, A. F., Verhamme, K. M., Kors, J. A., & Rijnbeek, P. R. (2022). [TreatmentPatterns: An R package to facilitate the standardized development and analysis of treatment patterns across disease domains](https://doi.org/10.1016/j.cmpb.2022.107081).Computer Methods and Programs in Biomedicine, 107081.

## Features

- Extracts the necessary data from a database in OMOP CDM format or directly imports the cohorts from a csv file.
- Performs baseline characterization of the study population of interest (only for databases in OMOP CDM format).
- Treatment pathways are constructed consisting of specified events of interest (e.g. prescriptions of drugs, therapies, other treatments) for specified target cohorts (study populations of interest).
- Aggregate output statistics are generated to describe the treatment sequence observed in the study population of interest including sunburst plots, Sankey diagrams, percentage(s) of people treated (with certain treatments), treatment changes over time, average duration of event eras. 
- All results can be explored in an interactive Shiny application.


## Screenshots
Demo: [example shiny application](https://mi-erasmusmc.shinyapps.io/TreatmentPatterns/).

<table>
<tr valign="bottom">
<td width = 50%>

<img src="https://github.com/mi-erasmusmc/TreatmentPatterns/blob/master/docs/sunburstplot.png"/>

</td>
<td width = 50%>
  
<img src="https://github.com/mi-erasmusmc/TreatmentPatterns/blob/master/docs/sankeydiagram.png"/>

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
- Vignette: [Perform a study using the TreatmentPatterns package](https://github.com/mi-erasmusmc/TreatmentPatterns/blob/master/docs/TreatmentPatternsStudy.pdf)
- Package manual: [TreatmentPatterns](https://github.com/mi-erasmusmc/TreatmentPatterns/blob/master/docs/TreatmentPatterns_1.0.0.pdf)

## Package Structure
<img src="https://github.com/mi-erasmusmc/TreatmentPatterns/blob/master/docs/package.png"/>


## Support
We use the <a href="https://github.com/mi-erasmusmc/TreatmentPatterns/issues">GitHub issue tracker</a> for all bugs/issues/enhancements.
