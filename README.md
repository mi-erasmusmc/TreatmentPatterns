# TreatmentPatterns Package

## Description
This R package contains the resources for performing a treatment pathway analysis of a study population of interest.

*Background*:
Clinical guidelines are available for a wide range of conditions. However, there is often a lack of knowledge on how these guidelines are followed in practice. This package gives insight in treatment patterns to help understand and address current research gaps in clinical care by utilizing the powerful analytical tools developed by the Observational Health Data Sciences and Informatics (OHDSI) community. 

*Methods*: 
This study will describe the treatment pathway consisting of specified events of interest (e.g. prescriptions of drugs, therapies, other treatments) for specified target cohorts (study populations of interest). For each of the target cohorts, a sunburst diagram (and more) is produced to describe the treatment sequence observed in the target population. 

## Customization
If you like to use this package to design a study follow the following steps:

1. Define target/event cohorts and add to package.
2. Define study settings (mandatory) and characterization (optional).
3. Add custom analysis parts (optional).
For more details, see vignette "SetUpNewStudy" (in progress).

## Installation/Execution
If you like to execute the customized study package against a database follow these instructions:

1. Download and open the R package using RStudio. 
2. Build the package (packages required are listed in DESCRIPTION file).
3. In extras -> CodeToRun.R: specify connection details (if OMOP-CDM) or cohort location (if Other format). 
4. To execute the study run code in CodeToRun.R. 
5. The results are located in '~/shiny/output'.
6. Run the Shiny App for an interactive visualization of the results.
7. Share the results in the automatically generated zip folder.