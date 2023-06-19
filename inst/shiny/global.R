library(shiny)
library(shinydashboard)
library(reshape2)
library(ggplot2)
library(data.table)
library(DT)

if (exists("shinySettings")) { # run via TreatmentPatterns::launchResultsExplorer
  setwd(shinySettings$outputFolder)
} else {
  if (exists("pathResultsDirectory")) {
    setwd(pathResultsDirectory)
  } else {
    stop("Define location of outputFolder in pathResultsDirectory (folder ~/output including the different database results)")
  }
}

# local <- paste0(getwd(), "/output/")
local <- paste0(getwd())
addResourcePath("workingdirectory", local)

# Fixing the labels
included_databases <- list.dirs(local, recursive = FALSE, full.names = FALSE)
names(included_databases) <- included_databases # optional: change with own custom names
included_databases <- as.list(included_databases) 

# Import settings
pathway_settings <- data.frame(readr::read_csv(file.path(local, included_databases[[1]],"settings", "pathway_settings.csv"), col_types = readr::cols()))
cohorts <- data.frame(readr::read_csv(file.path(local, included_databases[[1]],"settings", "cohorts_to_create.csv"), col_types = readr::cols()))

all_targetcohorts <- unique(as.numeric(pathway_settings[pathway_settings$param == "targetCohortId",-1]))
names(all_targetcohorts) <- sapply(all_targetcohorts, function(c) cohorts$cohortName[cohorts$cohortId == c])

all_studynames <- unique(as.character(pathway_settings[pathway_settings$param == "studyName",-1]))
names(all_studynames) <- all_studynames # optional: change with own custom names
all_studynames <- as.list(all_studynames)

all_years <- list("Entire study period" = "all",  # TODO: adjust to study period
                  "Index year 2000" = "2000",
                  "Index year 2005" = "2005",
                  "Index year 2010" = "2010",
                  "Index year 2015" = "2015",
                  "Index year 2016" = "2016",
                  "Index year 2017" = "2017",
                  "Index year 2018" = "2018",
                  "Index year 2019" = "2019",
                  "Index year 2020" = "2020")

layers <- list("First-line treatment" = 1,
               "Second-line treatment" = 2,
               "Third-line treatment" = 3,
               "Fourth-line treatment" = 4,
               "Fifth-line treatment" = 5)

# Order characterization (alphabetic with some exceptions)
orderRows <- c("Number of persons", "Male" , "Age", "Charlson comorbidity index score")   

# Order event cohorts
orderEventCohorts <- c()

# Load in all results from output folder
characterization <- list()
summary_counts <- list()
summary_treated <- list()
summary_eventcohorts <- list()
summary_eventcohorts_year <- list()
duration <- list()

suppressWarnings({
  for (d in included_databases) {
    
    # Load characterization for entire database
    try(characterization[[d]]  <- read.csv(paste0(local, "/", d, "/characterization/characterization.csv")), silent = TRUE)
    
    # Load remaining file per study population
    summary_counts_d <- list()
    summary_treated_d <- list()
    summary_eventcohorts_d <- list()
    summary_eventcohorts_year_d <- list()
    duration_d <- list()
    
    # For database find study populations
    available_studies <- list.dirs(path = paste0(local, "/", d), full.names = FALSE, recursive = FALSE)
    
    for (s in available_studies[!(available_studies %in% c("characterization", "settings"))]) {
      
      try({
        # Load summary counts
        file <- read.csv(paste0(local, "/", d, "/", s, "/", d , "_", s, "_summary_cnt.csv"))
        transformed_file <- data.table(year = character(), number_target = integer(), number_pathways = integer())
        transformed_file <- rbind(transformed_file, list("all", file$N[file$index_year == "Number of persons in target cohort NA"], file$N[file$index_year == "Number of pathways (before minCellCount) in NA"]))
        
        for (y in all_years[-c(1)]) {
          try(transformed_file <- rbind(transformed_file, list(y, file$N[file$index_year == paste0("Number of persons in target cohort ", y)], file$N[file$index_year == paste0("Number of pathways (before minCellCount) in ", y)])), silent = TRUE)
        }
        
        transformed_file$perc <- round(transformed_file$number_pathways * 100.0 / transformed_file$number_target,1)
        summary_counts_d[[s]] <- transformed_file
        
        # Load percentage treated
        summary_treated_d[[s]] <- file[grep("Percentage treated", file$index_year),] 
        
      }, silent = TRUE)
  
      # Load event cohorts classes file for available study settings
      try(summary_eventcohorts_d[[s]] <- read.csv(paste0(local, "/", d, "/", s, "/",d , "_", s, "_percentage_groups_treated_noyear.csv")), silent = TRUE)
      try(summary_eventcohorts_year_d[[s]] <- read.csv(paste0(local, "/", d, "/", s, "/",d , "_", s, "_percentage_groups_treated_withyear.csv")), silent = TRUE)
      
      # Load duration file for available study populations
      try(duration_d[[s]] <- read.csv(paste0(local, "/", d, "/", s, "/",d , "_", s, "_duration.csv")), silent = TRUE)
    }
    
    summary_counts[[d]] <- summary_counts_d
    summary_treated[[d]] <- summary_treated_d
    summary_eventcohorts[[d]] <- summary_eventcohorts_d
    summary_eventcohorts_year[[d]] <- summary_eventcohorts_year_d
    duration[[d]] <- duration_d
  }
})

writeLines("Data Loaded")

