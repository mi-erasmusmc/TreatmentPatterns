
#' Generate all result files and plots from constructed pathways.
#'
#' @param databaseName Name of the database that will appear in the results.
#' @param studyName Name for the study corresponding to the current pathway settings.
#' @param outputFolder Name of local folder to place results; make sure to use forward slashes (/).
#' @param tempFolder Name of local folder to place intermediate results (not to be shared); make sure to use forward slashes (/).
#'
#' @export
generateResults <- function(databaseName,
                            studyName,
                            outputFolder,
                            tempFolder) {
  
  # Load pathway settings
  pathwaySettings <- data.frame(readr::read_csv(paste0(instFolder, "/settings/pathway_settings.csv"), col_types = readr::cols()))
  
  # Save pathway settings
  write.csv(pathwaySettings, file.path(outputFolder, "settings.csv"), row.names = FALSE)
  
  # For all different pathway settings
  settings <- colnames(pathwaySettings)[grepl("analysis", colnames(pathwaySettings))]
  
  for (s in settings) {
    studyName <- pathwaySettings[pathwaySettings$param == "studyName",s]
    
    ParallelLogger::logInfo(print(paste0("Creating output: ", studyName)))
    
    # Select cohorts included
    targetCohortId <- pathwaySettings[pathwaySettings$param == "targetCohortId",s]
    eventCohortIds <- pathwaySettings[pathwaySettings$param == "eventCohortIds",s]
    eventCohortIds <- unlist(strsplit(eventCohortIds, split = ","))
    
    # Result settings
    maxPathLength <- as.integer(pathwaySettings[pathwaySettings$param == "maxPathLength",s]) # Maximum number of steps included in treatment pathway (max 5)
    minCellCount <- as.integer(pathwaySettings[pathwaySettings$param == "minCellCount",s]) # Minimum number of persons with a specific treatment pathway for the pathway to be included in analysis
    minCellMethod <- pathwaySettings[pathwaySettings$param == "minCellMethod",s] # Select to completely remove / sequentially adjust (by removing last step as often as necessary) treatment pathways below minCellCount
    groupCombinations <- pathwaySettings[pathwaySettings$param == "groupCombinations",s] # Select to group all non-fixed combinations in one category 'other’ in the sunburst plot
    addNoPaths <- pathwaySettings[pathwaySettings$param == "addNoPaths",s] # Select to include untreated persons without treatment pathway in the sunburst plot
    
    path <- paste0(outputFolder, "/", studyName, "/", databaseName, "_", studyName)
    temp_path <- paste0(tempFolder, "/", studyName, "/", databaseName, "_", studyName)
    
    # Transform results for output
    transformed_data <- transformTreatmentSequence(studyName = studyName, path = path, temp_path = temp_path, maxPathLength = maxPathLength, minCellCount = minCellCount)
    
    if (!is.null(transformed_data)) {
      file_noyear <- as.data.table(transformed_data[[1]])
      file_withyear <- as.data.table(transformed_data[[2]])
      
      # Compute percentage of people treated with each event cohort separately and in the form of combination treatments
      outputPercentageGroupTreated(data = file_noyear, studyName = studyName, eventCohortIds = eventCohortIds, groupCombinations = TRUE, outputFolder = outputFolder, outputFile = paste(path,"_percentage_groups_treated_noyear.csv",sep=''))
      outputPercentageGroupTreated(data = file_withyear, studyName = studyName, eventCohortIds = eventCohortIds, groupCombinations = TRUE, outputFolder = outputFolder, outputFile = paste(path,"_percentage_groups_treated_withyear.csv",sep=''))
      
      # Duration of era's
      transformDuration(outputFolder = outputFolder, path = path, temp_path = temp_path, eventCohortIds = eventCohortIds, maxPathLength = maxPathLength, groupCombinations = TRUE, minCellCount = minCellCount)
      
      # Save (censored) results file_noyear and file_year
      saveTreatmentSequence(file_noyear = file_noyear, file_withyear = file_withyear, path = path, temp_path = temp_path, groupCombinations = groupCombinations, minCellCount = minCellCount, minCellMethod = minCellMethod)
      
      file_noyear <- as.data.table(readr::read_csv(paste(path,"_file_noyear.csv",sep=''), col_types = readr::cols()))
      file_withyear <- as.data.table(readr::read_csv(paste(path,"_file_withyear.csv",sep=''), col_types = readr::cols()))
      
      # Treatment pathways sankey diagram
      createSankeyDiagram(data = file_noyear, outputFolder = outputFolder, databaseName = databaseName, studyName = studyName, groupCombinations = TRUE)
      
      # Treatment pathways sunburst plot 
      outputSunburstPlot(data = file_noyear, databaseName = databaseName, eventCohortIds = eventCohortIds, studyName = studyName, outputFolder=outputFolder, path=path, addNoPaths=addNoPaths, maxPathLength=maxPathLength)
      outputSunburstPlot(data = file_withyear, databaseName = databaseName, eventCohortIds = eventCohortIds, studyName = studyName, outputFolder=outputFolder, path=path, addNoPaths=addNoPaths, maxPathLength=maxPathLength)
      
    }
  }
  
  ParallelLogger::logInfo("generateResults done.")
}


# Transformed treatment pathways files to use in generating output.
#
# Input:
# studyName Name for the study corresponding to the current settings.
# path Path to the output folder including data specific file name.
# temp_path Path to temporary output files.
# maxPathLength Maximum number of steps included in treatment pathway (max 5).
# minCellCount Minimum number of persons with a specific treatment pathway for the pathway to be included in analysis.
#
# Output: List with two dataframes.
transformTreatmentSequence <- function(studyName, path, temp_path, maxPathLength, minCellCount) {
  
  file <- try(data.table(readr::read_csv(paste(temp_path,"_paths.csv",sep=''), col_types = readr::cols())), silent = TRUE)
  
  if (class(file) == "try-error") {
    ParallelLogger::logInfo(warning(paste0("Data is empty for study settings ", studyName)))
    return (NULL)
  }
  
  # Apply maxPathLength and remove unnecessary columns
  layers <- as.vector(colnames(file)[!grepl("index_year|freq", colnames(file))])
  layers <- layers[1:min(length(layers),maxPathLength)]
  
  columns <- c(layers, "index_year", "freq")
  
  file <- file[,..columns]
  
  # Summarize which non-fixed combinations occurring
  findCombinations <- apply(file, 2, function(x) grepl("+", x, fixed = TRUE))
  
  combinations <- as.matrix(file)[findCombinations == TRUE]
  num_columns <- sum(grepl("event_cohort_name", colnames(file)))
  freqCombinations <- matrix(rep(file$freq, times = num_columns), ncol = num_columns)[findCombinations == TRUE]
  
  summaryCombinations <- data.table(combination = combinations, freq = freqCombinations)
  summaryCombinations <- summaryCombinations[,.(freq=sum(freq)), by=combination][order(-freq)]
  
  summaryCombinations <- summaryCombinations[freq >= minCellCount,]
  write.csv(summaryCombinations, file=paste(path,"_combinations.csv",sep=''), row.names = FALSE)
  
  # Group the resulting treatment paths
  file_noyear <- file[,.(freq=sum(freq)), by=layers]
  file_withyear <- file[,.(freq=sum(freq)), by=c(layers, "index_year")]
  
  ParallelLogger::logInfo("transformTreatmentSequence done")
  
  return(list(file_noyear, file_withyear))
}

# Input:
# data Dataframe with event cohorts of the target cohort in different columns.
# eventCohortIds IDs to refer to event cohorts.
# studyName Name for the study corresponding to the current settings.
# groupCombinations Select to group all non-fixed combinations in one category 'other’ in the sunburst plot.
# path Path to the output folder including data specific file name.
# outputFolder Path to the output folder.
# outputFile Name of output file.
outputPercentageGroupTreated <- function(data, eventCohortIds, studyName, groupCombinations, path, outputFolder, outputFile) {
  if (is.null(data$index_year)) {
    # For file_noyear compute once
    result <- computePercentageGroupTreated(data, eventCohortIds, groupCombinations, outputFolder)
    
  } else {
    # For file_withyear compute per year
    years <- unlist(unique(data[,"index_year"]))
    
    results <- lapply(years, function(y) {
      subset_data <- data[index_year == as.character(y),]
      if (nrow(subset_data) != 0) {
        subset_result <- cbind(y, computePercentageGroupTreated(subset_data, eventCohortIds, groupCombinations, outputFolder))
      } else {
        ParallelLogger::logInfo(warning(paste0("Subset of data is empty for study settings ", studyName, " in year ", y)))
        subset_result <- NULL
      }
      return(subset_result)
    }) 
    
    result <- rbindlist(results)
    result$y <- as.character(result$y)
  }
  
  write.csv(result, file=outputFile, row.names = FALSE)
  ParallelLogger::logInfo("outputPercentageGroupTreated done")
}

# Help function to compute percentage of treated patient with certain event cohorts for outputPercentageGroupTreated.
computePercentageGroupTreated <- function(data, eventCohortIds, groupCombinations, outputFolder) {
  layers <- as.vector(colnames(data)[!grepl("index_year|freq", colnames(data))])
  cohorts <- readr::read_csv(paste(outputFolder, "/cohort.csv",sep=''), col_types = list("i", "c", "c", "c"))
  outcomes <- c(cohorts$cohortName[cohorts$cohortId %in% eventCohortIds], "Other")
  
  # Group non-fixed combinations in one group according to groupCobinations
  data <- groupInfrequentCombinations(data, groupCombinations)
  
  percentGroupLayer <- sapply(layers, function(l) {
    percentGroup <- sapply(outcomes, function(g) {
      sumGroup <- sum(data$freq[data[,..l] == g], na.rm = TRUE)
      sumAllNotNA <- sum(data$freq[!is.na(data[,..l])])
      return(sumGroup * 100.0 / sumAllNotNA)
    })
  })
  
  # Add outcome names
  result <- data.frame(outcomes, percentGroupLayer, stringsAsFactors = FALSE)
  colnames(result) <- c("outcomes", layers)
  rownames(result) <- NULL
  
  paths_all <- sum(data$freq)
  
  result$ALL_LAYERS <- sapply(outcomes, function(o) {
    paths_with_outcome <- sum(sapply(1:nrow(data), function(r) ifelse(o %in% data[r,], data[r,freq], 0)))
    return(paths_with_outcome * 100.0 / paths_all)
  })
  
  # Add rows for total, fixed combinations, all combinations
  result <- rbind(result, c("Fixed combinations",colSums(result[grepl("\\&", result$outcomes), layers]), NA), c("All combinations",colSums(result[grepl("Other|\\+|\\&", result$outcomes), layers]), NA), c("Monotherapy",colSums(result[!grepl("Other|\\+|\\&", result$outcomes), layers]), NA))
  
  result$ALL_LAYERS[result$outcomes == "Fixed combinations"] <- sum(sapply(1:nrow(data), function(r) ifelse(any(grepl("\\&", data[r,])), data[r,freq], 0))) * 100.0 / paths_all
  result$ALL_LAYERS[result$outcomes == "All combinations"] <- sum(sapply(1:nrow(data), function(r) ifelse(any(grepl("Other|\\+|\\&", data[r,])), data[r,freq], 0))) * 100.0 / paths_all
  result$ALL_LAYERS[result$outcomes == "Monotherapy"] <- sum(sapply(1:nrow(data), function(r) ifelse(any(!grepl("Other|\\+|\\&", data[r,])), data[r,freq], 0))) * 100.0 / paths_all
  
  return(result)
}

# Input:
# outputFolder Path to the output folder.
# path Path to the output folder including data specific file name.
# temp_path Path to temporary output files.
# eventCohortIds IDs to refer to event cohorts.
# maxPathLength Maximum number of steps included in treatment pathway (max 5).
# groupCombinations Select to group all non-fixed combinations in one category 'other’ in the sunburst plot.
# minCellCount Minimum number of persons with a specific treatment pathway for the pathway to be included in analysis.
transformDuration <- function(outputFolder, path, temp_path, eventCohortIds, maxPathLength, groupCombinations, minCellCount) {
  
  file <- data.table(readr::read_csv(paste(temp_path,"_event_seq_processed.csv",sep=''), col_types = list("c", "i", "i", "D", "D", "i", "i", "c")))
  
  # Remove unnessary columns
  columns <- c("duration_era", "event_seq", "event_cohort_name")
  file <- file[,..columns]
  file$duration_era <- as.numeric(file$duration_era)
  
  # Apply maxPathLength
  file <- file[event_seq <= maxPathLength,]
  
  # Group non-fixed combinations in one group according to groupCobinations
  file <- groupInfrequentCombinations(file, groupCombinations)
  
  result <- file[,.(AVG_DURATION=round(mean(duration_era),3), COUNT = .N), by = c("event_cohort_name", "event_seq")][order(event_cohort_name, event_seq)]
  
  # Add column for total treated, fixed combinations, all combinations
  file$fixed_combinations[grepl("\\&", file$event_cohort_name)] <- 1
  file$all_combinations[grepl("Other|\\+|\\&", file$event_cohort_name)] <- 1
  file$monotherapy[!grepl("Other|\\+|\\&", file$event_cohort_name)] <- 1
  
  # Duration average per layer
  result_total_concept <- file[,.(event_cohort_name = "Total treated", AVG_DURATION = round(mean(duration_era),2), COUNT = .N), by = c("event_seq")]
  
  result_fixed_combinations <- file[,.(event_cohort_name = "Fixed combinations", AVG_DURATION = round(mean(duration_era),2), COUNT = .N), by = c("event_seq", "fixed_combinations")]
  result_fixed_combinations <- result_fixed_combinations[!is.na(fixed_combinations),]
  result_fixed_combinations$fixed_combinations <- NULL
  
  result_all_combinations <- file[,.(event_cohort_name = "All combinations", AVG_DURATION = round(mean(duration_era),2), COUNT = .N), by = c("event_seq", "all_combinations")]
  result_all_combinations <- result_all_combinations[!is.na(all_combinations),]
  result_all_combinations$all_combinations <- NULL
  
  result_monotherapy <- file[,.(event_cohort_name = "Monotherapy", AVG_DURATION = round(mean(duration_era),2), COUNT = .N), by = c("event_seq", "monotherapy")]
  result_monotherapy <- result_monotherapy[!is.na(monotherapy),]
  result_monotherapy$monotherapy <- NULL
  
  # Duration average all layers 
  result_total_seq <- file[,.(event_seq = "Overall", AVG_DURATION= round(mean(duration_era),2), COUNT = .N), by = c("event_cohort_name")]
  
  results_total_treated <- file[,.(event_cohort_name = "Total treated", event_seq = "Overall", AVG_DURATION= round(mean(duration_era),2), COUNT = .N)]
  
  results_total_fixed <- file[fixed_combinations == 1,.(event_cohort_name = "Fixed combinations", event_seq = "Overall", AVG_DURATION= round(mean(duration_era),2), COUNT = .N)]
  
  results_total_mono <- file[all_combinations == 1,.(event_cohort_name = "All combinations", event_seq = "Overall", AVG_DURATION= round(mean(duration_era),2), COUNT = .N)]
  
  results_total_allcombi <- file[monotherapy == 1,.(event_cohort_name = "Monotherapy", event_seq = "Overall", AVG_DURATION= round(mean(duration_era),2), COUNT = .N)]
  
  results <- rbind(result, result_total_concept, result_fixed_combinations, result_all_combinations, result_monotherapy, result_total_seq, results_total_treated, results_total_fixed, results_total_mono, results_total_allcombi)
  
  # Add missing groups
  cohorts <- readr::read_csv(paste(outputFolder, "/cohort.csv",sep=''), col_types = list("i", "c", "c", "c"))
  outcomes <- c(cohorts$cohortName[cohorts$cohortId %in% eventCohortIds], "Other")
  
  for (o in outcomes[!(outcomes %in% results$event_cohort_name)]) {
    results <- rbind(results, list(o, "Overall", 0.0, 0))
  }
  
  # Remove durations computed using less than minCellCount observations
  results[COUNT < minCellCount, c("AVG_DURATION", "COUNT")] <- NA
  
  write.csv(results, paste(path,"_duration.csv",sep=''), row.names = FALSE)
  
  ParallelLogger::logInfo("transformDuration done")
}

# Censored treatment pathways files to share.
#
# Input:
# file_noyear Dataframe with aggregated treatment pathways of the target cohort in different rows (unique pathways, with frequency).
# file_withyear Idem, but split over index_year.
# path Path to the output folder including data specific file name.
# temp_path Path to temporary output files.
# groupCombinations Select to group all non-fixed combinations in one category 'other’ in the sunburst plot.
# minCellCount Minimum number of persons with a specific treatment pathway for the pathway to be included in analysis.
# minCellMethod Select to completely remove / sequentially adjust (by removing last step as often as necessary) treatment pathways below minCellCount.
saveTreatmentSequence <- function(file_noyear, file_withyear, path, temp_path, groupCombinations, minCellCount, minCellMethod) {
  
  # Group non-fixed combinations in one group according to groupCobinations
  file_noyear <- groupInfrequentCombinations(file_noyear, groupCombinations)
  file_withyear <- groupInfrequentCombinations(file_withyear, groupCombinations)
  
  layers <- as.vector(colnames(file_noyear)[!grepl("index_year|freq", colnames(file_noyear))])
  
  # Apply minCellCount by adjusting to other most similar path (removing last treatment in path) or else remove complete path
  if (minCellMethod == "Adjust") {
    col <- ncol(file_noyear) - 1
    while (sum(file_noyear$freq < minCellCount) > 0 & col !=0) {
      ParallelLogger::logInfo(paste("Change col ", col, " to NA for ", sum(file_noyear$freq < minCellCount), " paths with too low frequency (without year)"))
      
      file_noyear[freq < minCellCount, col] <- NA
      file_noyear <- file_noyear[,.(freq=sum(freq)), by=layers]
      
      col <- col - 1
    }
    
    col <- ncol(file_withyear) - 2
    while (sum(file_withyear$freq < minCellCount) > 0 & col !=0) {
      ParallelLogger::logInfo(paste("Change col ", col, " to NA for ", sum(file_withyear$freq < minCellCount), " paths with too low frequency (with year)"))
      
      file_withyear[freq < minCellCount,col] <- NA
      file_withyear <- file_withyear[,.(freq=sum(freq)), by=c(layers, "index_year")]
      
      col <- col - 1
    }
    
    # If path becomes completely NA -> add to "Other" group to distinguish from non-treated
    file_noyear$event_cohort_name1[is.na(file_noyear$event_cohort_name1)] <- "Other"
    file_noyear <- file_noyear[,.(freq=sum(freq)), by=layers]
    
    file_withyear$event_cohort_name1[is.na(file_withyear$event_cohort_name1)] <- "Other"
    file_withyear <- file_withyear[,.(freq=sum(freq)), by=c(layers, "index_year")]
  }
  
  ParallelLogger::logInfo(paste("Remove ", sum(file_noyear$freq < minCellCount), " paths with too low frequency (without year)"))
  file_noyear <- file_noyear[freq >= minCellCount,]
  
  ParallelLogger::logInfo(paste("Remove ", sum(file_withyear$freq < minCellCount), " paths with too low frequency (with year)"))
  file_withyear <- file_withyear[freq >= minCellCount,]
  
  summary_counts <- readr::read_csv(paste(temp_path,"_summary_cnt.csv",sep=''), col_types = list("c", "i"))
  summary_counts <- rbind(summary_counts, c("Total number of pathways (after minCellCount)", sum(file_noyear$freq)))
  
  for (y in unique(file_withyear$index_year)) {
    summary_counts <- rbind(summary_counts, c(paste0("Number of pathways (after minCellCount) in ", y), sum(file_withyear$freq[file_withyear$index_year == y])))
  }
  
  write.table(summary_counts,file=paste(path,"_summary_cnt.csv",sep=''), sep = ",", row.names = FALSE, col.names = TRUE)
  
  write.csv(file_noyear, paste(path,"_file_noyear.csv",sep=''), row.names = FALSE)
  write.csv(file_withyear, paste(path,"_file_withyear.csv",sep=''), row.names = FALSE)
  
  ParallelLogger::logInfo("saveTreatmentSequence done")
}

# Input:
# data Dataframe with event cohorts of the target cohort in different columns.
# databaseName Name of the database that will appear in the results.
# eventCohortIds IDs to refer to event cohorts.
# studyName Name for the study corresponding to the current settings.
# outputFolder Path to the output folder.
# path Path to the output folder including data specific file name.
# groupCombinations Select to group all non-fixed combinations in one category 'other’ in the sunburst plot.
# addNoPaths Select to include untreated persons without treatment pathway in the sunburst plot.
# maxPathLength Maximum number of steps included in treatment pathway (max 5).
# createInput Whether the input for sunburst plot should be created.
# createPlot Whether the html for sunburst plot should be created.
outputSunburstPlot <- function(data, databaseName, eventCohortIds, studyName, outputFolder, path, addNoPaths, maxPathLength) {
  
  cohorts <- readr::read_csv(paste(outputFolder, "/cohort.csv",sep=''), col_types = list("i", "c", "c", "c"))
  outcomes <- c(cohorts$cohortName[cohorts$cohortId %in% eventCohortIds], "Other")
  
  if (is.null(data$index_year)) {
    # For file_noyear compute once
    data <- inputSunburstPlot(data, path, addNoPaths, index_year = 'all')
    
    createSunburstPlot(data, outcomes, folder = outputFolder, file_name = paste0(studyName, "/", databaseName, "_", studyName,"_all"), shiny = TRUE)
    
    # Create legend once
    createLegend(studyName, outputFolder, databaseName)
      
  } else {
    # For file_withyear compute per year
    years <- unlist(unique(data[,"index_year"]))
    
    for (y in years) {
      subset_data <- data[index_year == as.character(y),]
      if (nrow(subset_data) != 0) {
        subset_data <- inputSunburstPlot(subset_data, path, addNoPaths, index_year = y)
        
        createSunburstPlot(subset_data, outcomes, folder = outputFolder, file_name = paste0(studyName, "/", databaseName, "_", studyName,"_", y), shiny = TRUE)
        
      } else {
        ParallelLogger::logInfo(warning(paste0("Subset of data is empty for study settings ", studyName, " in year ", y)))
      }
      
    }
  }
  
  ParallelLogger::logInfo("outputSunburstPlot done")
}


#' Function to create sunburst plot from CSV file.
#'
#' @param data A data frame containing two columns: 1) column "path" should specify the event cohorts separated by dashes - (combinations can be indicated using &) and 2) column "freq" should specify how often that (unique) path occurs.
#' @param outcomes Character vector containing all event cohorts.
#' @param folder Root folder to store the results.
#' @param file_name File name for the results.
#' @param shiny Set to TRUE if HTML file is generated for shiny application, FALSE will generate a standalone HTML with title and legend.
#' @param title Optional if shiny = FALSE: add descriptive title in sunburst plot for standalone HTML.
#' 
#' @export
createSunburstPlot <- function(data, outcomes = NULL, folder = NULL, file_name = NULL, shiny = FALSE, title = ""){
  
  # Check inputs
  if(!("path" %in% colnames(data))) {
    ParallelLogger::logWarn("Column 'path' is missing from input data to create sunburst plot.")
  }
  
  if(!("freq" %in% colnames(data))) {
    ParallelLogger::logWarn("Column 'freq' is missing from input data to create sunburst plot.")
  }
  
  if (is.null(outcomes)) {
    outcomes <- unique(unlist(strsplit(data$path, split="-", fixed=TRUE)))
  }
  
  if (is.null(folder)) {
    folder <- getwd()
  }
  
  if (shiny == FALSE) {
    folder <- paste0(folder, "/plot")
  }
  
  if (!file.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  if (is.null(file_name)) {
    file_name <- "sunburst"
  }
  
  # if (maxPathLength > 5) {
  #   stop(paste0("MaxPathLength exceeds 5, currently not supported in buildHierarchy function."))
  # }
  
  # Load CSV file and convert to JSON
  json <- transformCSVtoJSON(data, outcomes, folder, file_name)
  
  # Load template HTML file
  if (shiny == TRUE) {
    html <- paste(readLines(paste0(system.file(package = "TreatmentPatterns"), "/shiny/sunburst/sunburst_shiny.html")), collapse="\n")
  } else {
    html <- paste(readLines(paste0(system.file(package = "TreatmentPatterns"), "/shiny/sunburst/sunburst_standalone.html")), collapse="\n")
    
    # Replace @name
    html <- sub("@name", title, html)
    
    # Add supporting html files in folder
    
    addSunburstFiles(filesLocation = folder)
  }
 
  # Replace @insert_data
  html <- sub("@insert_data", json, html)
  
  # Save HTML file
  write.table(html, 
              file = paste0(folder, "/", file_name,"_plot.html"), 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
}


# Help function to create transform data to correct format for sunburst plot.
inputSunburstPlot <- function(data, path, addNoPaths, index_year) {
  
  layers <- as.vector(colnames(data)[!grepl("index_year|freq", colnames(data))])
  
  transformed_file <- apply(data[,..layers],1, paste, collapse = "-")
  transformed_file <- stringr::str_replace_all(transformed_file, "-NA", "")
  transformed_file <- paste0(transformed_file, "-End")
  transformed_file <- data.frame(path=transformed_file, freq=data$freq, stringsAsFactors = FALSE)
  
  if (addNoPaths) {
    summary_counts <- readr::read_csv(paste(path,"_summary_cnt.csv",sep=''), col_types = list("c","i"))
    
    if (index_year == "all") {
      noPath <- as.integer(summary_counts[summary_counts$index_year == "Number of persons in target cohort NA", "N"]) - sum(transformed_file$freq)
    } else {
      noPath <- as.integer(summary_counts[summary_counts$index_year == paste0("Number of persons in target cohort ", index_year), "N"]) - sum(transformed_file$freq)
    }
    
    transformed_file <- rbind(transformed_file, c("End", noPath))
  }
  
  transformed_file$path <- as.factor(transformed_file$path)
  transformed_file$freq <- as.integer(transformed_file$freq)
  transformed_file <- transformed_file[order(-transformed_file$freq, transformed_file$path),]
  
  write.table(transformed_file, file=paste(path,"_inputsunburst_", index_year, ".csv",sep=''), sep = ",", row.names = FALSE)
  
  return(transformed_file)
}

# Help function to transform data in csv format to required JSON format for HTML.
transformCSVtoJSON <- function(data, outcomes, folder, file_name) {
 
  # Add bitwise numbers to define combination treatments
  bitwiseNumbers <- sapply(1:length(outcomes), function(o) {2^(o-1)})
  linking <- data.frame(outcomes,bitwiseNumbers)
  
  # Generate lookup file
  series <- sapply(1:nrow(linking), function (row) {
    paste0('{ "key": "', linking$bitwiseNumbers[row] ,'", "value": "', linking$outcomes[row],'"}')
  })
  
  series <- c(series, '{ "key": "End", "value": "End"}')
  lookup <- paste0("[", paste(series, collapse = ","), "]")
  
  # Order names from longest to shortest to adjust in the right order
  linking <- linking[order(-sapply(linking$outcomes, function(x) stringr::str_length(x))),]
  
  # Apply linking
  # Change all outcomes to bitwise number
  updated_path <- sapply(data$path, function(p) {
    stringi::stri_replace_all_fixed(p, replacement = as.character(linking$bitwiseNumbers), pattern = as.character(linking$outcomes), vectorize = FALSE)
  })
  
  # Sum the bitwise numbers of combinations (indicated by +)
  updated_path <- sapply(updated_path, function(p) {
    while(!is.na(stringr::str_extract(p, "[[:digit:]]+[+][[:digit:]]+"))) {
      pattern <- stringr::str_extract(p, "[[:digit:]]+[+][[:digit:]]+")
      
      p <- sub("[[:digit:]]+[+][[:digit:]]+", eval(parse(text=pattern)), p)
    }
    return(p)
  })
  
  transformed_csv <- cbind(oath = updated_path, freq = data$freq)
  transformed_json <- buildHierarchy(transformed_csv) 
  
  result <- paste0("{ \"data\" : ", transformed_json, ", \"lookup\" : ", lookup, "}")
  
  file <- file(paste0(folder, "/", file_name ,"_input.txt"))
  writeLines(result, file)
  close(file)
  
  return(result) # TODO: check this, or return writeLines(result)
}

# Help function to create hierarchical data structure.
buildHierarchy <- function(csv) {

  root = list("name" = "root", "children" = list())
  
  # Create nested structure of lists 
  for (i in 1:nrow(csv)) {
    sequence = csv[i,1]
    size = csv[i,2]
    
    parts = unlist(stringr::str_split(sequence,pattern="-"))
    
    currentNode = root
    
    for (j in 1:length(parts)) {
      children = currentNode[["children"]]
      nodeName = parts[j]
      
      if (j < length(parts)) {
        # Not yet at the end of the sequence; move down the tree
        foundChild = FALSE
        
        if (length(children) != 0) {
          for (k in 1:length(children)) {
            if (children[[k]]$name == nodeName) {
              childNode = children[[k]]
              foundChild = TRUE
              break
            }
          }
        }
        
        # If we don't already have a child node for this branch, create it
        if (!foundChild) {
          childNode = list("name" = nodeName, "children" = list())
          children[[nodeName]] <- childNode
          
          # Add to main root
          if (j == 1) {
            root[['children']] <- children
          } else if (j == 2) {
            root[['children']][[parts[1]]][['children']] <- children
          } else if (j == 3) {
            root[['children']][[parts[1]]][['children']][[parts[2]]][['children']] <- children
          } else if (j == 4) {
            root[['children']][[parts[1]]][['children']][[parts[2]]][['children']][[parts[3]]][['children']] <- children
          } else if (j == 5) {
            root[['children']][[parts[1]]][['children']][[parts[2]]][['children']][[parts[3]]][['children']][[parts[4]]][['children']] <- children
          }
        }
        currentNode = childNode
      } else {
        # Reached the end of the sequence; create a leaf node
        childNode = list("name" = nodeName, "size" = size)
        children[[nodeName]] <- childNode
        
        # Add to main root
        if (j == 1) {
          root[['children']] <- children
        } else if (j == 2) {
          root[['children']][[parts[1]]][['children']] <- children
        } else if (j == 3) {
          root[['children']][[parts[1]]][['children']][[parts[2]]][['children']] <- children
        } else if (j == 4) {
          root[['children']][[parts[1]]][['children']][[parts[2]]][['children']][[parts[3]]][['children']] <- children
        } else if (j == 5) {
          root[['children']][[parts[1]]][['children']][[parts[2]]][['children']][[parts[3]]][['children']][[parts[4]]][['children']] <- children
        }
      }
    }
  }
  
  # Remove list names
  root <- suppressWarnings(stripname(root, "children"))
  
  # Convert nested list structure to json
  json <- rjson::toJSON(root)
  
  return(json)
}


createLegend <- function(studyName, outputFolder, databaseName) {
  # Load template HTML file
  html <- paste(readLines(paste0(system.file(package = "TreatmentPatterns"), "/shiny/sunburst/legend.html")), collapse="\n")
  
  # Replace @insert_data
  input_plot <- readLines( paste0(outputFolder, "/", studyName, "/", databaseName, "_", studyName,"_all_input.txt"))
  html <- sub("@insert_data", input_plot, html)

  # Save HTML file as sunburst_@studyName
  write.table(html, 
              file=paste0(outputFolder, "/", studyName, "/legend.html"), 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
}

# Input:
# data Dataframe with event cohorts of the target cohort in different columns.
# outputFolder Path to the output folder.
# databaseName Name of the database that will appear in the results.
# studyName Name for the study corresponding to the current settings.
# groupCombinations Select to group all non-fixed combinations in one category 'other’ in the sunburst plot.
createSankeyDiagram <- function(data, outputFolder, databaseName, studyName, groupCombinations) {
  
  # Group non-fixed combinations in one group according to groupCobinations
  data <- groupInfrequentCombinations(data, groupCombinations)
  
  # Define stop treatment
  data[is.na(data)] <- 'Stopped'
  
  # Sankey diagram for first three treatment layers
  data$event_cohort_name1 <- paste0("1. ",data$event_cohort_name1)
  data$event_cohort_name2 <- paste0("2. ",data$event_cohort_name2)
  data$event_cohort_name3 <- paste0("3. ",data$event_cohort_name3)
  
  results1 <- data %>% 
    dplyr::group_by(event_cohort_name1,event_cohort_name2) %>% 
    dplyr::summarise(freq = sum(freq))
  
  results2 <- data %>% 
    dplyr::group_by(event_cohort_name2,event_cohort_name3) %>% 
    dplyr::summarise(freq = sum(freq))
  
  # Format in prep for sankey diagram
  colnames(results1) <- c("source", "target", "value")
  colnames(results2) <- c("source", "target", "value")
  links <- as.data.frame(rbind(results1,results2))
  
  # Draw sankey network
  plot <- googleVis::gvisSankey(links, from = "source", to = "target", weight = "value", chartid = 1)
  
  write.table(plot$html$chart, 
              file=paste0(outputFolder, "/", studyName, "/", "sankeydiagram_", databaseName, "_", studyName,"_all.html"), 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  ParallelLogger::logInfo("createSankeyDiagram done")
}

# Help function to group combinations
groupInfrequentCombinations <- function(data, groupCombinations) {
  
  data <- as.data.frame(data)
  
  # Find all non-fixed combinations occurring
  findCombinations <- apply(data, 2, function(x) grepl("+", x, fixed = TRUE))
  
  # Group all non-fixed combinations in one group if TRUE
  if (groupCombinations == "TRUE") {
    data[findCombinations] <- "Other"
  } else {
    # Otherwise: group infrequent treatments below groupCombinations as "other"
    combinations <- as.matrix(data)[findCombinations == TRUE]
    num_columns <- sum(grepl("cohort_name", colnames(data)))
    freqCombinations <- matrix(rep(data$freq, times = num_columns), ncol = num_columns)[findCombinations == TRUE]
    
    summaryCombinations <- data.table(combination = combinations, freq = freqCombinations)
    
    if (nrow(summaryCombinations) > 0) {
      summaryCombinations <- summaryCombinations[,.(freq=sum(freq)), by=combination][order(-freq)]
      
      summarizeCombinations <- summaryCombinations$combination[summaryCombinations$freq <= groupCombinations]
      selectedCombinations <- apply(data, 2, function(x) x %in% summarizeCombinations)
      data[selectedCombinations] <- "Other"
    }
  }
  
  return(as.data.table(data))
}
