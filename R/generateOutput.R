#' generateOutput
#'
#' @param saveSettings
#'   Settings object as created by createSaveSettings(). 
#'
#' @import readr
#' @import ParallelLogger
#' @import OhdsiSharing
#'
#' @returns some stuff
#'
#' @export
generateOutput <- function(saveSettings) {
  if (!class(saveSettings) %in% c("saveSettings")) {
    stop("Incorrect class for saveSettings")
  }
  
  pathwaySettings <- data.frame(readr::read_csv(
    file.path(
      saveSettings$outputFolder,
      "settings",
      "pathway_settings.csv"
    ),
    col_types = readr::cols()
  ))
  
  settings <-
    colnames(pathwaySettings)[grepl("analysis", colnames(pathwaySettings))]
  
  for (s in settings) {
    studyName <- pathwaySettings[pathwaySettings$param == "studyName", s]
    ParallelLogger::logInfo(print(paste0("Creating output: ", studyName)))
    
    eventCohortIds <-
      pathwaySettings[pathwaySettings$param == "eventCohortIds", s]
    eventCohortIds <-
      unlist(strsplit(eventCohortIds, split = c(";|,")))
    
    minCellCount <-
      as.integer(pathwaySettings[pathwaySettings$param == "minCellCount", s])
    minCellMethod <-
      pathwaySettings[pathwaySettings$param == "minCellMethod", s]
    groupCombinations <-
      pathwaySettings[pathwaySettings$param == "groupCombinations", s]
    addNoPaths <-
      pathwaySettings[pathwaySettings$param == "addNoPaths", s]
    
    outputFolders <-
      file.path(saveSettings$outputFolder, studyName)
    if (!file.exists(outputFolders)) {
      dir.create(outputFolders, recursive = TRUE)
    }
    
    treatmentPathways <- getPathways(
      outputFolder = saveSettings$outputFolder,
      tempFolder = saveSettings$tempFolder,
      databaseName = saveSettings$databaseName,
      studyName = studyName,
      minCellCount = minCellCount
    )
    
    if (!is.null(treatmentPathways)) {
      outputTreatedPatients(
        data = treatmentPathways[[1]],
        eventCohortIds = eventCohortIds,
        groupCombinations = TRUE,
        outputFolder = saveSettings$outputFolder,
        outputFile = file.path(
          outputFolders,
          paste0(
            saveSettings$databaseName,
            "_",
            studyName,
            "_percentage_groups_treated_noyear.csv"
          )
        )
      )
      outputTreatedPatients(
        data = treatmentPathways[[2]],
        eventCohortIds = eventCohortIds,
        groupCombinations = TRUE,
        outputFolder = saveSettings$outputFolder,
        outputFile = file.path(
          outputFolders,
          paste0(
            saveSettings$databaseName,
            "_",
            studyName,
            "_percentage_groups_treated_withyear.csv"
          )
        )
      )
      
      outputDurationEras(
        outputFolder = saveSettings$outputFolder,
        tempFolder = saveSettings$tempFolder,
        databaseName = saveSettings$databaseName,
        studyName = studyName,
        eventCohortIds = eventCohortIds,
        groupCombinations = TRUE,
        minCellCount = minCellCount
      )
      
      treatmentPathways <-
        doMinCellCount(
          file_noyear = treatmentPathways[[1]],
          file_withyear = treatmentPathways[[2]],
          outputFolder = saveSettings$outputFolder,
          tempFolder = saveSettings$tempFolder,
          databaseName = saveSettings$databaseName,
          studyName = studyName,
          groupCombinations = groupCombinations,
          minCellCount = minCellCount,
          minCellMethod = minCellMethod
        )
      
      
      preprocessSunburstData(
        data = treatmentPathways[[1]],
        outputFolder = saveSettings$outputFolder,
        databaseName = saveSettings$databaseName,
        studyName = studyName,
        eventCohortIds = eventCohortIds,
        addNoPaths = addNoPaths
      )
      
      preprocessSunburstData(
        data = treatmentPathways[[2]],
        outputFolder = saveSettings$outputFolder,
        databaseName = saveSettings$databaseName,
        studyName = studyName,
        eventCohortIds = eventCohortIds,
        addNoPaths = addNoPaths
      )
    }
  }
  
  zipName <- file.path(
    saveSettings$rootFolder,
    paste0(saveSettings$databaseName, ".zip")
  )
  
  OhdsiSharing::compressFolder(file.path(saveSettings$outputFolder), zipName)
  # zip::zipr(list.files(saveSettings$outputFolder, full.names = TRUE), zipName)
  
  ParallelLogger::logInfo("generateOutput done.")
}

#' getPathways
#'
#' Get treatment pathways for generating aggregate output.
#'
#' @param outputFolder
#'     Path to the output folder.
#' @param tempFolder
#'     Path to the temp folder.
#' @param databaseName
#'     Name of the database that will appear in the results.
#' @param studyName
#'     Name for the study corresponding to the current settings.
#' @param minCellCount
#'     Minimum number of persons with a specific treatment pathway for the
#'     pathway to be included in analysis.
#'
#' @importFrom data.table data.table
#' @import readr
#' @import ParallelLogger
#'
#' @return List with two dataframes.
getPathways <- function(
    outputFolder,
    tempFolder,
    databaseName,
    studyName,
    minCellCount) {
  # Try to read in paths from constructPathways.R for studyName
  file <- try(
    {
      data.table::data.table(readr::read_csv(
        file.path(
          tempFolder,
          studyName,
          paste0(databaseName, "_", studyName, "_paths.csv")
        ),
        col_types = readr::cols()
      ))
    },
    silent = TRUE
  )
  
  if ("try-error" %in% class(file)) {
    ParallelLogger::logInfo(warning(paste0(
      "Data is empty for study settings ", studyName
    )))
    return(NULL)
  }
  
  # Summarize which non-fixed combinations occurring
  findCombinations <-
    apply(file, 2, function(x) {
      grepl("+", x, fixed = TRUE)
    })
  
  combinations <- as.matrix(file)[findCombinations == TRUE]
  num_columns <- sum(grepl("event_cohort_name", colnames(file)))
  freqCombinations <- matrix(
    rep(file$freq, times = num_columns),
    ncol = num_columns
  )[findCombinations == TRUE]
  
  summaryCombinations <- data.table::data.table(
    combination = combinations,
    freq = freqCombinations
  )
  
  summaryCombinations <-
    summaryCombinations[, .(freq = sum(freq)), by = combination][order(-freq)]
  
  summaryCombinations <- summaryCombinations[freq >= minCellCount, ]
  write.csv(
    summaryCombinations,
    file = file.path(
      outputFolder,
      studyName,
      paste0(databaseName, "_", studyName, "_combinations.csv")
    ),
    row.names = FALSE
  )
  
  # Group the resulting treatment paths
  layers <-
    as.vector(colnames(file)[!grepl("index_year|freq", colnames(file))])
  file_noyear <- file[, .(freq = sum(freq)), by = layers]
  file_withyear <-
    file[, .(freq = sum(freq)), by = c(layers, "index_year")]
  
  return(list(file_noyear, file_withyear))
}


#' outputTreatedPatients
#'
#' @param data
#'     Dataframe with event cohorts of the target cohort in different columns.
#' @param eventCohortIds
#'     IDs to refer to event cohorts.
#' @param groupCombinations
#'     Select to group all non-fixed combinations in one category "other" in
#'     the sunburst plot.
#' @param outputFolder
#'     Path to the output folder.
#' @param outputFile
#'     Name of output file.
#'
#' @import ParallelLogger
#' @importFrom data.table rbindlist
#'
#' @returns NULL
outputTreatedPatients <- function(
    data,
    eventCohortIds,
    groupCombinations,
    outputFolder,
    outputFile) {
  if (is.null(data$index_year)) {
    # For file_noyear compute once
    result <- percentageGroupTreated(
      data,
      eventCohortIds,
      groupCombinations,
      outputFolder
    )
  } else {
    # For file_withyear compute per year
    years <- unlist(unique(data[, "index_year"]))
    
    results <- lapply(years, function(y) {
      subset_data <- data[index_year == as.character(y), ]
      if (nrow(subset_data) != 0) {
        subset_result <-
          cbind(
            y,
            percentageGroupTreated(
              subset_data,
              eventCohortIds,
              groupCombinations,
              outputFolder
            )
          )
      } else {
        ParallelLogger::logInfo(warning(
          paste0("Subset of data is empty for study settings in year ", y)
        ))
        subset_result <- NULL
      }
      return(subset_result)
    })
    result <- data.table::rbindlist(results)
    result$y <- as.character(result$y)
  }
  write.csv(result, file = outputFile, row.names = FALSE)
  ParallelLogger::logInfo("outputTreatedPatients done")
}


#' percentageGroupTreated
#'
#' Help function to compute percentage of treated patient with certain event
#' cohorts for outputTreatedPatients.
#'
#' @param data
#'     Data
#' @param eventCohortIds
#'     Event cohort IDs
#' @param groupCombinations
#'     Group combinations
#' @param outputFolder
#'     Output folder
#'
#' @import readr
#'
#' @returns result object
percentageGroupTreated <- function(
    data,
    eventCohortIds,
    groupCombinations,
    outputFolder) {
  layers <- as.vector(colnames(data)[
    !grepl("index_year|freq", colnames(data))
  ])
  
  cohorts <- readr::read_csv(
    file.path(outputFolder, "settings", "cohorts_to_create.csv"),
    col_types = list("i", "c", "c")
  )
  outcomes <- c(
    cohorts$cohortName[cohorts$cohortId %in% eventCohortIds],
    "Other"
  )
  
  # Group non-fixed combinations in one group according to groupCobinations
  data <- groupInfrequentCombinations(data, groupCombinations)
  
  percentGroupLayer <- sapply(
    X = layers,
    FUN = function(l) {
      percentGroup <- sapply(
        X = outcomes,
        FUN = function(g) {
          sumGroup <- sum(data$freq[data[, ..l] == g], na.rm = TRUE)
          sumAllNotNA <- sum(data$freq[!is.na(data[, ..l])])
          return(sumGroup * 100.0 / sumAllNotNA)
        }
      )
    }
  )
  
  # Add outcome names
  result <- data.frame(
    outcomes,
    percentGroupLayer,
    stringsAsFactors = FALSE
  )
  
  colnames(result) <- c("outcomes", layers)
  rownames(result) <- NULL
  
  paths_all <- sum(data$freq)
  
  result$ALL_LAYERS <- sapply(
    X = outcomes,
    FUN = function(o) {
      paths_with_outcome <- sum(sapply(
        X = 1:nrow(data), FUN = function(r) {
          ifelse(o %in% data[r, ], data[r, freq], 0)
        }
      ))
      return(paths_with_outcome * 100.0 / paths_all)
    }
  )
  
  # Add rows for total, fixed combinations, all combinations
  if (length(layers) == 1) {
    result <- rbind(
      result,
      c(
        "Fixed combinations",
        sum(result[grepl("\\&", result$outcomes), layers]),
        NA
      ),
      c(
        "All combinations",
        sum(result[grepl("Other|\\+|\\&", result$outcomes), layers]),
        NA
      ),
      c(
        "Monotherapy",
        sum(result[!grepl("Other|\\+|\\&", result$outcomes), layers]),
        NA
      )
    )
  } else {
    result <- rbind(
      result,
      c(
        "Fixed combinations",
        colSums(result[grepl("\\&", result$outcomes), layers]),
        NA
      ),
      c(
        "All combinations",
        colSums(result[grepl("Other|\\+|\\&", result$outcomes), layers]),
        NA
      ),
      c(
        "Monotherapy",
        colSums(result[!grepl("Other|\\+|\\&", result$outcomes), layers]),
        NA
      )
    )
  }
  
  result$ALL_LAYERS[result$outcomes == "Fixed combinations"] <- sum(
    sapply(
      X = 1:nrow(data),
      FUN = function(r) {
        ifelse(any(
          grepl("\\&", data[r, ])
        ), data[r, freq], 0)
      }
    )
  ) * 100.0 / paths_all
  
  result$ALL_LAYERS[result$outcomes == "All combinations"] <- sum(
    sapply(
      X = 1:nrow(data),
      FUN = function(r) {
        ifelse(any(
          grepl("Other|\\+|\\&", data[r, ])
        ), data[r, freq], 0)
      }
    )
  ) * 100.0 / paths_all
  
  result$ALL_LAYERS[result$outcomes == "Monotherapy"] <- sum(
    sapply(1:nrow(data), function(r) {
      ifelse(any(
        !grepl("Other|\\+|\\&", data[r, ])
      ), data[r, freq], 0)
    })
  ) * 100.0 / paths_all
  return(result)
}


#' outputDurationEras
#'
#' @param outputFolder
#'     Path to the output folder.
#' @param tempFolder
#'     Path to the temp folder.
#' @param databaseName
#'     Name of the database that will appear in the results.
#' @param studyName
#'     Name for the study corresponding to the current settings.
#' @param eventCohortIds
#'     IDs to refer to event cohorts.
#' @param groupCombinations
#'     Select to group all non-fixed combinations in one category "other" in
#'     the sunburst plot.
#' @param minCellCount
#'     Minimum number of persons with a specific treatment pathway for the
#'     pathway to be included in analysis.
#'
#' @importFrom data.table data.table
#' @import readr
#'
#' @returns NULL
outputDurationEras <- function(
    outputFolder,
    tempFolder,
    databaseName,
    studyName,
    eventCohortIds,
    groupCombinations,
    minCellCount) {
  
  # Try to read in treatment history from constructPathways.R for studyName
  file <- data.table::data.table(readr::read_csv(
    file.path(
      tempFolder,
      studyName,
      paste0(databaseName, "_", studyName, "_event_seq_processed.csv")
    ),
    col_types = list("c", "i", "i", "D", "D", "i", "i", "c")
  ))
  
  # Remove unnessary columns
  columns <- c("duration_era", "event_seq", "event_cohort_name")
  file <- file[, ..columns]
  file$duration_era <- as.numeric(file$duration_era)
  
  # Group non-fixed combinations in one group according to groupCobinations
  file <- groupInfrequentCombinations(file, groupCombinations)
  
  result <- file[, .(AVG_DURATION = round(mean(duration_era), 3), COUNT = .N),
                 by = c("event_cohort_name", "event_seq")
  ][
    order(event_cohort_name, event_seq)
  ]
  
  # Add column for total treated, fixed combinations, all combinations
  file$fixed_combinations[grepl("\\&", file$event_cohort_name)] <- 1
  file$all_combinations[grepl("Other|\\+|\\&", file$event_cohort_name)] <- 1
  file$monotherapy[!grepl("Other|\\+|\\&", file$event_cohort_name)] <- 1
  
  # Duration average per layer
  result_total_concept <- file[, .(
    event_cohort_name = "Total treated",
    AVG_DURATION = round(mean(duration_era), 2),
    COUNT = .N
  ), by = c("event_seq")]
  
  result_fixed_combinations <- file[, .(
    event_cohort_name = "Fixed combinations",
    AVG_DURATION = round(mean(duration_era), 2),
    COUNT = .N
  ), by = c("event_seq", "fixed_combinations")]
  
  result_fixed_combinations <- result_fixed_combinations[
    !is.na(fixed_combinations),
  ]
  
  result_fixed_combinations$fixed_combinations <- NULL
  
  result_all_combinations <- file[, .(
    event_cohort_name = "All combinations",
    AVG_DURATION = round(mean(duration_era), 2),
    COUNT = .N
  ), by = c("event_seq", "all_combinations")]
  
  result_all_combinations <- result_all_combinations[
    !is.na(all_combinations),
  ]
  
  result_all_combinations$all_combinations <- NULL
  
  result_monotherapy <- file[, .(
    event_cohort_name = "Monotherapy",
    AVG_DURATION = round(mean(duration_era), 2),
    COUNT = .N
  ), by = c("event_seq", "monotherapy")]
  
  result_monotherapy <- result_monotherapy[!is.na(monotherapy), ]
  result_monotherapy$monotherapy <- NULL
  
  # Duration average all layers
  result_total_seq <- file[, .(
    event_seq = "Overall",
    AVG_DURATION = round(mean(duration_era), 2),
    COUNT = .N
  ), by = c("event_cohort_name")]
  
  results_total_treated <- file[, .(
    event_cohort_name = "Total treated",
    event_seq = "Overall",
    AVG_DURATION = round(mean(duration_era), 2),
    COUNT = .N
  )]
  
  results_total_fixed <- file[fixed_combinations == 1, .(
    event_cohort_name = "Fixed combinations",
    event_seq = "Overall",
    AVG_DURATION = round(mean(duration_era), 2),
    COUNT = .N
  )]
  
  results_total_mono <-
    file[all_combinations == 1, .(
      event_cohort_name = "All combinations",
      event_seq = "Overall",
      AVG_DURATION = round(mean(duration_era), 2),
      COUNT = .N
    )]
  
  results_total_allcombi <- file[monotherapy == 1, .(
    event_cohort_name = "Monotherapy",
    event_seq = "Overall",
    AVG_DURATION = round(mean(duration_era), 2),
    COUNT = .N
  )]
  
  results <- rbind(
    result,
    result_total_concept,
    result_fixed_combinations,
    result_all_combinations,
    result_monotherapy,
    result_total_seq,
    results_total_treated,
    results_total_fixed,
    results_total_mono,
    results_total_allcombi
  )
  
  # Add missing groups
  cohorts <- readr::read_csv(
    file.path(outputFolder, "settings", "cohorts_to_create.csv"),
    col_types = list("i", "c", "c")
  )
  
  outcomes <- c(
    cohorts$cohortName[cohorts$cohortId %in% eventCohortIds],
    "Other"
  )
  
  for (o in outcomes[!(outcomes %in% results$event_cohort_name)]) {
    results <- rbind(results, list(o, "Overall", 0.0, 0))
  }
  
  # Remove durations computed using less than minCellCount observations
  results[COUNT < minCellCount, c("AVG_DURATION", "COUNT")] <- NA
  write.csv(
    results,
    file.path(
      outputFolder,
      studyName,
      paste0(databaseName, "_", studyName, "_duration.csv")
    ),
    row.names = FALSE
  )
  
  ParallelLogger::logInfo("outputDurationEras done")
}


#' doMinCellCount
#'
#' Apply minCellCount to generate censored treatment pathways to share and for
#' generating detailed output.
#'
#' @param file_noyear
#'     Dataframe with aggregated treatment pathways of the target cohort in
#'     different rows (unique pathways, with frequency).
#' @param file_withyear
#'     Idem to file_noyear, but split over index_year.
#' @param outputFolder
#'     Path to the output folder.
#' @param tempFolder
#'     Path to the temp folder.
#' @param databaseName
#'     Name of the database that will appear in the results.
#' @param studyName
#'     Name for the study corresponding to the current settings.
#' @param groupCombinations
#'     Select to group all non-fixed combinations in one category "other" in
#'     the sunburst plot.
#' @param minCellCount
#'     Minimum number of persons with a specific treatment pathway for the
#'     pathway to be included in analysis.
#' @param minCellMethod
#'     Select to completely remove / sequentially adjust (by removing last step
#'     as often as necessary) treatment pathways below minCellCount.
#' @return List
#'     List of file_noyear and file_withyear
#'
#' @import readr
#' @import ParallelLogger
#'
#' @returns List
doMinCellCount <- function(
    file_noyear,
    file_withyear,
    outputFolder,
    tempFolder,
    databaseName,
    studyName,
    groupCombinations,
    minCellCount,
    minCellMethod) {
  # Group non-fixed combinations in one group according to groupCobinations
  file_noyear <- groupInfrequentCombinations(
    file_noyear,
    groupCombinations
  )
  
  file_withyear <- groupInfrequentCombinations(
    file_withyear,
    groupCombinations
  )
  
  layers <- as.vector(colnames(file_noyear)[
    !grepl("index_year|freq", colnames(file_noyear))
  ])
  
  # Calculate percentage treated before minCellCount
  summary_counts <- readr::read_csv(
    file.path(
      tempFolder,
      studyName,
      paste0(databaseName, "_", studyName, "_summary_cnt.csv")
    ),
    col_types = list("c", "d")
  )
  
  sumAll <- as.integer(summary_counts[
    summary_counts$index_year == "Number of persons in target cohort NA", "N"
  ])
  
  for (l in layers) {
    sumAllNotNA <- sum(file_noyear$freq[!is.na(file_noyear[, ..l])])
    percAllNotNA <- sumAllNotNA * 100.0 / sumAll
    summary_counts <-
      rbind(summary_counts, c(
        paste0("Percentage treated (before minCellCount) in ", l),
        percAllNotNA
      ))
  }
  
  # Apply minCellCount by adjusting to other most similar path (removing last
  # treatment in path) or else remove complete path
  if (minCellMethod == "Adjust") {
    col <- ncol(file_noyear) - 1
    while (sum(file_noyear$freq < minCellCount) > 0 & col != 0) {
      ParallelLogger::logInfo(
        paste(
          "Change col ",
          col,
          " to NA for ",
          sum(file_noyear$freq < minCellCount),
          " paths with too low frequency (without year)"
        )
      )
      
      file_noyear[freq < minCellCount, col] <- NA
      file_noyear <- file_noyear[, .(freq = sum(freq)), by = layers]
      
      col <- col - 1
    }
    
    col <- ncol(file_withyear) - 2
    while (sum(file_withyear$freq < minCellCount) > 0 & col != 0) {
      ParallelLogger::logInfo(
        paste(
          "Change col ",
          col,
          " to NA for ",
          sum(file_withyear$freq < minCellCount),
          " paths with too low frequency (with year)"
        )
      )
      
      file_withyear[freq < minCellCount, col] <- NA
      file_withyear <- file_withyear[
        , .(freq = sum(freq)),
        by = c(layers, "index_year")
      ]
      
      col <- col - 1
    }
    
    # If path becomes completely NA -> add to "Other" group to distinguish
    # from non-treated
    file_noyear$event_cohort_name1[is.na(file_noyear$event_cohort_name1)] <-
      "Other"
    file_noyear <- file_noyear[, .(freq = sum(freq)), by = layers]
    
    file_withyear$event_cohort_name1[is.na(file_withyear$event_cohort_name1)] <-
      "Other"
    file_withyear <-
      file_withyear[, .(freq = sum(freq)), by = c(layers, "index_year")]
  }
  
  ParallelLogger::logInfo(paste(
    "Remove ",
    sum(file_noyear$freq < minCellCount),
    " paths with too low frequency (without year)"
  ))
  
  file_noyear <- file_noyear[freq >= minCellCount, ]
  
  ParallelLogger::logInfo(paste(
    "Remove ",
    sum(file_withyear$freq < minCellCount),
    " paths with too low frequency (with year)"
  ))
  
  file_withyear <- file_withyear[freq >= minCellCount, ]
  
  # Add updated counts after minCellCount
  summary_counts <- rbind(
    summary_counts,
    c("Number of pathways (after minCellCount) in NA", sum(file_noyear$freq))
  )
  
  for (y in unique(file_withyear$index_year)) {
    summary_counts <- rbind(
      summary_counts,
      c(
        paste0("Number of pathways (after minCellCount) in ", y),
        sum(file_withyear$freq[file_withyear$index_year == y])
      )
    )
  }
  
  write.table(
    summary_counts,
    file = file.path(
      outputFolder,
      studyName,
      paste0(databaseName, "_", studyName, "_summary_cnt.csv")
    ),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )
  
  write.csv(
    file_noyear,
    file.path(
      outputFolder,
      studyName,
      paste0(databaseName, "_", studyName, "_file_noyear.csv")
    ),
    row.names = FALSE
  )
  
  write.csv(
    file_withyear,
    file.path(
      outputFolder,
      studyName,
      paste0(databaseName, "_", studyName, "_file_withyear.csv")
    ),
    row.names = FALSE
  )
  
  ParallelLogger::logInfo("doMinCellCount done")
  return(list(file_noyear, file_withyear))
}


#' preprocessSunburstData
#'
#' Preprocesses the data for the sunburst either total or by year
#'
#' @param data
#'     Dataframe with event cohorts of the target cohort in different columns.
#' @param outputFolder
#'     Path to the output folder.
#' @param databaseName
#'     Name of the database that will appear in the results.
#' @param studyName
#'     Name for the study corresponding to the current settings.
#' @param eventCohortIds
#'     IDs to refer to event cohorts.
#' @param addNoPaths
#'     Select to include untreated persons without treatment pathway in the
#'     sunburst plot.
#'
#' @import readr
#' @import ParallelLogger
#'
#' @returns NULL
preprocessSunburstData <- function(
    data,
    outputFolder,
    databaseName,
    studyName,
    eventCohortIds,
    addNoPaths) {
  cohorts <- readr::read_csv(
    file.path(outputFolder, "settings", "cohorts_to_create.csv"),
    col_types = list("i", "c", "c")
  )
  
  outcomes <- c(
    cohorts$cohortName[cohorts$cohortId %in% eventCohortIds],
    "Other"
  )
  
  if (nrow(data) != 0) {
    if (is.null(data$index_year)) {
      # For file_noyear compute once
      data <- inputSunburstPlot(
        data,
        outputFolder,
        databaseName,
        studyName,
        addNoPaths,
        index_year = "all"
      )
    } else {
      # For file_withyear compute per year
      years <- unlist(unique(data[, "index_year"]))
      
      for (y in years) {
        subset_data <- data[index_year == as.character(y), ]
        if (nrow(subset_data) != 0) {
          subset_data <- inputSunburstPlot(
            subset_data,
            outputFolder,
            databaseName,
            studyName,
            addNoPaths,
            index_year = y
          )
        } else {
          ParallelLogger::logInfo(warning(
            paste0(
              "Subset of data is empty for study settings ",
              studyName,
              " in year ",
              y
            )
          ))
        }
      }
    }
    ParallelLogger::logInfo("preprocessSunburstData done")
  }
}


#' inputSunburstPlot
#'
#' Help function to create transform data to correct format for sunburst plot.
#'
#' @param data
#'     Data
#' @param outputFolder
#'     Output folder
#' @param databaseName
#'     Database name
#' @param studyName
#'     Study name
#' @param addNoPaths
#'     Add no paths
#' @param index_year
#'     Index year
#'
#' @import stringr
#' @import readr
#'
#' @returns transformed_file
inputSunburstPlot <- function(
    data,
    outputFolder,
    databaseName,
    studyName,
    addNoPaths,
    index_year) {
  layers <- as.vector(
    colnames(data)[!grepl("index_year|freq", colnames(data))]
  )
  
  transformed_file <- apply(
    X = data[, ..layers],
    MARGIN = 1,
    FUN = paste,
    collapse = "-"
  )
  
  transformed_file <- stringr::str_replace_all(
    transformed_file, "-NA", ""
  )
  
  transformed_file <- paste0(transformed_file, "-End")
  
  transformed_file <- data.frame(
    path = transformed_file,
    freq = data$freq,
    stringsAsFactors = FALSE
  )
  
  if (addNoPaths) {
    summary_counts <- readr::read_csv(
      file.path(
        outputFolder,
        studyName,
        paste0(databaseName, "_", studyName, "_summary_cnt.csv")
      ),
      col_types = list("c", "d")
    )
    
    if (index_year == "all") {
      noPath <- as.integer(summary_counts[
        summary_counts$index_year ==
          "Number of persons in target cohort NA", "N"
      ]) -
        sum(transformed_file$freq)
    } else {
      noPath <- as.integer(summary_counts[
        summary_counts$index_year ==
          paste0("Number of persons in target cohort ", index_year), "N"
      ]) -
        sum(transformed_file$freq)
    }
    transformed_file <- rbind(transformed_file, c("End", noPath))
  }
  transformed_file$path <- as.factor(transformed_file$path)
  transformed_file$freq <- as.integer(transformed_file$freq)
  transformed_file <-
    transformed_file[order(-transformed_file$freq, transformed_file$path), ]
  
  write.table(
    transformed_file,
    file = file.path(
      outputFolder,
      studyName,
      paste0(
        databaseName,
        "_",
        studyName,
        "_inputsunburst_",
        index_year,
        ".csv"
      )
    ),
    sep = ",",
    row.names = FALSE
  )
  return(transformed_file)
}


#' groupInfrequentCombinations
#'
#' Help function to group combinations
#'
#' @param data
#'     Data
#' @param groupCombinations
#'     Group combinations
#'
#' @importFrom data.table data.table as.data.table
#'
#' @returns data.table
#' 
#' @examples \dontrun{
#' source(system.file(
#'   package = "TreatmentPatterns",
#'   "testing", "testParamsOutput.R"))
#'   
#' TreatmentPatterns:::groupInfrequentCombinations(
#'   data = treatment_pathways[[1]],
#'   groupCombinations = groupCombinations)
#' }
groupInfrequentCombinations <- function(data, groupCombinations) {
  data <- as.data.frame(data)
  
  # Find all non-fixed combinations occurring
  findCombinations <- apply(
    X = data,
    MARGIN = 2,
    FUN = function(x) {
      grepl("+", x, fixed = TRUE)
    }
  )
  
  # Group all non-fixed combinations in one group if TRUE
  if (groupCombinations == TRUE) {
    data[findCombinations] <- "Other"
  } else {
    # Otherwise: group infrequent treatments below groupCombinations as "other"
    combinations <- as.matrix(data)[findCombinations == TRUE]
    
    freqCombinations <- matrix(
      rep(data$freq, times = ncol(data)),
      ncol = ncol(data))[findCombinations == TRUE]
    
    summaryCombinations <- data.table::data.table(
      combination = combinations,
      freq = freqCombinations
    )
    
    if (nrow(summaryCombinations) > 0) {
      summaryCombinations <- summaryCombinations[
        , .(freq = sum(freq)),
        by = combination
      ][order(-freq)]
      
      summarizeCombinations <- summaryCombinations$combination[
        summaryCombinations$freq <= as.numeric(groupCombinations)]
      
      selectedCombinations <- apply(
        X = data,
        MARGIN = 2,
        FUN = function(x) {
          x %in% summarizeCombinations
        }
      )
      data[selectedCombinations] <- "Other"
    }
  }
  return(data.table::as.data.table(data))
}
utils::globalVariables(c(
  ".", "..columns", "..l", "..layers", ".N", ".SD", "ALL_ROWS", "COUNT", "GAP_PREVIOUS",
  "SELECTED_ROWS", "all_combinations", "cohortId",  "combination", "combination_FRFS",
  "combination_LRFS", "duration_era", "event_cohort_id", 
  "event_cohort_id_previous", "event_cohort_name", "event_cohort_name1",
  "event_cohort_name2", "event_cohort_name3", "event_end_date",
  "event_end_date_next", "event_end_date_previous", "event_seq",
  "event_start_date", "event_start_date_next", "fixed_combinations", "freq",
  "gap_same", "group", "index_year", "lag_variable", "monotherapy", "person_id", "rleid"))