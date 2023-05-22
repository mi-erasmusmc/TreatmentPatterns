#' generateOutput
#'
#' Generates the output files in the specified output folder. It will also zip
#' the output folder into a zip-file.
#'
#' @param saveSettings
#'   Settings object as created by createSaveSettings().
#'
#' @returns NULL
#'
#' @export
generateOutput <- function(saveSettings) {
  # Get pathwaySettings
  pathwaySettings <- read.csv(
    file.path(
      saveSettings$outputFolder,
      "pathwaySettings.csv"
    )
  )

  settings <- pathwaySettings[-1]

  objList <- c()
  on.exit(rm(objList))

  # For each setting set do:
  dat <- lapply(seq_len(ncol(settings)), function(i) {
    objList <- append(objList, getPathways(
      outputFolder = saveSettings$outputFolder,
      tempFolder = saveSettings$tempFolder,
      databaseName = saveSettings$databaseName,
      studyName = settings[1, i],
      minCellCount = settings[14, i]
    ))

    treatmentPathways <- objList

    if (!is.null(treatmentPathways)) {
      # Write WithYear and NoYear files
      outputTreatedPatients(
        data = treatmentPathways[[1]],
        eventCohortIds = unlist(stringr::str_split(settings[3, i], ",")),
        groupCombinations = TRUE,
        outputFolder = saveSettings$outputFolder,
        outputFile = "percentageGroupsTreatedNoYear.csv"
      )

      outputTreatedPatients(
        data = treatmentPathways[[2]],
        eventCohortIds = unlist(stringr::str_split(settings[3, i], ",")),
        groupCombinations = TRUE,
        outputFolder = saveSettings$outputFolder,
        outputFile = "PercentageGroupsTreatedWithYear.csv"
      )

      objList <- append(objList, outputDurationEras(
        outputFolder = saveSettings$outputFolder,
        tempFolder = saveSettings$tempFolder,
        databaseName = saveSettings$databaseName,
        studyName = settings[1, i],
        eventCohortIds = unlist(stringr::str_split(settings[3, i], ",")),
        groupCombinations = TRUE,
        minCellCount = settings[14, i]
      ))

      objList <- append(objList, doMinCellCount(
        fileNoYear = treatmentPathways[[1]],
        fileWithYear = treatmentPathways[[2]],
        outputFolder = saveSettings$outputFolder,
        tempFolder = saveSettings$tempFolder,
        databaseName = saveSettings$databaseName,
        studyName = settings[1, i],
        groupCombinations = settings[16, i],
        minCellCount = settings[14, i],
        minCellMethod = settings[15, i]
      ))

      objList <- append(objList, lapply(treatmentPathways, function(pathway) {
        preprocessSunburstData(
          data = pathway,
          tempFolder = saveSettings$tempFolder,
          outputFolder = saveSettings$outputFolder,
          databaseName = saveSettings$databaseName,
          studyName = settings[1, i],
          eventCohortIds = unlist(stringr::str_split(settings[3, i], ",")),
          addNoPaths = settings[17, i]
        )
      }))
    }
  })

  durationEras <- dplyr::bind_rows(lapply(seq_len(length(dat)), function(i) {
    data.frame(
      eventCohortName = dat[[i]][[3]],
      eventSeq = dat[[i]][[4]],
      averageDuration = dat[[i]][[5]],
      median = dat[[i]][[6]],
      sd = dat[[i]][[7]],
      min = dat[[i]][[8]],
      max = dat[[i]][[9]],
      count = dat[[i]][[10]],
      study = rep(settings[1, i], length(dat[[i]][[3]]))
    )
  }))

  write.csv(
    x = durationEras,
    file = file.path(saveSettings$outputFolder, "durationEras.csv"),
    row.names = FALSE)


  perYear <- dplyr::bind_rows(lapply(seq_len(length(dat)), function(i) {
    row.names(dat[[i]][[1]]) <- NULL
    row.names(dat[[i]][[2]]) <- NULL

    dat[[i]][[1]]$index_year <- rep("no year", nrow(dat[[i]][[1]]))

    rbind(
      dat[[i]][[1]], # no year
      dat[[i]][[2]], fill = TRUE) # with year
  }))

  write.csv(
    x = perYear,
    file = file.path(saveSettings$outputFolder, "freqPerYear.csv"))

  # summary cnt
  summaryCount <- dplyr::bind_rows(lapply(seq_len(length(dat)), function(i) {
    dat[[i]][[11]]
  }))

  write.csv(
    x = summaryCount,
    file = file.path(saveSettings$outputFolder, "summaryCount.csv")
  )

  treatmentPathways <- dplyr::bind_rows(lapply(
    seq_len(length(dat)), function(i) {
    row.names(dat[[i]][[12]]) <- NULL
    row.names(dat[[i]][[13]]) <- NULL

    rbind(dat[[i]][[12]], dat[[i]][[13]], fill = TRUE)
  }))

  write.csv(
    x = treatmentPathways,
    file = file.path(saveSettings$outputFolder, "treatmentPathways.csv"),
    row.names = FALSE)

  zipPath <- normalizePath(file.path(
    saveSettings$rootFolder,
    paste0(basename(saveSettings$outputFolder), ".zip")))

  message(
    glue::glue("Zipping: {saveSettings$outputFolder}\nto: {zipPath}"))

  utils::zip(
    zipfile = zipPath,
    files = saveSettings$outputFolder,
    extras = "-j -q")
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
#' @return List with two dataframes.
getPathways <- function(
    outputFolder,
    tempFolder,
    databaseName,
    studyName,
    minCellCount) {
  # Try to read in paths from constructPathways.R for studyName
  file <- tryCatch({
    data.table::data.table(read.csv(
      file.path(
        tempFolder,
        studyName,
        paste0(databaseName, "_", studyName, "_paths.csv")))
      )
  }, error = function(e) {
    warning(
      glue::glue("Data is empty for study settings {studyName}"))
    return(NULL)
    }
  )

  # Summarize which non-fixed combinations occurring
  findCombinations <-
    apply(file, 2, function(x) {
      grepl("+", x, fixed = TRUE)
    })

  combinations <- as.matrix(file)[findCombinations == TRUE]
  numColumns <- sum(grepl("event_cohort_name", colnames(file)))
  freqCombinations <- matrix(
    rep(file$freq, times = numColumns),
    ncol = numColumns
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
      paste0("combinations.csv")
    ),
    row.names = FALSE
  )

  # Group the resulting treatment paths
  layers <-
    as.vector(colnames(file)[!grepl("index_year|freq", colnames(file))])
  fileNoYear <- file[, .(freq = sum(freq)), by = layers]
  fileWithYear <-
    file[, .(freq = sum(freq)), by = c(layers, "index_year")]

  return(list(fileNoYear, fileWithYear))
}


#' outputTreatedPatients
#'
#' Writes a csv-file containing the outcomes, event cohorts and layers
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
      subsetData <- data[data$index_year == as.character(y), ]
      if (nrow(subsetData) != 0) {
        subsetResult <-
          cbind(
            y,
            percentageGroupTreated(
              subsetData,
              eventCohortIds,
              groupCombinations,
              outputFolder
            )
          )
      } else {
        ParallelLogger::logInfo(warning(
          paste0("Subset of data is empty for study settings in year ", y)
        ))
        subsetResult <- NULL
      }
      return(subsetResult)
    })
    result <- data.table::rbindlist(results)
    result$y <- as.character(result$y)
  }
  write.csv(
    x = result,
    file = file.path(outputFolder, outputFile),
    row.names = FALSE)
  ParallelLogger::logInfo("outputTreatedPatients done")
}


#' percentageGroupTreated
#'
#' Help function to compute percentage of treated patient with certain event
#' cohorts for outputTreatedPatients.
#'
#' @param data
#'     data.frame of treatmentPathway list.
#' @param eventCohortIds
#'     Event cohort IDs
#' @param groupCombinations
#'     Group combinations
#' @param outputFolder
#'     Output folder
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

  cohorts <- read.csv(
    file.path(outputFolder, "cohortsToCreate.csv"))

  outcomes <- c(
    cohorts$cohortName[cohorts$cohortId %in% eventCohortIds],
    "Other"
  )

  # Group non-fixed combinations in one group according to groupCobinations
  data <- groupInfrequentCombinations(data, groupCombinations)

  percentGroupLayer <- sapply(
    X = layers,
    FUN = function(l) {
      sapply(
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

  pathsAll <- sum(data$freq)

  result$ALL_LAYERS <- sapply(
    X = outcomes,
    FUN = function(o) {
      pathsWithOutcome <- sum(sapply(
        X = seq_len(nrow(data)), FUN = function(r) {
          ifelse(o %in% data[r, ], data[r, freq], 0)
        }
      ))
      return(pathsWithOutcome * 100.0 / pathsAll)
    }
  )

  # Add rows for total, fixed combinations, all combinations
  if (length(layers) == 1) {
    result <- rbind(
      result,
      c("Fixed combinations",
        sum(result[grepl("\\&", result$outcomes), layers]),
        NA),
      c("All combinations",
        sum(result[grepl("Other|\\+|\\&", result$outcomes), layers]),
        NA),
      c("Monotherapy",
        sum(result[!grepl("Other|\\+|\\&", result$outcomes), layers]),
        NA)
      )
  } else {
    result <- rbind(
      result,
      c("Fixed combinations",
        colSums(result[grepl("\\&", result$outcomes), layers]),
        NA),
      c("All combinations",
        colSums(result[grepl("Other|\\+|\\&", result$outcomes), layers]),
        NA),
      c("Monotherapy",
        colSums(result[!grepl("Other|\\+|\\&", result$outcomes), layers]),
        NA)
      )
    }

  result$ALL_LAYERS[result$outcomes == "Fixed combinations"] <- sum(
    sapply(
      X = seq_len(nrow(data)),
      FUN = function(r) {
        ifelse(any(
          grepl("\\&", data[r, ])
        ), data[r, freq], 0)
      }
    )
  ) * 100.0 / pathsAll

  result$ALL_LAYERS[result$outcomes == "All combinations"] <- sum(
    sapply(
      X = seq_len(nrow(data)),
      FUN = function(r) {
        ifelse(any(
          grepl("Other|\\+|\\&", data[r, ])
        ), data[r, freq], 0)
      }
    )
  ) * 100.0 / pathsAll

  result$ALL_LAYERS[result$outcomes == "Monotherapy"] <- sum(
    sapply(seq_len(nrow(data)), function(r) {
      ifelse(any(
        !grepl("Other|\\+|\\&", data[r, ])
      ), data[r, freq], 0)
    })
  ) * 100.0 / pathsAll
  return(result)
}


#' outputDurationEras
#'
#' Computes the duration and count of each era.
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
  file <- data.table::data.table(read.csv(
    file.path(
      tempFolder,
      studyName,
      paste0(databaseName, "_", studyName, "_event_seq_processed.csv")
    )
  ))

  # Remove unnessary columns
  columns <- c("duration_era", "event_seq", "event_cohort_name")
  file <- file[, ..columns]
  file$duration_era <- as.numeric(file$duration_era)

  # Group non-fixed combinations in one group according to groupCobinations
  file <- groupInfrequentCombinations(file, groupCombinations)

  result <- file[, .(
    AVG_DURATION = round(mean(duration_era), 2),
    MEDIAN = round(median(duration_era), 2),
    SD = round(sd(duration_era), 2),
    MIN = min(duration_era),
    MAX = max(duration_era),
    COUNT = .N
  ), by = c("event_cohort_name", "event_seq")
  ][
    order(event_cohort_name, event_seq)
  ]

  # Add column for total treated, fixed combinations, all combinations
  file$fixed_combinations[grepl("\\&", file$event_cohort_name)] <- 1
  file$all_combinations[grepl("Other|\\+|\\&", file$event_cohort_name)] <- 1
  file$monotherapy[!grepl("Other|\\+|\\&", file$event_cohort_name)] <- 1

  # Duration average per layer
  resultTotalConcept <- file[, .(
    event_cohort_name = "Total treated",
    AVG_DURATION = round(mean(duration_era), 2),
    MEDIAN = round(median(duration_era), 2),
    SD = round(sd(duration_era), 2),
    MIN = min(duration_era),
    MAX = max(duration_era),
    COUNT = .N
  ), by = c("event_seq")]

  resultFixedCombinations <- file[, .(
    event_cohort_name = "Fixed combinations",
    AVG_DURATION = round(mean(duration_era), 2),
    MEDIAN = round(median(duration_era), 2),
    SD = round(sd(duration_era), 2),
    MIN = min(duration_era),
    MAX = max(duration_era),
    COUNT = .N
  ), by = c("event_seq", "fixed_combinations")]

  resultFixedCombinations <- resultFixedCombinations[
    !is.na(fixed_combinations),
  ]

  resultFixedCombinations$fixed_combinations <- NULL

  resultAllCombinations <- file[, .(
    event_cohort_name = "All combinations",
    AVG_DURATION = round(mean(duration_era), 2),
    MEDIAN = round(median(duration_era), 2),
    SD = round(sd(duration_era), 2),
    MIN = min(duration_era),
    MAX = max(duration_era),
    COUNT = .N
  ), by = c("event_seq", "all_combinations")]

  resultAllCombinations <- resultAllCombinations[
    !is.na(all_combinations),
  ]

  resultAllCombinations$all_combinations <- NULL

  resultMonotherapy <- file[, .(
    event_cohort_name = "Monotherapy",
    AVG_DURATION = round(mean(duration_era), 2),
    MEDIAN = round(median(duration_era), 2),
    SD = round(sd(duration_era), 2),
    MIN = min(duration_era),
    MAX = max(duration_era),
    COUNT = .N
  ), by = c("event_seq", "monotherapy")]

  resultMonotherapy <- resultMonotherapy[!is.na(monotherapy), ]
  resultMonotherapy$monotherapy <- NULL

  # Duration average all layers
  resultTotalSeq <- file[, .(
    event_seq = "Overall",
    AVG_DURATION = round(mean(duration_era), 2),
    MEDIAN = round(median(duration_era), 2),
    SD = round(sd(duration_era), 2),
    MIN = min(duration_era),
    MAX = max(duration_era),
    COUNT = .N
  ), by = c("event_cohort_name")]

  resultsTotalTreated <- file[, .(
    event_cohort_name = "Total treated",
    event_seq = "Overall",
    AVG_DURATION = round(mean(duration_era), 2),
    MEDIAN = round(median(duration_era), 2),
    SD = round(sd(duration_era), 2),
    MIN = min(duration_era),
    MAX = max(duration_era),
    COUNT = .N
  )]

  resultsTotalFixed <- file[fixed_combinations == 1, .(
    event_cohort_name = "Fixed combinations",
    event_seq = "Overall",
    AVG_DURATION = round(mean(duration_era), 2),
    MEDIAN = round(median(duration_era), 2),
    SD = round(sd(duration_era), 2),
    MIN = min(duration_era),
    MAX = max(duration_era),
    COUNT = .N
  )]

  resultsTotalMono <-
    file[all_combinations == 1, .(
      event_cohort_name = "All combinations",
      event_seq = "Overall",
      AVG_DURATION = round(mean(duration_era), 2),
      MEDIAN = round(median(duration_era), 2),
      SD = round(sd(duration_era), 2),
      MIN = min(duration_era),
      MAX = max(duration_era),
      COUNT = .N
    )]

  resultsTotalAllcombi <- file[monotherapy == 1, .(
    event_cohort_name = "Monotherapy",
    event_seq = "Overall",
    AVG_DURATION = round(mean(duration_era), 2),
    MEDIAN = round(median(duration_era), 2),
    SD = round(sd(duration_era), 2),
    MIN = min(duration_era),
    MAX = max(duration_era),
    COUNT = .N
  )]

  results <- rbind(
    result,
    resultTotalConcept,
    resultFixedCombinations,
    resultAllCombinations,
    resultMonotherapy,
    resultTotalSeq,
    resultsTotalTreated,
    resultsTotalFixed,
    resultsTotalMono,
    resultsTotalAllcombi
  )

  # Add missing groups
  cohorts <- read.csv(
    file.path(outputFolder, "cohortsToCreate.csv"))

  outcomes <- c(
    cohorts$cohortName[cohorts$cohortId %in% eventCohortIds],
    "Other")

  for (o in outcomes[!(outcomes %in% results$event_cohort_name)]) {
    results <- rbind(results, list(o, "Overall", 0.0, 0.0, 0.0, 0, 0, 0))
  }

  # Remove durations computed using less than minCellCount observations
  results[COUNT < as.numeric(minCellCount), c("AVG_DURATION", "MEDIAN", "SD", "MIN", "MAX", "COUNT")] <- NA
  ParallelLogger::logInfo("outputDurationEras done")
  on.exit(rm(columns))
  return(results)
}


#' doMinCellCount
#'
#' Apply minCellCount to generate censored treatment pathways to share and for
#' generating detailed output.
#'
#' @param fileNoYear
#'     Dataframe with aggregated treatment pathways of the target cohort in
#'     different rows (unique pathways, with frequency).
#' @param fileWithYear
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
#' @returns List
doMinCellCount <- function(
    fileNoYear,
    fileWithYear,
    outputFolder,
    tempFolder,
    databaseName,
    studyName,
    groupCombinations,
    minCellCount,
    minCellMethod) {
  # Group non-fixed combinations in one group according to groupCobinations
  fileNoYear <- groupInfrequentCombinations(
    fileNoYear,
    groupCombinations
  )

  fileWithYear <- groupInfrequentCombinations(
    fileWithYear,
    groupCombinations
  )

  layers <- as.vector(colnames(fileNoYear)[
    !grepl("index_year|freq", colnames(fileNoYear))
  ])

  # Calculate percentage treated before minCellCount
  summaryCounts <- read.csv(
    file.path(
      tempFolder,
      studyName,
      paste0(databaseName, "_", studyName, "_summary_cnt.csv")
    ))

  sumAll <- as.integer(summaryCounts[
    summaryCounts$index_year == "Number of persons in target cohort NA", "N"
  ])

  for (l in layers) {
    sumAllNotNA <- sum(fileNoYear$freq[!is.na(fileNoYear[, ..l])])
    percAllNotNA <- sumAllNotNA * 100.0 / sumAll
    summaryCounts <-
      rbind(summaryCounts, c(
        paste0("Percentage treated (before minCellCount) in ", l),
        percAllNotNA
      ))
  }

  # Apply minCellCount by adjusting to other most similar path (removing last
  # treatment in path) or else remove complete path
  if (minCellMethod == "Adjust") {
    col <- ncol(fileNoYear) - 1
    while (sum(fileNoYear$freq < minCellCount) > 0 && col != 0) {
      ParallelLogger::logInfo(
        paste(
          "Change col ",
          col,
          " to NA for ",
          sum(fileNoYear$freq < minCellCount),
          " paths with too low frequency (without year)"
        )
      )

      fileNoYear[freq < minCellCount, col] <- NA
      fileNoYear <- fileNoYear[, .(freq = sum(freq)), by = layers]

      col <- col - 1
    }

    col <- ncol(fileWithYear) - 2
    while (sum(fileWithYear$freq < minCellCount) > 0 && col != 0) {
      ParallelLogger::logInfo(
        paste(
          "Change col ",
          col,
          " to NA for ",
          sum(fileWithYear$freq < minCellCount),
          " paths with too low frequency (with year)"
        )
      )

      fileWithYear[freq < minCellCount, col] <- NA
      fileWithYear <- fileWithYear[
        , .(freq = sum(freq)),
        by = c(layers, "index_year")
      ]

      col <- col - 1
    }

    # If path becomes completely NA -> add to "Other" group to distinguish
    # from non-treated
    fileNoYear$event_cohort_name1[is.na(fileNoYear$event_cohort_name1)] <-
      "Other"
    fileNoYear <- fileNoYear[, .(freq = sum(freq)), by = layers]

    fileWithYear$event_cohort_name1[is.na(fileWithYear$event_cohort_name1)] <-
      "Other"
    fileWithYear <-
      fileWithYear[, .(freq = sum(freq)), by = c(layers, "index_year")]
  }

  ParallelLogger::logInfo(paste(
    "Remove ",
    sum(fileNoYear$freq < minCellCount),
    " paths with too low frequency (without year)"
  ))

  fileNoYear <- fileNoYear[freq >= minCellCount, ]

  ParallelLogger::logInfo(paste(
    "Remove ",
    sum(fileWithYear$freq < minCellCount),
    " paths with too low frequency (with year)"
  ))

  fileWithYear <- fileWithYear[freq >= minCellCount, ]

  # Add updated counts after minCellCount
  summaryCounts <- rbind(
    summaryCounts,
    c("Number of pathways (after minCellCount) in NA", sum(fileNoYear$freq))
  )

  for (y in unique(fileWithYear$index_year)) {
    summaryCounts <- rbind(
      summaryCounts,
      c(
        paste0("Number of pathways (after minCellCount) in ", y),
        sum(fileWithYear$freq[fileWithYear$index_year == y])
      )
    )
  }
  ParallelLogger::logInfo("doMinCellCount done")
  return(list(summaryCounts, fileNoYear, fileWithYear))
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
#' @param tempFolder
#'     Temp folder
#'
#' @returns NULL
preprocessSunburstData <- function(
    data,
    tempFolder,
    outputFolder,
    databaseName,
    studyName,
    eventCohortIds,
    addNoPaths) {
  if (nrow(data) != 0) {
    if (is.null(data$index_year)) {
      # For file_noyear compute once
      outDF <- inputSunburstPlot(
        data,
        tempFolder,
        outputFolder,
        databaseName,
        studyName,
        addNoPaths,
        indexYear = "all"
      )
    } else {
      # For file_withyear compute per year
      years <- unlist(unique(data[, "index_year"]))

      outDF <- dplyr::bind_rows(lapply(years, function(year) {
        subsetData <- data[index_year == as.character(year), ]

        if (nrow(subsetData) != 0) {
          subsetData <- inputSunburstPlot(
            subsetData,
            tempFolder,
            outputFolder,
            databaseName,
            studyName,
            addNoPaths,
            indexYear = year
          )
        } else {
          ParallelLogger::logInfo(warning(
            paste0(
              "Subset of data is empty for study settings ",
              studyName,
              " in year ",
              year
            )
          ))
        }
      }))
    }
    ParallelLogger::logInfo("preprocessSunburstData done")
  }
  return(outDF)
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
#' @param indexYear
#'     Index year
#' @param tempFolder
#'     Temp folder
#'
#' @returns transformed_file
inputSunburstPlot <- function(
    data,
    tempFolder,
    outputFolder,
    databaseName,
    studyName,
    addNoPaths,
    indexYear) {
  layers <- as.vector(
    colnames(data)[!grepl("index_year|freq", colnames(data))]
  )

  transforedFile <- apply(
    X = data[, ..layers],
    MARGIN = 1,
    FUN = paste,
    collapse = "-"
  )

  transforedFile <- stringr::str_replace_all(
    transforedFile, "-NA", ""
  )

  transforedFile <- paste0(transforedFile, "-End")

  transforedFile <- data.frame(
    path = transforedFile,
    freq = data$freq,
    stringsAsFactors = FALSE
  )

  if (addNoPaths) {
    summaryCounts <- read.csv(
      file.path(
        tempFolder,
        studyName,
        paste0(databaseName, "_", studyName, "_summary_cnt.csv"))
    )

    if (indexYear == "all") {
      noPath <- as.integer(summaryCounts[
        summaryCounts$index_year ==
          "Number of persons in target cohort NA", "N"
      ]) -
        sum(transforedFile$freq)
    } else {
      noPath <- as.integer(summaryCounts[
        summaryCounts$index_year ==
          paste0("Number of persons in target cohort ", indexYear), "N"
      ]) -
        sum(transforedFile$freq)
    }
    transforedFile <- rbind(transforedFile, c("End", noPath))
  }
  transforedFile$path <- as.factor(transforedFile$path)
  transforedFile$freq <- as.integer(transforedFile$freq)
  transforedFile <-
    transforedFile[order(-transforedFile$freq, transforedFile$path), ]

  transforedFile$year <- rep(x = indexYear, nrow(transforedFile))
  transforedFile$studyName <- rep(x = studyName, nrow(transforedFile))
  on.exit(rm(layers))
  return(transforedFile)
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
  ".", "..columns", "..l", "..layers", ".N", ".SD", "ALL_ROWS", "COUNT",
  "GAP_PREVIOUS", "SELECTED_ROWS", "all_combinations", "cohortId",
  "combination", "combination_FRFS", "combination_LRFS", "duration_era",
  "event_cohort_id", "event_cohort_id_previous", "event_cohort_name",
  "event_cohort_name1", "event_cohort_name2", "event_cohort_name3",
  "event_end_date", "event_end_date_next", "event_end_date_previous",
  "event_seq", "event_start_date", "event_start_date_next",
  "fixed_combinations", "freq", "gap_same", "group", "index_year",
  "lag_variable", "monotherapy", "person_id", "rleid"))
