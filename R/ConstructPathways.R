#' checkConstructPathways
#'
#' Checks parameters for constructPathways.
#'
#' @param env Environment containging all the function environment variables.
#'
#' @return TRUE if all assertions pass
checkConstructPathways <- function(env) {
  # dataSettings
  checkmate::assert(
    checkmate::checkClass(env$dataSettings, "dataSettings"),
    checkmate::checkClass(
      env$dataSettings$connectionDetails,
      "connectionDetails"),
    checkmate::checkCharacter(env$dataSettings$connectionDetails$dbms, len = 1),
    checkmate::checkCharacter(env$dataSettings$cdmDatabaseSchema, len = 1),
    checkmate::checkCharacter(env$dataSettings$resultSchema, len = 1),
    checkmate::checkCharacter(env$dataSettings$cohortTable, len = 1))

  # pathwaySettings
  checkmate::assert(
    checkmate::checkClass(env$pathwaySettings, "pathwaySettings"),
    checkmate::checkDataFrame(env$pathwaySettings$all_settings, nrows = 17))

  # saveSettings
  checkmate::assert(
    checkmate::checkClass(env$saveSettings, "saveSettings"),
    checkmate::checkCharacter(env$saveSettings$databaseName, len = 1),
    checkmate::checkDirectory(env$saveSettings$rootFolder),
    checkmate::checkDirectory(env$saveSettings$outputFolder),
    checkmate::checkDirectory(env$saveSettings$tempFolder)
  )
  return(TRUE)
}

#' constructPathways
#'
#' Construct treatment pathways. Also generates output in csv format.
#'
#' @param dataSettings
#'     Settings object as created by createDataSettings().
#' @param pathwaySettings
#'     Settings object as created by createPathwaySettings().
#' @param saveSettings
#'     Settings object as created by createSaveSettings().
#'
#' @export
#'
#' @examples \dontrun{
#'   constructPathways(
#'     dataSettings = dataSettings,
#'     pathwaySettings = pathwaySettings,
#'     saveSettings = saveSettings)}
constructPathways <- function(dataSettings,
                              pathwaySettings,
                              saveSettings) {
  # Check if inputs correct
  check <- checkConstructPathways(environment())

  if (check) {
    # do stuff
    message("check passed")
  }

  # Load already created cohorts
  # Connect to database
  connection <- DatabaseConnector::connect(dataSettings$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Get cohorts from database
  fullCohorts <- data.table::as.data.table(extractFile(
    connection,
    dataSettings$cohortTable,
    dataSettings$resultSchema,
    dataSettings$connectionDetails$dbms))

  colnames(fullCohorts) <- c("cohort_id",
                              "person_id",
                              "start_date",
                              "end_date")

  # Save pathway settings
  pathwaySettings <- pathwaySettings$all_settings

  # Create output and temp folders
  fs::dir_create(saveSettings$outputFolder)
  fs::dir_create(saveSettings$tempFolder)

  write.csv(
    pathwaySettings,
    file.path(
      saveSettings$outputFolder,
      "pathwaySettings.csv"),
    row.names = FALSE)

  # For all different pathway settings
  settings <- colnames(pathwaySettings)[grepl("analysis",
                                              colnames(pathwaySettings))]

  for (s in settings) {
    studyName <- pathwaySettings[pathwaySettings$param == "studyName", s]

    # Check if directories exist and create if necessary
    tempFolders <- file.path(saveSettings$tempFolder, studyName)
    if (!file.exists(tempFolders))
      dir.create(tempFolders,
                 recursive = TRUE)

    ParallelLogger::logInfo(print(paste0("Constructing treatment pathways: ",
                                         studyName)))

    # Select cohorts included
    targetCohortId <- pathwaySettings[
      pathwaySettings$param == "targetCohortId", s]

    eventCohortIds <- pathwaySettings[
      pathwaySettings$param == "eventCohortIds", s]

    eventCohortIds <- unlist(strsplit(eventCohortIds, split = c(";|,")))

    # Analysis settings
    includeTreatments <- pathwaySettings[
      pathwaySettings$param == "includeTreatments", s]

    periodPriorToIndex <- as.integer(pathwaySettings[
      pathwaySettings$param == "periodPriorToIndex", s])

    minEraDuration <- as.integer(pathwaySettings[
      pathwaySettings$param == "minEraDuration", s])

    splitEventCohorts <- pathwaySettings[
      pathwaySettings$param == "splitEventCohorts", s]

    splitTime <- pathwaySettings[
      pathwaySettings$param == "splitTime", s]

    eraCollapseSize <- as.integer(pathwaySettings[
      pathwaySettings$param == "eraCollapseSize", s])

    combinationWindow <- as.integer(pathwaySettings[
      pathwaySettings$param == "combinationWindow", s])

    minPostCombinationDuration <- as.integer(pathwaySettings[
      pathwaySettings$param == "minPostCombinationDuration", s])

    filterTreatments <-  pathwaySettings[
      pathwaySettings$param == "filterTreatments", s]

    maxPathLength <- as.integer(pathwaySettings[
      pathwaySettings$param == "maxPathLength", s])

    # Select subset of full cohort including only data for the current target
    # cohort
    selectPeople <- fullCohorts$person_id[
      fullCohorts$cohort_id == targetCohortId]

    currentCohorts <- fullCohorts[
      fullCohorts$person_id %in% selectPeople, ]

    if (nrow(currentCohorts) != 0) {
      # Preprocess the target/event cohorts to create treatment history
      treatmentHistory <- doCreateTreatmentHistory(
        currentCohorts,
        targetCohortId,
        eventCohortIds,
        periodPriorToIndex,
        includeTreatments)

      # Apply pathway settings to create treatment pathways
      ParallelLogger::logInfo(paste(
        "Construct treatment pathways, this may",
        "take a while for larger datasets."))

      writeLines(paste0("Original number of rows: ", nrow(treatmentHistory)))

      # TODO: check what happens if treatmentHistory zero or few rows
      # (throw errors)

      treatmentHistory <- doEraDuration(
        treatmentHistory,
        minEraDuration)

      treatmentHistory <- doSplitEventCohorts(
        treatmentHistory,
        splitEventCohorts,
        splitTime,
        saveSettings$outputFolder)

      treatmentHistory <- doEraCollapse(
        treatmentHistory,
        eraCollapseSize)

      treatmentHistory <- doCombinationWindow(
        treatmentHistory,
        combinationWindow,
        minPostCombinationDuration)

      treatmentHistory <- doFilterTreatments(
        treatmentHistory,
        filterTreatments)

      if (nrow(treatmentHistory) != 0) {
        # Add event_seq number to determine order of treatments in pathway
        ParallelLogger::logInfo("Adding drug sequence number.")
        treatmentHistory <- treatmentHistory[
          order(person_id, event_start_date, event_end_date), ]

        treatmentHistory[, event_seq := seq_len(.N), by = .(person_id)]

        treatmentHistory <- doMaxPathLength(
          treatmentHistory,
          maxPathLength)

        # Add event_cohort_name (instead of only event_cohort_id)
        ParallelLogger::logInfo("Adding concept names.")

        treatmentHistory <- addLabels(
          treatmentHistory,
          saveSettings$outputFolder)

        # Order the combinations
        ParallelLogger::logInfo("Ordering the combinations.")
        combi <- grep(
          pattern = "+",
          x = treatmentHistory$event_cohort_name,
          fixed = TRUE)

        cohortNames <- strsplit(
          x = treatmentHistory$event_cohort_name[combi],
          split = "+",
          fixed = TRUE)

        treatmentHistory$event_cohort_name[combi] <- sapply(
          X = cohortNames,
          FUN = function(x) {
            paste(sort(x), collapse = "+")})

        treatmentHistory$event_cohort_name <- unlist(
          treatmentHistory$event_cohort_name)
      }

      # Save the processed treatment history
      write.csv(treatmentHistory, file.path(
        tempFolders,
        paste0(
          saveSettings$databaseName,
          "_",
          studyName,
          "_event_seq_processed.csv")),
        row.names = FALSE)

      # Save the treatment pathways
      if (nrow(treatmentHistory) != 0) {
        treatmentPathways <- data.table::as.data.table(
          data.table::dcast(
            data = treatmentHistory,
            formula = person_id + index_year ~ event_seq,
            value.var = "event_cohort_name"))

        colnames(treatmentPathways)[3:ncol(treatmentPathways)] <- paste0(
          "event_cohort_name",
          colnames(treatmentPathways)[3:ncol(treatmentPathways)])

        layers <- c(colnames(treatmentPathways))[
          3:min(7, ncol(treatmentPathways))] # max first 5

        treatmentPathways <- treatmentPathways[
          , .(freq = length((person_id))), by = c(layers, "index_year")]

        write.csv(
          x = treatmentPathways,
          file = file.path(
            tempFolders,
            paste0(
              saveSettings$databaseName,
              "_", studyName,
              "_paths.csv")),
          row.names = FALSE)

        # Calculate counts of the number of persons in target cohort / with
        # pathways, in total / per year
        targetCohort <- currentCohorts[
          currentCohorts$cohort_id %in% targetCohortId, , ]

        targetCohort$index_year <- as.numeric(format(
          targetCohort$start_date,
          "%Y"))

        countsTargetCohort <- data.table::rollup(
          targetCohort,
          .N,
          by = c("index_year"))

        countsTargetCohort$index_year <- paste0(
          "Number of persons in target cohort ",
          countsTargetCohort$index_year)

        countsPathway <- rollup(
          treatmentPathways,
          sum(freq),
          by = c("index_year"))

        countsPathway$index_year <- paste0(
          "Number of pathways (before minCellCount) in ",
          countsPathway$index_year)

        colnames(countsPathway) <- colnames(countsTargetCohort)
        counts <- rbind(countsTargetCohort, countsPathway)

        write.csv(
          counts,
          file.path(
            tempFolders,
            paste0(
              saveSettings$databaseName,
              "_",
              studyName,
              "_summary_cnt.csv")),
          row.names = FALSE)
      }
    }
  }
  ParallelLogger::logInfo("constructPathways done.")
}


#' doCreateTreatmentHistory
#'
#' @param currentCohorts
#'     Dataframe with target and event cohorts of current study settings.
#' @param targetCohortId
#'     Target cohort ID of current study settings.
#' @param eventCohortIds
#'     Event cohort IDs of current study settings.
#' @param periodPriorToIndex
#'     Number of days prior to the index date of the target cohort that event
#'     cohorts are allowed to start
#' @param includeTreatments
#'     Include treatments starting ('startDate') or ending ('endDate') after
#'     target cohort start date
#'
#' @return currentCohorts
#'     Updated dataframe, including only event cohorts after
#'     target cohort start date and with added index year, duration, gap same
#'     columns.
doCreateTreatmentHistory <- function(
    currentCohorts,
    targetCohortId,
    eventCohortIds,
    periodPriorToIndex,
    includeTreatments) {
  checkmate::assert(checkmate::check_data_frame(
    currentCohorts,
    min.cols = 4,
    col.names = "named"))
  checkmate::assert(checkmate::checkNames(
    names(currentCohorts),
    permutation.of = c("cohort_id", "person_id", "start_date", "end_date")))

  checkmate::assert(checkmate::checkCharacter(targetCohortId, len = 1))
  checkmate::assert(checkmate::checkCharacter(eventCohortIds))
  checkmate::assert(checkmate::checkInt(periodPriorToIndex))

  # Add index year column based on start date target cohort
  targetCohort <- currentCohorts[
    currentCohorts$cohort_id %in% targetCohortId, , ]

  targetCohort$index_year <- as.numeric(format(targetCohort$start_date, "%Y"))

  # Select event cohorts for target cohort and merge with start/end date and
  # index year
  eventCohorts <- currentCohorts[
    currentCohorts$cohort_id %in% eventCohortIds, , ]

  currentCohorts <- merge(
    x = eventCohorts,
    y = targetCohort,
    by = c("person_id"),
    all.x = TRUE,
    allow.cartesian = TRUE)

  # Only keep event cohorts starting (startDate) or ending (endDate) after
  # target cohort start date
  if (includeTreatments == "startDate") {
    currentCohorts <- currentCohorts[
      currentCohorts$start_date.y -
        as.difftime(periodPriorToIndex, units = "days") <=
        currentCohorts$start_date.x &
        currentCohorts$start_date.x <
        currentCohorts$end_date.y, ]

  } else if (includeTreatments == "endDate") {
    currentCohorts <- currentCohorts[
      currentCohorts$start_date.y -
        as.difftime(periodPriorToIndex, units = "days") <=
        currentCohorts$end_date.x &
        currentCohorts$start_date.x <
        currentCohorts$end_date.y, ]

    currentCohorts$start_date.x <- pmax(
      currentCohorts$start_date.y - as.difftime(
        periodPriorToIndex, units = "days"),
      currentCohorts$start_date.x)
  } else {
    warning(paste(
      "includeTreatments input incorrect,",
      "return all event cohorts ('includeTreatments')"))
    currentCohorts <- currentCohorts[
      currentCohorts$start_date.y -
        as.difftime(periodPriorToIndex, units = "days") <=
        currentCohorts$start_date.x &
        currentCohorts$start_date.x <
        currentCohorts$end_date.y, ]
  }

  # Remove unnecessary columns
  currentCohorts <- currentCohorts[
    , c("person_id", "index_year", "cohort_id.x",
        "start_date.x", "end_date.x")]

  colnames(currentCohorts) <- c(
    "person_id", "index_year", "event_cohort_id",
    "event_start_date", "event_end_date")

  # Calculate duration and gap same
  currentCohorts[,
    duration_era := difftime(event_end_date, event_start_date, units = "days")]

  currentCohorts <- currentCohorts[order(event_start_date, event_end_date), ]

  currentCohorts[
    , lag_variable := data.table::shift(event_end_date, type = "lag"),
    by = c("person_id", "event_cohort_id")]

  currentCohorts[,
    gap_same := difftime(event_start_date, lag_variable, units = "days"), ]

  currentCohorts$lag_variable <- NULL
  return(currentCohorts)
}

#' doEraDuration
#'
#' Filters the treatmentHistory based on the specified minimum era duration
#'
#' @param treatmentHistory
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param minEraDuration
#'     Minimum time an event era should last to be included in analysis.
#'
#' @return treatmentHistory
#'     Updated dataframe, rows with duration <
#'     minEraDuration filtered out.
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#' doEraDuration(treatmentHistory = th, minEraDuration = 1)
#' }
doEraDuration <- function(treatmentHistory, minEraDuration) {
  # Assertions
  checkmate::assertDataFrame(x = treatmentHistory)
  checkmate::assertNumeric(
    x = minEraDuration,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE
  )

  treatmentHistory <- treatmentHistory[duration_era >= minEraDuration, ]
  ParallelLogger::logInfo(print(
    paste0("After minEraDuration: ", nrow(treatmentHistory))))
  return(treatmentHistory)
}


#' doStepDuration
#'
#' Filters treatmentHistory based on if durationEra is smaller than the
#' specified minimum post combination duration (minPostCombinationDuration).
#'
#' @param treatmentHistory
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param minPostCombinationDuration
#'     Minimum time an event era should last before or after a generated
#'     combination treatment for it to be included in analysis.
#'
#' @return treatmentHistory
#'     Updated dataframe, rows with duration_era <
#'     minPostCombinationDuration filtered out.
doStepDuration <- function(treatmentHistory, minPostCombinationDuration) {
  # Assertions
  checkmate::assertDataFrame(x = treatmentHistory)
  checkmate::assertNumeric(
    x = minPostCombinationDuration,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE
  )

  treatmentHistory <- subset(
    x = treatmentHistory,
    duration_era >= minPostCombinationDuration | is.na(duration_era))

  ParallelLogger::logInfo(
    glue::glue("After minPostCombinationDuration: {nrow(treatmentHistory)}"))
  return(treatmentHistory)
}


#' doSplitEventCohorts
#'
#' Splits the treatmentHistory data.frame based on event cohorts into ‘acute’
#' and ‘therapy’ cohorts.
#'
#' @param treatmentHistory
#'     Dataframe with event cohorts of the target cohort in different rows.
#'
#' @param splitEventCohorts
#'     Specify event cohort to split in acute (< X days) and therapy
#'     (>= X days).
#'
#' @param splitTime
#'     Specify number of days (X) at which each of the split event cohorts
#'     should be split in acute and therapy
#'
#' @param outputFolder
#'     Name of local folder to place results; make sure to use forward
#'     slashes (/).
#'
#' @return treatmentHistory
#'     Updated dataframe, with specified event cohorts now
#'     split in two different event cohorts (acute and therapy).
#'
#' @examples \dontrun{
#' source(system.file(
#'   package = "TreatmentPatterns",
#'   "examples", "SettingObjects", "createDummySettings.R"))
#'
#' source(system.file(
#'   package = "TreatmentPatterns",
#'   "testing",
#'   "testParams.R"))
#'
#' doSplitEventCohorts(
#'   treatmentHistory = doEraDurationTH,
#'   splitEventCohorts = c(1,2,3),
#'   splitTime = c("30", "20", "10"),
#'   outputFolder = saveSettings$outputFolder)}
doSplitEventCohorts <- function(
    treatmentHistory,
    splitEventCohorts,
    splitTime,
    outputFolder) {

  if (all(!is.na(splitEventCohorts))) {
    # Load in labels cohorts
    labels <- data.table::data.table(read.csv(
      file = file.path(outputFolder, "cohortsToCreate.csv")))

    # Check if splitEventCohorts == splitTime
    checkmate::assertTRUE(length(splitEventCohorts) == length(splitTime))

    for (c in seq_len(length(splitEventCohorts))) {
      cohort <- splitEventCohorts[c]
      cutoff <- splitTime[c]

      # Label as acute
      treatmentHistory[
        event_cohort_id == cohort &
          duration_era <
          cutoff, "event_cohort_id"] <- as.integer(paste0(cohort, 1))

      # Label as therapy
      treatmentHistory[
        event_cohort_id == cohort &
          duration_era >= cutoff,
        "event_cohort_id"] <- as.integer(paste0(cohort, 2))

      # Add new labels
      original <- labels[cohortId == as.integer(cohort), ]

      acute <- original
      acute$cohortId <- as.integer(paste0(cohort, 1))
      acute$cohortName <- paste0(acute$cohortName, " (acute)")

      therapy <- original
      therapy$cohortId <- as.integer(paste0(cohort, 2))
      therapy$cohortName <- paste0(therapy$cohortName, " (therapy)")

      labels <- labels[cohortId != as.integer(cohort), ]
      labels <- rbind(labels, acute, therapy)
    }
  }
  return(treatmentHistory)
}


#' doEraCollapse
#'
#' Updates the treatmentHistory data.frame where if gapSame is smaller than the
#' specified era collapse size (eraCollapseSize) are collapsed
#'
#' @param treatmentHistory
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param eraCollapseSize
#'     Window of time between which two eras of the same event cohort are
#'     collapsed into one era.
#'
#' @return treatmentHistory
#'     Updated dataframe, where event cohorts with
#'     gap_same < eraCollapseSize are collapsed.
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#' doEraCollapse(treatmentHistory = th, eraCollapseSize = 1)
#' }
doEraCollapse <- function(treatmentHistory, eraCollapseSize) {
  # Assertions
  checkmate::assertDataFrame(x = treatmentHistory)
  checkmate::assertNumeric(
    x = eraCollapseSize,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE
  )

  # Order treatmentHistory by person_id, event_cohort_id, start_date, end_date
  treatmentHistory <- treatmentHistory[
    order(person_id, event_cohort_id, event_start_date, event_end_date), ]

  # Find all rows with gap_same < eraCollapseSize
  rows <- which(treatmentHistory$gap_same < eraCollapseSize)

  # For all rows, modify the row preceding, loop backwards in case more than
  # one collapse
  for (r in rev(rows)) {
    treatmentHistory[r - 1, "event_end_date"] <- treatmentHistory[
      r,
      event_end_date]
  }

  # Remove all rows with gap_same < eraCollapseSize
  treatmentHistory <- treatmentHistory[!rows, ]
  treatmentHistory[, gap_same := NULL]

  # Re-calculate duration_era
  treatmentHistory[, duration_era := difftime(
    time1 = event_end_date,
    time2 = event_start_date,
    units = "days")]

  ParallelLogger::logInfo(print(paste0(
    "After eraCollapseSize: ",
    nrow(treatmentHistory))))
  return(treatmentHistory)
}


#' Combine overlapping events into combinations
#'
#' doCombinationWindow is an internal function that combines overlapping events
#' into combination events. It accepts a treatmentHistory dataframe and returns
#' a modified treatmentHistory dataframe. The returned treatmentHistory
#' dataframe always has the property that a person is only in one event cohort,
#' which might be a combination event cohort, at any point time.
#'
#' @param treatmentHistory
#'     A dataframe of 'event cohorts' with the following columns:
#'     event_cohort_id, person_id, event_start_date, event_end_date.
#'
#' @param combinationWindow
#'     Minimum number of days two event cohorts need to overlap to be
#'     considered a combination event.
#'
#' @param minPostCombinationDuration
#'     Minimum number of days an event era starting after a combination event
#'     or ending before a combination event must last to be counted a separate
#'     event. Events occuring before or after a combination that are less than
#'     `minPostCombinationDuration` days long will be dropped from the analysis.
#'
#' @return A treatmentHistory dataframe with the columns event_cohort_id,
#'     person_id, event_start_date, event_end_date. event_cohort_id will be
#'     of character type and combination events will have a new event_cohort_id
#'     made up of the concatenated event_cohort_ids of each combined
#'     event_cohort_id. When two events overlap for more than
#'     `combinationWindow` days they will be collapsed into a single combination
#'     event. Events are collapsed iteratively starting with the first two
#'     overlapping events per person and continuing until no more overlapping
#'     events exist in the treatmentHistory.
doCombinationWindow <- function(
    treatmentHistory,
    combinationWindow,
    minPostCombinationDuration) {
  time1 <- Sys.time()

  treatmentHistory$event_cohort_id <- as.character(
    treatmentHistory$event_cohort_id)

  # Find which rows contain some overlap
  treatmentHistory <- selectRowsCombinationWindow(treatmentHistory)

  # While rows that need modification exist:
  iterations <- 1
  while (sum(treatmentHistory$SELECTED_ROWS) != 0) {

    # Which rows have gap previous shorter than combination window OR
    # min(current duration era, previous duration era) -> add column switch
    treatmentHistory[
      SELECTED_ROWS == 1 &
        (-GAP_PREVIOUS < combinationWindow &
           !(-GAP_PREVIOUS == duration_era |
               -GAP_PREVIOUS == data.table::shift(duration_era, type = "lag"))),
      switch := 1]

    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] <=
    # treatmentHistory[r, event_end_date] ->
    # add column combination first received, first stopped
    treatmentHistory[
      SELECTED_ROWS == 1 &
        is.na(switch) &
        data.table::shift(event_end_date, type = "lag") <= event_end_date,
      combination_FRFS := 1]

    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] >
    # treatmentHistory[r, event_end_date] ->
    # add column combination last received, first stopped
    treatmentHistory[
      SELECTED_ROWS == 1 &
        is.na(switch) &
        data.table::shift(event_end_date, type = "lag") >
        event_end_date, combination_LRFS := 1]

    ParallelLogger::logInfo(print(paste0(
      "Iteration ", iterations,
      " modifying  ", sum(treatmentHistory$SELECTED_ROWS),
      " selected rows out of ",
      nrow(treatmentHistory), ": ",
      sum(!is.na(treatmentHistory$switch)),
      " switches, ", sum(!is.na(treatmentHistory$combination_FRFS)),
      " combinations FRFS and ",
      sum(!is.na(treatmentHistory$combination_LRFS)),
      " combinations LRFS")))

    sumSwitchComb <- sum(
      sum(!is.na(treatmentHistory$switch)),
      sum(!is.na(treatmentHistory$combination_FRFS)),
      sum(!is.na(treatmentHistory$combination_LRFS)))

    sumSelectedRows <- sum(treatmentHistory$SELECTED_ROWS)

    if (sumSwitchComb != sumSelectedRows) {
      warning(paste0(
        sum(treatmentHistory$SELECTED_ROWS),
        " does not equal total sum ",
        sum(!is.na(treatmentHistory$switch)) +
          sum(!is.na(treatmentHistory$combination_FRFS)) +
          sum(!is.na(treatmentHistory$combination_LRFS))))
    }

    # Do transformations for each of the three newly added columns
    # Construct helpers
    treatmentHistory$event_start_date_next <-
      treatmentHistory[,
        data.table::shift(event_start_date, type = "lead"),
        by = person_id][, 2]

    treatmentHistory$event_end_date_previous <-
      treatmentHistory[,
        data.table::shift(event_end_date, type = "lag"),
        by = person_id][, 2]

    treatmentHistory$event_end_date_next <-
      treatmentHistory[,
        data.table::shift(event_end_date, type = "lead"),
        by = person_id][, 2]

    treatmentHistory$event_cohort_id_previous <-
      treatmentHistory[,
        data.table::shift(event_cohort_id, type = "lag"),
        by = person_id][, 2]

    # Case: switch
    # Change end treatmentHistory of previous row ->
    # no minPostCombinationDuration
    treatmentHistory[data.table::shift(
      switch,
      type = "lead") == 1,
      event_end_date := event_start_date_next]

    # Case: combination_FRFS
    # Add a new row with start date (r) and end date (r-1) as combination (copy
    # current row + change end date + update concept id) -> no
    # minPostCombinationDuration
    addRowsFRFS <- treatmentHistory[combination_FRFS == 1, ]
    addRowsFRFS[, event_end_date := event_end_date_previous]

    addRowsFRFS[, event_cohort_id := paste0(
      event_cohort_id, "+", event_cohort_id_previous)]

    # Change end date of previous row -> check minPostCombinationDuration
    treatmentHistory[
      data.table::shift(combination_FRFS, type = "lead") == 1,
      c("event_end_date", "check_duration") := list(event_start_date_next, 1)]

    # Change start date of current row -> check minPostCombinationDuration
    treatmentHistory[
      combination_FRFS == 1,
      c("event_start_date", "check_duration") := list(
        event_end_date_previous, 1)]

    # Case: combination_LRFS
    # Change current row to combination -> no minPostCombinationDuration
    treatmentHistory[
      combination_LRFS == 1,
      event_cohort_id := paste0(
        event_cohort_id, "+", event_cohort_id_previous)]

    # Add a new row with end date (r) and end date (r-1) to split drug era
    # (copy previous row + change end date) -> check minPostCombinationDuration
    addRowsLRFS <- treatmentHistory[
      data.table::shift(combination_LRFS, type = "lead") == 1, ]

    addRowsLRFS[
      , c("event_start_date", "check_duration") := list(
        event_end_date_next, 1)]

    # Change end date of previous row -> check minPostCombinationDuration
    treatmentHistory[
      data.table::shift(combination_LRFS, type = "lead") == 1,
      c("event_end_date", "check_duration") := list(event_start_date_next, 1)]

    # Combine all rows and remove helper columns
    treatmentHistory <- rbind(treatmentHistory, addRowsFRFS, fill = TRUE)
    treatmentHistory <- rbind(treatmentHistory, addRowsLRFS)

    # Re-calculate duration_era
    treatmentHistory[
      , duration_era := difftime(
        event_end_date, event_start_date, units = "days")]

    # Check duration drug eras before/after generated combination treatments
    treatmentHistory <- doStepDuration(
      treatmentHistory, minPostCombinationDuration)

    # Preparations for next iteration
    treatmentHistory <- treatmentHistory[
      , c("person_id", "index_year", "event_cohort_id",
         "event_start_date", "event_end_date", "duration_era")]

    treatmentHistory <- selectRowsCombinationWindow(treatmentHistory)
    iterations <- iterations + 1

    gc()
  }

  ParallelLogger::logInfo(print(paste0(
    "After combinationWindow: ", nrow(treatmentHistory))))

  treatmentHistory[, GAP_PREVIOUS := NULL]
  treatmentHistory[, SELECTED_ROWS := NULL]

  time2 <- Sys.time()
  ParallelLogger::logInfo(paste0(
    "Time needed to execute combination window ",
    difftime(time2, time1, units = "mins")))

  return(treatmentHistory)
}


#' selectRowsCombinationWindow
#'
#' Help function for doCombinationWindow that selects one overlapping drug era
#' per person to modify in next iteration of the combination window.
#'
#' @param treatmentHistory
#'   Dataframe with event cohorts of the target cohort in different rows.
#'
#' @return Updated treatmentHistory data.frame
#'
#' @examples \dontrun{
#' source(system.file(
#'   package = "TreatmentPatterns",
#'  "testing", "testParams.R"))
#'
#' selectRowsCombinationWindow(doEraCollapseTH)
#' }
selectRowsCombinationWindow <- function(treatmentHistory) {
  # Order treatmentHistory by person_id, event_start_date, event_end_date
  treatmentHistory <- treatmentHistory[order(
    person_id, event_start_date, event_end_date), ]

  # Calculate gap with previous treatment
  treatmentHistory[, GAP_PREVIOUS := difftime(
      event_start_date, data.table::shift(
        event_end_date,
        type = "lag"),
      units = "days"),
    by = person_id]

  treatmentHistory$GAP_PREVIOUS <- as.integer(treatmentHistory$GAP_PREVIOUS)

  # Find all rows with gap_previous < 0
  treatmentHistory[
    treatmentHistory$GAP_PREVIOUS < 0,
    ALL_ROWS := which(treatmentHistory$GAP_PREVIOUS < 0)]

  # Select one row per iteration for each person
  rows <- treatmentHistory[
    !is.na(ALL_ROWS),
    head(.SD, 1),
    by = person_id]$ALL_ROWS

  treatmentHistory[rows, SELECTED_ROWS := 1]
  treatmentHistory[!rows, SELECTED_ROWS := 0]
  treatmentHistory[, ALL_ROWS := NULL]

  return(treatmentHistory)
}


#' doFilterTreatments
#'
#' Updates the treatmentHistory data.frame where the desired event cohorts are
#' maintained for the visualizations
#'
#' @param treatmentHistory
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param filterTreatments
#'     Select first occurrence of ('First') / changes between ('Changes') / all
#'     event cohorts ('All').
#'
#' @return treatmentHistory
#'     Updated dataframe, where the desired event cohorts are maintained for
#'     the visualizations.
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#'
#' doFilterTreatments(treatmentHistory = th, filterTreatments = "All")}
doFilterTreatments <- function(treatmentHistory, filterTreatments) {
  # Assertions
  checkmate::assertDataFrame(x = treatmentHistory)
  checkmate::assertChoice(
    x = filterTreatments,
    choices = c("First", "Changes", "All"),
    null.ok = FALSE
  )

  # Order treatmentHistory by person_id, event_start_date, event_end_date
  treatmentHistory <- treatmentHistory[
    order(person_id, event_start_date, event_end_date), ]

  if (filterTreatments != "All") {
    # Order the combinations
    ParallelLogger::logInfo("Order the combinations.")
    combi <- grep("+", treatmentHistory$event_cohort_id, fixed = TRUE)

    if (length(combi) != 0) {
      conceptIds <- strsplit(
        x = treatmentHistory$event_cohort_id[combi],
        split = "+",
        fixed = TRUE)

      treatmentHistory$event_cohort_id[combi] <- sapply(
        X = conceptIds,
        FUN = function(x) {
          paste(sort(x), collapse = "+")})
    }

    if (filterTreatments == "First") {
      treatmentHistory <- treatmentHistory[
        , head(.SD, 1),
        by = .(person_id, event_cohort_id)]

    } else if (filterTreatments == "Changes") {
      # Group all rows per person for which previous treatment is same
      tryCatch({
        treatmentHistory <- treatmentHistory[
          , group := rleid(person_id, event_cohort_id)]
        }, error = function(e) {
          print(paste0(
            "Check if treatmentHistory contains sufficient records: ", e))
          })

      # Remove all rows with same sequential treatments
      treatmentHistory <- treatmentHistory[
        , .(event_start_date = min(event_start_date),
            event_end_date = max(event_end_date),
            duration_era = sum(duration_era)),
        by = .(person_id, index_year, event_cohort_id, group)]

      treatmentHistory[, group := NULL]
    } else {
      warning(
        "filterTreatments input incorrect, return all event cohorts ('All')")
    }
  }

  ParallelLogger::logInfo(print(paste0(
    "After filterTreatments: ",
    nrow(treatmentHistory))))
  return(treatmentHistory)
}


#' doMaxPathLength
#'
#' Filters the treatmentHistory data.frame where eventSeq is smaller or equal
#' than maxPathLength
#'
#' @param treatmentHistory
#' Dataframe with event cohorts of the target cohort in different rows.
#' @param maxPathLength
#' Maximum number of steps included in treatment pathway.
#'
#' @return treatmentHistory
#' Updated dataframe, where the desired event cohorts all have a seq value of <=
#' maxPathLength
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#' doMaxPathLength(treatmentHistory = th, maxPathLength = 1)}
doMaxPathLength <- function(treatmentHistory, maxPathLength) {
  # Assertions
  checkmate::assertDataFrame(x = treatmentHistory)
  checkmate::assertNumeric(
    x = maxPathLength,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE
  )

  # Apply maxPathLength
  treatmentHistory <- treatmentHistory[event_seq <= maxPathLength, ]

  ParallelLogger::logInfo(print(paste0(
    "After maxPathLength: ", nrow(treatmentHistory))))
  return(treatmentHistory)
}

#' addLabels
#'
#' Adds back cohort names to concept ids.
#'
#' @param treatmentHistory treatmentHistory object
#' @param outputFolder Folder of output
#'
#' @return treatmentHistory
addLabels <- function(treatmentHistory, outputFolder) {
  labels <- read.csv(
      file = file.path(outputFolder, "cohortsToCreate.csv"))
  # convenrt event_cohort_id to character
  labels["cohortId"] <- as.character(labels[, "cohortId"])

  labels <- labels[labels$cohortType == "event", c("cohortId", "cohortName")]
  colnames(labels) <- c("event_cohort_id", "event_cohort_name")

  treatmentHistory <- merge(
    x = treatmentHistory,
    y = labels,
    all.x = TRUE,
    by = "event_cohort_id")

  treatmentHistory$event_cohort_name[
    is.na(treatmentHistory$event_cohort_name)] <- sapply(
      X = treatmentHistory$event_cohort_id[
        is.na(treatmentHistory$event_cohort_name)],
      FUN = function(x) {
    # Revert search to look for longest concept_ids first

    for (l in seq_len(nrow(labels))) {
      # If treatment occurs twice in a combination (as monotherapy and as part
      # of fixed-combination) -> remove monotherapy occurrence
      if (any(grep(labels$event_cohort_name[l], x))) {
        x <- gsub(labels$event_cohort_id[l], "", x)
      } else {
        x <- gsub(labels$event_cohort_id[l], labels$event_cohort_name[l], x)
      }
    }
    return(x)
  })

  # Filter out + at beginning/end or repetitions
  treatmentHistory$event_cohort_name <- gsub(
    pattern = "(^\\++|\\++$)",
    replacement = "+",
    x = treatmentHistory$event_cohort_name)
  return(treatmentHistory)
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
