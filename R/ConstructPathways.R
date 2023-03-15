#' checkConstructPathways
#' 
#' Checks parameters for constructPathways.
#' 
#' @import checkmate
#' 
#' @param dataSettings dataSettings object
#' @param pathwaySettings pathwaySettings object
#' @param saveSettings saveSettings object
#'
#' @return TRUE if all assertions pass
checkConstructPathways <- function(dataSettings, 
                                   pathwaySettings, 
                                   saveSettings) {
  # dataSettings
  checkmate::assert(
    checkmate::checkClass(dataSettings, "dataSettings"),
    checkmate::checkClass(dataSettings$connectionDetails, "connectionDetails"),
    checkmate::checkCharacter(dataSettings$connectionDetails$dbms, len = 1),
    checkmate::checkCharacter(dataSettings$cdmDatabaseSchema, len = 1),
    checkmate::checkCharacter(dataSettings$resultSchema, len = 1),
    checkmate::checkCharacter(dataSettings$cohortTable, len = 1))
  
  # pathwaySettings
  checkmate::assert(
    checkmate::checkClass(pathwaySettings, "pathwaySettings"),
    checkmate::checkDataFrame(pathwaySettings$all_settings, nrows = 17))
  
  # saveSettings
  checkmate::assert(
    checkmate::checkClass(saveSettings, "saveSettings"),
    checkmate::checkCharacter(saveSettings$databaseName, len = 1),
    checkmate::checkDirectory(saveSettings$rootFolder),
    checkmate::checkDirectory(saveSettings$outputFolder),
    checkmate::checkDirectory(saveSettings$tempFolder)
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
#' @importFrom data.table as.data.table rollup
#' @importFrom DatabaseConnector connect disconnect
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
  check <- checkConstructPathways(dataSettings, 
                                  pathwaySettings, 
                                  saveSettings)

  if (check) {
    # do stuff
    message("check passed")
  }
  
  # Load already created cohorts
  # Connect to database
  connection <- DatabaseConnector::connect(dataSettings$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Get cohorts from database
  full_cohorts <- data.table::as.data.table(extractFile(
    connection, 
    dataSettings$cohortTable, 
    dataSettings$resultSchema, 
    dataSettings$connectionDetails$dbms))

  colnames(full_cohorts) <- c("cohort_id", 
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
    tempFolder_s <- file.path(saveSettings$tempFolder, studyName)
    if (!file.exists(tempFolder_s))
      dir.create(tempFolder_s, 
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
    select_people <- full_cohorts$person_id[
      full_cohorts$cohort_id == targetCohortId]

    current_cohorts <- full_cohorts[
      full_cohorts$person_id %in% select_people, ]

    if (nrow(current_cohorts) != 0) {
      # Preprocess the target/event cohorts to create treatment history
      treatment_history <- doCreateTreatmentHistory(
        current_cohorts, 
        targetCohortId, 
        eventCohortIds, 
        periodPriorToIndex, 
        includeTreatments)

      # Apply pathway settings to create treatment pathways
      ParallelLogger::logInfo(paste(
        "Construct treatment pathways, this may",
        "take a while for larger datasets."))

      writeLines(paste0("Original number of rows: ", nrow(treatment_history)))

      # TODO: check what happens if treatment_history zero or few rows
      # (throw errors)

      treatment_history <- doEraDuration(
        treatment_history, 
        minEraDuration)

      treatment_history <- doSplitEventCohorts(
        treatment_history,
        splitEventCohorts,
        splitTime,
        saveSettings$outputFolder)

      treatment_history <- doEraCollapse(
        treatment_history,
        eraCollapseSize)

      treatment_history <- doCombinationWindow(
        treatment_history,
        combinationWindow,
        minPostCombinationDuration)

      treatment_history <- doFilterTreatments(
        treatment_history,
        filterTreatments)

      if (nrow(treatment_history) != 0) {
        # Add event_seq number to determine order of treatments in pathway
        ParallelLogger::logInfo("Adding drug sequence number.")
        treatment_history <- treatment_history[
          order(person_id, event_start_date, event_end_date), ]

        treatment_history[, event_seq := seq_len(.N), by = .(person_id)]

        treatment_history <- doMaxPathLength(
          treatment_history, 
          maxPathLength)

        # Add event_cohort_name (instead of only event_cohort_id)
        ParallelLogger::logInfo("Adding concept names.")
        
        treatment_history <- addLabels(
          treatment_history, 
          saveSettings$outputFolder)
     
        # Order the combinations
        ParallelLogger::logInfo("Ordering the combinations.")
        combi <- grep(
          pattern = "+", 
          x = treatment_history$event_cohort_name, 
          fixed = TRUE)
       
        cohort_names <- strsplit(
          x = treatment_history$event_cohort_name[combi], 
          split = "+", 
          fixed = TRUE)
      
        treatment_history$event_cohort_name[combi] <- sapply(
          X = cohort_names, 
          FUN = function(x) {
            paste(sort(x), collapse = "+")})
       
        treatment_history$event_cohort_name <- unlist(
          treatment_history$event_cohort_name)
      }

      # Save the processed treatment history
      write.csv(treatment_history, file.path(
        tempFolder_s,
        paste0(
          saveSettings$databaseName,
          "_",
          studyName,
          "_event_seq_processed.csv")),
        row.names = FALSE)

      # Save the treatment pathways
      if (nrow(treatment_history) != 0) {
        treatment_pathways <- data.table::as.data.table(
          reshape2::dcast(
            data = treatment_history, 
            formula = person_id + index_year ~ event_seq, 
            value.var = "event_cohort_name"))

        colnames(treatment_pathways)[3:ncol(treatment_pathways)] <- paste0(
          "event_cohort_name", 
          colnames(treatment_pathways)[3:ncol(treatment_pathways)])

        layers <- c(colnames(treatment_pathways))[
          3:min(7, ncol(treatment_pathways))] # max first 5

        treatment_pathways <- treatment_pathways[
          , .(freq = length((person_id))), by = c(layers, "index_year")]

        write.csv(
          x = treatment_pathways, 
          file = file.path(
            tempFolder_s, 
            paste0(
              saveSettings$databaseName, 
              "_", studyName, 
              "_paths.csv")), 
          row.names = FALSE) 

        # Calculate counts of the number of persons in target cohort / with 
        # pathways, in total / per year
        targetCohort <- current_cohorts[
          current_cohorts$cohort_id %in% targetCohortId,,]

        targetCohort$index_year <- as.numeric(format(
          targetCohort$start_date,
          "%Y"))

        counts_targetcohort <- data.table::rollup(
          targetCohort,
          .N,
          by = c("index_year"))

        counts_targetcohort$index_year <- paste0(
          "Number of persons in target cohort ",
          counts_targetcohort$index_year)

        counts_pathways <- rollup(
          treatment_pathways,
          sum(freq),
          by = c("index_year"))

        counts_pathways$index_year <- paste0(
          "Number of pathways (before minCellCount) in ",
          counts_pathways$index_year)

        colnames(counts_pathways) <- colnames(counts_targetcohort)
        counts <- rbind(counts_targetcohort, counts_pathways)

        write.csv(
          counts,
          file.path(
            tempFolder_s,
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
#' @importFrom data.table shift :=
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
  checkmate::assert(checkmate::check_data_frame(currentCohorts, min.cols = 4, col.names = "named"))
  checkmate::assert(checkmate::checkNames(names(currentCohorts), permutation.of = c("cohort_id", "person_id", "start_date", "end_date")))
  
  checkmate::assert(checkmate::checkCharacter(targetCohortId, len = 1))
  checkmate::assert(checkmate::checkCharacter(eventCohortIds))
  checkmate::assert(checkmate::checkInt(periodPriorToIndex))

  # Add index year column based on start date target cohort
  targetCohort <- currentCohorts[
    currentCohorts$cohort_id %in% targetCohortId,, ]

  targetCohort$index_year <- as.numeric(format(targetCohort$start_date, "%Y"))

  # Select event cohorts for target cohort and merge with start/end date and 
  # index year
  eventCohorts <- currentCohorts[
    currentCohorts$cohort_id %in% eventCohortIds,, ]

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
#' @param treatment_history
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param minEraDuration
#'     Minimum time an event era should last to be included in analysis.
#' 
#' @import checkmate
#' @import ParallelLogger
#' 
#' @return treatment_history
#'     Updated dataframe, rows with duration < 
#'     minEraDuration filtered out.
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#' doEraDuration(treatment_history = th, minEraDuration = 1)
#' }
doEraDuration <- function(treatment_history, minEraDuration) {
  # Assertions
  checkmate::assertDataFrame(x = treatment_history)
  checkmate::assertNumeric(
    x = minEraDuration,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE
  )
  
  treatment_history <- treatment_history[duration_era >= minEraDuration, ]
  ParallelLogger::logInfo(print(
    paste0("After minEraDuration: ", nrow(treatment_history))))
  return(treatment_history)
}


#' doStepDuration
#'
#' Filters treatmentHistory based on if durationEra is smaller than the
#' specified minimum post combination duration (minPostCombinationDuration). 
#'
#' @param treatment_history
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param minPostCombinationDuration
#'     Minimum time an event era should last before or after a generated combination
#'     treatment for it to be included in analysis.
#' 
#' @import glue
#' @import checkmate
#' @import ParallelLogger
#'
#' @return treatment_history
#'     Updated dataframe, rows with duration_era < 
#'     minPostCombinationDuration filtered out.
doStepDuration <- function(treatment_history, minPostCombinationDuration) {
  # Assertions
  checkmate::assertDataFrame(x = treatment_history)
  checkmate::assertNumeric(
    x = minPostCombinationDuration,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE
  )
  
  treatment_history <- subset(
    x = treatment_history,
    duration_era >= minPostCombinationDuration | is.na(duration_era))
  
  ParallelLogger::logInfo(
    glue::glue("After minPostCombinationDuration: {nrow(treatment_history)}"))
  return(treatment_history)
}


#' doSplitEventCohorts
#'
#' Splits the treatmentHistory data.frame based on event cohorts into ‘acute’
#' and ‘therapy’ cohorts. 
#'
#' @param treatment_history
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
#' @import checkmate
#' @importFrom data.table data.table
#'
#' @return treatment_history
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
#'   treatment_history = doEraDurationTH,
#'   splitEventCohorts = c(1,2,3),
#'   splitTime = c("30", "20", "10"),
#'   outputFolder = saveSettings$outputFolder)}
doSplitEventCohorts <- function(
    treatment_history, 
    splitEventCohorts,
    splitTime,
    outputFolder) {
  
  if (all(!is.na(splitEventCohorts))) {
    # Load in labels cohorts
    labels <- data.table::data.table(readr::read_csv(
      file = file.path(outputFolder, "cohortsToCreate.csv"), 
      col_types = list("i","c","c")))
    
    # Check if splitEventCohorts == splitTime
    checkmate::assertTRUE(length(splitEventCohorts) == length(splitTime))
    
    for (c in seq_len(length(splitEventCohorts))) {
      cohort <- splitEventCohorts[c]
      cutoff <- splitTime[c]
      
      # Label as acute
      treatment_history[
        event_cohort_id == cohort &
          duration_era <
          cutoff, "event_cohort_id"] <- as.integer(paste0(cohort,1))
      
      # Label as therapy
      treatment_history[
        event_cohort_id == cohort &
          duration_era >= cutoff,
        "event_cohort_id"] <- as.integer(paste0(cohort, 2))
      
      # Add new labels
      original <- labels[cohortId == as.integer(cohort), ]
      
      acute <- original
      acute$cohortId <- as.integer(paste0(cohort,1))
      acute$cohortName <- paste0(acute$cohortName, " (acute)")
      
      therapy <- original
      therapy$cohortId <- as.integer(paste0(cohort, 2))
      therapy$cohortName <- paste0(therapy$cohortName, " (therapy)")
      
      labels <- labels[cohortId != as.integer(cohort), ]
      labels <- rbind(labels, acute, therapy)
    }
  }
  return(treatment_history)
}


#' doEraCollapse
#' 
#' Updates the treatmentHistory data.frame where if gapSame is smaller than the specified era collapse size (eraCollapseSize) are collapsed
#'
#' @param treatment_history
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param eraCollapseSize
#'     Window of time between which two eras of the same event cohort are
#'     collapsed into one era.
#' 
#' @import checkmate
#' @import ParallelLogger
#' 
#' @return treatment_history
#'     Updated dataframe, where event cohorts with
#'     gap_same < eraCollapseSize are collapsed.
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#' doEraCollapse(treatment_history = th, eraCollapseSize = 1)
#' }
doEraCollapse <- function(treatment_history, eraCollapseSize) {
  # Assertions
  checkmate::assertDataFrame(x = treatment_history)
  checkmate::assertNumeric(
    x = eraCollapseSize,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE
  )
  
  # Order treatment_history by person_id, event_cohort_id, start_date, end_date
  treatment_history <- treatment_history[
    order(person_id, event_cohort_id, event_start_date, event_end_date), ]
  
  # Find all rows with gap_same < eraCollapseSize
  rows <- which(treatment_history$gap_same < eraCollapseSize)
  
  # For all rows, modify the row preceding, loop backwards in case more than
  # one collapse
  for (r in rev(rows)) {
    treatment_history[r - 1, "event_end_date"] <- treatment_history[
      r,
      event_end_date]
  }
  
  # Remove all rows with gap_same < eraCollapseSize
  treatment_history <- treatment_history[!rows, ]
  treatment_history[, gap_same := NULL]
  
  # Re-calculate duration_era
  treatment_history[, duration_era := difftime(
    time1 = event_end_date, 
    time2 = event_start_date, 
    units = "days")]
  
  ParallelLogger::logInfo(print(paste0(
    "After eraCollapseSize: ",
    nrow(treatment_history))))
  return(treatment_history)
}


#' Combine overlapping events into combinations
#' 
#' doCombinationWindow is an internal function that combines overlapping events 
#' into combination events. It accepts a treatment_history dataframe and returns
#' a modified treatment_history dataframe. The returned treatment_history 
#' dataframe always has the property that a person is only in one event cohort,
#' which might be a combination event cohort, at any point time.
#'
#' @param treatment_history
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
#' @importFrom data.table shift
#'
#' @return A treatment_history dataframe with the columns event_cohort_id,
#'     person_id, event_start_date, event_end_date. event_cohort_id will be 
#'     of character type and combination events will have a new event_cohort_id 
#'     made up of the concatenated event_cohort_ids of each combined 
#'     event_cohort_id. When two events overlap for more than 
#'     `combinationWindow` days they will be collapsed into a single combination
#'     event. Events are collapsed iteratively starting with the first two 
#'     overlapping events per person and continuing until no more overlapping
#'     events exist in the treatment_history.
doCombinationWindow <- function(
    treatment_history, 
    combinationWindow, 
    minPostCombinationDuration) {
  time1 <- Sys.time()
  
  treatment_history$event_cohort_id <- as.character(
    treatment_history$event_cohort_id)
  
  # Find which rows contain some overlap
  treatment_history <- selectRowsCombinationWindow(treatment_history)
  
  # While rows that need modification exist:
  iterations <- 1
  while (sum(treatment_history$SELECTED_ROWS) != 0) {
    
    # Which rows have gap previous shorter than combination window OR 
    # min(current duration era, previous duration era) -> add column switch
    treatment_history[
      SELECTED_ROWS == 1 & 
        (-GAP_PREVIOUS < combinationWindow & 
           !(-GAP_PREVIOUS == duration_era |
               -GAP_PREVIOUS == data.table::shift(duration_era, type = "lag"))), 
      switch := 1]
    
    # For rows selected not in column switch ->
    # if treatment_history[r - 1, event_end_date] <= 
    # treatment_history[r, event_end_date] -> 
    # add column combination first received, first stopped
    treatment_history[
      SELECTED_ROWS == 1 &
        is.na(switch) &
        data.table::shift(event_end_date, type = "lag") <= event_end_date, 
      combination_FRFS := 1]
    
    # For rows selected not in column switch ->
    # if treatment_history[r - 1, event_end_date] >
    # treatment_history[r, event_end_date] ->
    # add column combination last received, first stopped
    treatment_history[
      SELECTED_ROWS == 1 &
        is.na(switch) &
        data.table::shift(event_end_date, type = "lag") >
        event_end_date, combination_LRFS := 1]
    
    ParallelLogger::logInfo(print(paste0(
      "Iteration ", iterations,
      " modifying  ", sum(treatment_history$SELECTED_ROWS),
      " selected rows out of ",
      nrow(treatment_history), ": ",
      sum(!is.na(treatment_history$switch)),
      " switches, ", sum(!is.na(treatment_history$combination_FRFS)),
      " combinations FRFS and ",
      sum(!is.na(treatment_history$combination_LRFS)),
      " combinations LRFS")))
    
    sumSwitchComb <- sum(
      sum(!is.na(treatment_history$switch)), 
      sum(!is.na(treatment_history$combination_FRFS)),
      sum(!is.na(treatment_history$combination_LRFS)))
    
    sumSelectedRows <- sum(treatment_history$SELECTED_ROWS)
    
    if (sumSwitchComb != sumSelectedRows) {
      warning(paste0(
        sum(treatment_history$SELECTED_ROWS),
        ' does not equal total sum ',
        sum(!is.na(treatment_history$switch)) + 
          sum(!is.na(treatment_history$combination_FRFS)) + 
          sum(!is.na(treatment_history$combination_LRFS))))
    }
    
    # Do transformations for each of the three newly added columns
    # Construct helpers
    treatment_history$event_start_date_next <- 
      treatment_history[,
        data.table::shift(event_start_date, type = "lead"),
        by = person_id][, 2]
    
    treatment_history$event_end_date_previous <-
      treatment_history[,
        data.table::shift(event_end_date, type = "lag"),
        by = person_id][, 2]
    
    treatment_history$event_end_date_next <-
      treatment_history[,
        data.table::shift(event_end_date, type = "lead"),
        by = person_id][, 2]
    
    treatment_history$event_cohort_id_previous <-
      treatment_history[,
        data.table::shift(event_cohort_id, type = "lag"),
        by = person_id][, 2]
    
    # treatment_history[, `:=`(
    #   event_start_date_next = data.table::shift(event_start_date, type = "lead"),
    #   event_end_date_previous = data.table::shift(event_end_date, type = "lag"),
    #   event_end_date_next = data.table::shift(event_end_date, type = "lead"),
    #   event_cohort_id_previous = data.table::shift(event_cohort_id, type = "lag")
    # ), by = person_id]
    
    # Case: switch
    # Change end treatment_history of previous row ->
    # no minPostCombinationDuration
    treatment_history[data.table::shift(
      switch, 
      type = "lead") == 1,
      event_end_date := event_start_date_next]
    
    # Case: combination_FRFS
    # Add a new row with start date (r) and end date (r-1) as combination (copy
    # current row + change end date + update concept id) -> no
    # minPostCombinationDuration
    add_rows_FRFS <- treatment_history[combination_FRFS == 1, ]
    add_rows_FRFS[, event_end_date := event_end_date_previous]
    
    add_rows_FRFS[, event_cohort_id := paste0(
      event_cohort_id, "+", event_cohort_id_previous)]
    
    # Change end date of previous row -> check minPostCombinationDuration
    treatment_history[
      data.table::shift(combination_FRFS, type = "lead") == 1,
      c("event_end_date","check_duration") := list(event_start_date_next, 1)]
    
    # Change start date of current row -> check minPostCombinationDuration 
    treatment_history[
      combination_FRFS == 1,
      c("event_start_date", "check_duration") := list(
        event_end_date_previous, 1)]
    
    # Case: combination_LRFS
    # Change current row to combination -> no minPostCombinationDuration
    treatment_history[
      combination_LRFS == 1,
      event_cohort_id := paste0(
        event_cohort_id, "+", event_cohort_id_previous)]
    
    # Add a new row with end date (r) and end date (r-1) to split drug era 
    # (copy previous row + change end date) -> check minPostCombinationDuration
    add_rows_LRFS <- treatment_history[
      data.table::shift(combination_LRFS, type = "lead") == 1, ]
    
    add_rows_LRFS[
      , c("event_start_date", "check_duration") := list(
        event_end_date_next, 1)]
    
    # Change end date of previous row -> check minPostCombinationDuration 
    treatment_history[
      data.table::shift(combination_LRFS, type = "lead") == 1,
      c("event_end_date", "check_duration") := list(event_start_date_next, 1)]
    
    # Combine all rows and remove helper columns
    treatment_history <- rbind(treatment_history, add_rows_FRFS, fill = TRUE)
    treatment_history <- rbind(treatment_history, add_rows_LRFS)
    
    # Re-calculate duration_era
    treatment_history[
      , duration_era := difftime(
        event_end_date, event_start_date, units = "days")]
    
    # Check duration drug eras before/after generated combination treatments
    treatment_history <- doStepDuration(
      treatment_history, minPostCombinationDuration)
    
    # Preparations for next iteration
    treatment_history <- treatment_history[
      ,c("person_id", "index_year", "event_cohort_id", 
         "event_start_date", "event_end_date", "duration_era")]
    
    treatment_history <- selectRowsCombinationWindow(treatment_history)
    iterations <- iterations + 1
    
    gc()
  }
  
  ParallelLogger::logInfo(print(paste0(
    "After combinationWindow: ", nrow(treatment_history))))
  
  treatment_history[, GAP_PREVIOUS := NULL]
  treatment_history[, SELECTED_ROWS := NULL]
  
  time2 <- Sys.time()
  ParallelLogger::logInfo(paste0(
    "Time needed to execute combination window ",
    difftime(time2, time1, units = "mins")))
  
  return(treatment_history)
}


#' selectRowsCombinationWindow
#' 
#' Help function for doCombinationWindow that selects one overlapping drug era
#' per person to modify in next iteration of the combination window. 
#'
#' @param treatment_history
#'   Dataframe with event cohorts of the target cohort in different rows. 
#'
#' @return Updated treatment_history data.frame
#' 
#' @examples \dontrun{
#' source(system.file(
#'   package = "TreatmentPatterns", 
#'  "testing", "testParams.R"))
#'  
#' selectRowsCombinationWindow(doEraCollapseTH)
#' }
selectRowsCombinationWindow <- function(treatment_history) {
  # Order treatment_history by person_id, event_start_date, event_end_date
  treatment_history <- treatment_history[order(
    person_id, event_start_date, event_end_date), ]
  
  # Calculate gap with previous treatment
  treatment_history[, GAP_PREVIOUS := difftime(
      event_start_date, data.table::shift(event_end_date, type = "lag"),units = "days"),
    by = person_id]
  
  treatment_history$GAP_PREVIOUS <- as.integer(treatment_history$GAP_PREVIOUS)
  
  # Find all rows with gap_previous < 0
  treatment_history[
    treatment_history$GAP_PREVIOUS < 0,
    ALL_ROWS := which(treatment_history$GAP_PREVIOUS < 0)]
  
  # Select one row per iteration for each person
  rows <- treatment_history[
    !is.na(ALL_ROWS),
    head(.SD,1),
    by = person_id]$ALL_ROWS
  
  treatment_history[rows, SELECTED_ROWS := 1]
  treatment_history[!rows, SELECTED_ROWS := 0]
  treatment_history[, ALL_ROWS := NULL]
  
  return(treatment_history)
}


#' doFilterTreatments
#' 
#' Updates the treatmentHistory data.frame where the desired event cohorts are maintained for the visualizations
#'
#' @param treatment_history
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param filterTreatments 
#'     Select first occurrence of ('First') / changes between ('Changes') / all
#'     event cohorts ('All').
#'
#' @import checkmate
#' @import ParallelLogger     
#'
#' @return treatment_history
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
#' doFilterTreatments(treatment_history = th, filterTreatments = "All")}
doFilterTreatments <- function(treatment_history, filterTreatments) {
  # Assertions
  checkmate::assertDataFrame(x = treatment_history)
  checkmate::assertChoice(
    x = filterTreatments,
    choices = c("First", "Changes", "All"),
    null.ok = FALSE
  )
  
  # Order treatment_history by person_id, event_start_date, event_end_date
  treatment_history <- treatment_history[
    order(person_id, event_start_date, event_end_date), ]
  
  if (filterTreatments != "All") {
    # Order the combinations
    ParallelLogger::logInfo("Order the combinations.")
    combi <- grep("+", treatment_history$event_cohort_id, fixed = TRUE)
    
    if (length(combi) != 0) {
      concept_ids <- strsplit(
        x = treatment_history$event_cohort_id[combi],
        split = "+",
        fixed = TRUE)
      
      treatment_history$event_cohort_id[combi] <- sapply(
        X = concept_ids,
        FUN = function(x) {
          paste(sort(x), collapse = "+")})
    }
    
    if (filterTreatments == "First") {
      treatment_history <- treatment_history[
        , head(.SD, 1), 
        by = .(person_id, event_cohort_id)]
      
    } else if (filterTreatments == "Changes") {
      # Group all rows per person for which previous treatment is same
      tryCatch({
        treatment_history <- treatment_history[
          , group := rleid(person_id, event_cohort_id)]
        }, error = function(e){
          print(paste0(
            "Check if treatment_history contains sufficient records: ", e))
          })
      
      # Remove all rows with same sequential treatments
      treatment_history <- treatment_history[
        , .(event_start_date = min(event_start_date),
            event_end_date = max(event_end_date),
            duration_era = sum(duration_era)),
        by = .(person_id,index_year,event_cohort_id,group)]
      
      treatment_history[, group := NULL]
    } else {
      warning(
        "filterTreatments input incorrect, return all event cohorts ('All')")
    }
  }
  
  ParallelLogger::logInfo(print(paste0(
    "After filterTreatments: ",
    nrow(treatment_history))))
  return(treatment_history)
}


#' doMaxPathLength
#' 
#' Filters the treatmentHistory data.frame where eventSeq is smaller or equal than maxPathLength
#'
#' @param treatment_history
#' Dataframe with event cohorts of the target cohort in different rows.
#' @param maxPathLength
#' Maximum number of steps included in treatment pathway.
#'
#' @import checkmate
#' @import ParallelLogger
#' 
#' @return treatment_history
#' Updated dataframe, where the desired event cohorts all have a seq value of <= 
#' maxPathLength
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#' doMaxPathLength(treatment_history = th, maxPathLength = 1)}
doMaxPathLength <- function(treatment_history, maxPathLength) {
  # Assertions
  checkmate::assertDataFrame(x = treatment_history)
  checkmate::assertNumeric(
    x = maxPathLength,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE
  )
  
  # Apply maxPathLength
  treatment_history <- treatment_history[event_seq <= maxPathLength, ]
  
  ParallelLogger::logInfo(print(paste0(
    "After maxPathLength: ", nrow(treatment_history))))
  return(treatment_history)
}

#' addLabels
#'
#' Adds back cohort names to concept ids.
#'
#' @param treatment_history treatment_history object
#' @param outputFolder Folder of output
#' 
#' @import utils
#'
#' @return treatment_history
addLabels <- function(treatment_history, outputFolder) {
  labels <- read.csv(
      file = file.path(outputFolder, "cohortsToCreate.csv"))
  # convenrt event_cohort_id to character
  labels["cohortId"] <- as.character(labels[, "cohortId"])
  
  labels <- labels[labels$cohortType == "event",c("cohortId", "cohortName")]
  colnames(labels) <- c("event_cohort_id", "event_cohort_name")
  
  treatment_history <- merge(
    x = treatment_history,
    y = labels,
    all.x = TRUE,
    by = "event_cohort_id")
  
  treatment_history$event_cohort_name[
    is.na(treatment_history$event_cohort_name)] <- sapply(
      X = treatment_history$event_cohort_id[
        is.na(treatment_history$event_cohort_name)],
      FUN = function(x) {
    # Revert search to look for longest concept_ids first
    
    for (l in nrow(labels):1) {
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
  treatment_history$event_cohort_name <- gsub(
    pattern = "(^\\++|\\++$)",
    replacement = "+",
    x = treatment_history$event_cohort_name)
  return(treatment_history)
}
utils::globalVariables(c(".", "..columns", "..l", "..layers", ".N", ".SD", "ALL_ROWS", "COUNT", "GAP_PREVIOUS",
    "SELECTED_ROWS", "all_combinations", "cohortId",  "combination", "combination_FRFS",
    "combination_LRFS", "duration_era", "event_cohort_id", 
    "event_cohort_id_previous", "event_cohort_name", "event_cohort_name1",
    "event_cohort_name2", "event_cohort_name3", "event_end_date",
    "event_end_date_next", "event_end_date_previous", "event_seq",
    "event_start_date", "event_start_date_next", "fixed_combinations", "freq",
    "gap_same", "group", "index_year", "lag_variable", "monotherapy", "person_id", "rleid"))