
#' This function will construct treatment pathways.
#'
#' @param all_data Datatable with all target and event cohorts.
#' @param study_settings Object that contains all study settings inputted by the user.
#' @param databaseName  Name of the database that will appear in the results.
#' @param studyName Name for the study corresponding to the current settings.
#' @param outputFolder Name of local folder to place results; make sure to use forward slashes (/).
#' @export
constructPathways <- function(all_data,
                              study_settings,
                              databaseName,
                              studyName,
                              outputFolder,
                              tempFolder) {
  
  # For all different study settings
  settings <- colnames(study_settings)[grepl("analysis", colnames(study_settings))]
  
  for (s in settings) {
    studyName <- study_settings[study_settings$param == "studyName",s]
    
    if (!file.exists(paste0(tempFolder, "/", studyName)))
      dir.create(paste0(tempFolder, "/", studyName), recursive = TRUE)
    
    if (!file.exists(paste0(outputFolder, "/", studyName)))
      dir.create(paste0(outputFolder, "/",studyName), recursive = TRUE)
    
    ParallelLogger::logInfo(print(paste0("Constructing treatment pathways: ", studyName)))
    
    # Select cohorts included
    targetCohortId <- study_settings[study_settings$param == "targetCohortId",s]
    eventCohortIds <- study_settings[study_settings$param == "eventCohortIds",s]
    
    # Analysis settings
    includeTreatmentsPriorToIndex <- as.integer(study_settings[study_settings$param == "includeTreatmentsPriorToIndex",s]) # Number of days prior to the index date of the target cohort that event cohorts are allowed to start
    minEraDuration <-  as.integer(study_settings[study_settings$param == "minEraDuration",s]) # Minimum time an event era should last to be included in analysis
    splitEventCohorts <-  study_settings[study_settings$param == "splitEventCohorts",s] # Specify event cohort to split in acute (< 30 days) and therapy (>= 30 days)
    eraCollapseSize <-  as.integer(study_settings[study_settings$param == "eraCollapseSize",s]) # Window of time between which two eras of the same event cohort are collapsed into one era
    combinationWindow <-  as.integer(study_settings[study_settings$param == "combinationWindow",s]) # Window of time two event cohorts need to overlap to be considered a combination treatment
    minStepDuration <-  as.integer(study_settings[study_settings$param == "minStepDuration",s]) # Minimum time an event era before or after a generated combination treatment should last to be included in analysis
    filterTreatments <-  study_settings[study_settings$param == "filterTreatments",s] # Select first occurrence of / changes between / all event cohorts
    
    # Select target cohort
    select_people <- all_data$person_id[all_data$cohort_id == targetCohortId]
    data <- all_data[all_data$person_id %in% select_people, ]
    
    if (nrow(data) != 0) {
      # Add index year column based on start date target cohort
      targetCohort <- data[data$cohort_id %in% targetCohortId,,]
      targetCohort$index_year <- as.numeric(format(targetCohort$start_date, "%Y"))
      
      # Number of persons in target cohort (in total + per year)
      counts_targetcohort <- data.table::rollup(targetCohort, .N, by = c("index_year"))
      counts_targetcohort$index_year <- paste0("Number of persons in target cohort ", counts_targetcohort$index_year)
      
      # Select event cohorts for target cohort and merge with start/end date and index year
      eventCohorts <- data[data$cohort_id %in% unlist(strsplit(eventCohortIds, split = ",")),,]
      data <- merge(x = eventCohorts, y = targetCohort, by = c("person_id"), all.x = TRUE)
      
      # Only keep event cohorts after target cohort start date
      data <- data[data$start_date.y - as.difftime(includeTreatmentsPriorToIndex, unit="days") <= data$start_date.x & data$start_date.x < data$end_date.y,]
      
      # Remove unnecessary columns
      data <- data[,c("person_id","index_year", "cohort_id.x", "start_date.x", "end_date.x")]
      colnames(data) <- c("person_id","index_year", "event_cohort_id", "event_start_date", "event_end_date")
      
      # Calculate duration and gap same
      data[,duration_era:=difftime(event_end_date, event_start_date, units = "days")]
      
      data <- data[order(event_start_date, event_end_date),]
      data[,lag_variable:=shift(event_end_date, type = "lag"), by=c("person_id", "event_cohort_id")]
      data[,gap_same:=difftime(event_start_date, lag_variable, units = "days"),]
      data$lag_variable <- NULL
      
      # Apply analysis settings
      ParallelLogger::logInfo("Construct combinations, this may take a while for larger datasets.")
      writeLines(paste0("Original: ", nrow(data)))
      
      data <- doEraDuration(data, minEraDuration)
      if (!is.na(splitEventCohorts)) {data <- doSplitEventCohorts(data, splitEventCohorts, outputFolder)}
      data <- doEraCollapse(data, eraCollapseSize)
      
      time1 <- Sys.time()
      data <- doCombinationWindow(data, combinationWindow, minStepDuration)
      time2 <- Sys.time()
      ParallelLogger::logInfo(paste0("Time needed to execute combination window ", difftime(time2, time1, units = "mins")))
      
      # Order the combinations
      ParallelLogger::logInfo("Order the combinations.")
      combi <- grep("+", data$event_cohort_id, fixed=TRUE)
      if (length(combi) != 0) {
        concept_ids <- strsplit(data$event_cohort_id[combi], split="+", fixed=TRUE)
        data$event_cohort_id[combi] <- sapply(concept_ids, function(x) paste(sort(x), collapse = "+"))
      }
      
      data <- doFilterTreatments(data, filterTreatments)
      
      # Add event_seq
      ParallelLogger::logInfo("Adding drug sequence number.")
      data <- data[order(person_id, event_start_date, event_end_date),]
      data[, event_seq:=seq_len(.N), by= .(person_id)]
      
      # Add event_cohort_name
      ParallelLogger::logInfo("Adding concept names.")
      data <- addLabels(data, outputFolder)
      
      # Order the combinations
      ParallelLogger::logInfo("Ordering the combinations.")
      combi <- grep("+", data$event_cohort_name, fixed=TRUE)
      cohort_names <- strsplit(data$event_cohort_name[combi], split="+", fixed=TRUE)
      data$event_cohort_name[combi] <- sapply(cohort_names, function(x) paste(sort(x), collapse = "+"))
      
      # Reformat and save counts target cohort/treatment pathways 
      data$event_cohort_name <- unlist(data$event_cohort_name)
      write.csv(data, paste0(tempFolder, "/", studyName, "/", databaseName, "_", studyName, "_event_seq_processed.csv"), row.names = FALSE) 
      
      # Group based on treatments and rename columns
      data <- as.data.table(reshape2::dcast(data = data, person_id + index_year ~ event_seq, value.var = "event_cohort_name"))
      colnames(data)[3:ncol(data)] <- paste0("event_cohort_name", colnames(data)[3:ncol(data)])
      
      layers <- c(colnames(data))[3:min(7,ncol(data))] # max first 5
      data <- data[, .(freq=length((person_id))), by = c(layers, "index_year")]
      write.csv(data, paste0(tempFolder, "/", studyName, "/", databaseName, "_", studyName, "_paths.csv"), row.names = FALSE) 
      
      # Number of pathways (in total + per year)
      counts_pathways <- rollup(data, sum(freq), by = c("index_year"))
      counts_pathways$index_year <- paste0("Number of pathways (before minCellCount) in ", counts_pathways$index_year)
      
      # Calculate number of persons in target cohort / with pathways, in total / per year
      colnames(counts_pathways) <- colnames(counts_targetcohort)
      counts <- rbind(counts_targetcohort, counts_pathways)

      write.csv(counts, paste0(tempFolder, "/", studyName, "/", databaseName, "_", studyName, "_summary_cnt.csv"), row.names = FALSE)
    }
  }
}

# Input:
# data Dataframe with event cohorts of the target cohort in different rows.
# minEraDuration Minimum time an event era should last to be included in analysis.
#
# Output: Updated dataframe, rows with duration < minEraDuration filtered out.
doEraDuration <- function(data, minEraDuration) {
  data <- data[duration_era >= minEraDuration,]
  ParallelLogger::logInfo(print(paste0("After minEraDuration: ", nrow(data))))
  
  return(data)
}

# Input:
# data Dataframe with event cohorts of the target cohort in different rows.
# minStepDuration Minimum time an event era before or after a generated combination treatment should last to be included in analysis.
#
# Output: Updated dataframe, rows with duration_era < minStepDuration filtered out.
doStepDuration <- function(data, minStepDuration) {
  data <- data[(is.na(check_duration) | duration_era >= minStepDuration),]
  ParallelLogger::logInfo(print(paste0("After minStepDuration: ", nrow(data))))
  
  return(data)
}

# Input:
# data Dataframe with event cohorts of the target cohort in different rows.
# splitEventCohorts Specify event cohort to split in acute (< 30 days) and therapy (>= 30 days).
# outputFolder Name of local folder to place results; make sure to use forward slashes (/).
#
# Output: Updated dataframe, with specified event cohorts now split in two different event cohorts (acute and therapy).
doSplitEventCohorts <- function(data, splitEventCohorts, outputFolder) {
  
  # Load in labels cohorts
  labels <- data.table(readr::read_csv(paste(outputFolder, "/cohort.csv",sep=''), col_types = list("c", "c", "c", "c")))
  
  for (c in splitEventCohorts) {
    # Label as acute
    data[event_cohort_id == c & duration_era < 30, "event_cohort_id"] <- as.integer(paste0(c,1))
    
    # Label as therapy
    data[event_cohort_id == c & duration_era >= 30, "event_cohort_id"] <- as.integer(paste0(c,2))
    
    # Add new labels
    original <- labels[cohortId == as.integer(c),]
    
    new1 <- original
    new1$cohortId <- as.integer(paste0(c,1))
    new1$cohortName <- paste0(new1$cohortName, " (acute)")
    
    new2 <- original
    new2$cohortId <- as.integer(paste0(c,2))
    new2$cohortName <- paste0(new2$cohortName, " (therapy)")
    
    labels <- labels[cohortId != as.integer(c),]
    labels <- rbind(labels, new1, new2)
    
  }
  
  # Save new labels cohorts
  write.csv(labels, file.path(outputFolder, "cohort.csv"), row.names = FALSE)
  
  return(data) 
}

# Input:
# data Dataframe with event cohorts of the target cohort in different rows.
# eraCollapseSize Window of time between which two eras of the same event cohort are collapsed into one era.
#
# Output:
# Updated dataframe, where event cohorts with gap_same < eraCollapseSize are collapsed.
doEraCollapse <- function(data, eraCollapseSize) {
  # Order data by person_id, event_cohort_id, start_date, end_date
  data <- data[order(person_id, event_cohort_id,event_start_date, event_end_date),]
  
  # Find all rows with gap_same < eraCollapseSize
  rows <- which(data$gap_same < eraCollapseSize)
  
  # For all rows, modify the row preceding, loop backwards in case more than one collapse
  for (r in rev(rows)) {
    data[r - 1,"event_end_date"] <- data[r,event_end_date]
  }
  
  # Remove all rows with gap_same < eraCollapseSize
  data <- data[!rows,]
  data[,gap_same:=NULL]
  
  # Re-calculate duration_era
  data[,duration_era:=difftime(event_end_date , event_start_date, units = "days")]
  
  ParallelLogger::logInfo(print(paste0("After eraCollapseSize: ", nrow(data))))
  return(data)
}

# Input:
# data Dataframe with event cohorts of the target cohort in different rows.
# combinationWindow Window of time two event cohorts need to overlap to be considered a combination treatment.
# minStepDuration Minimum time an event era before or after a generated combination treatment should last to be included in analysis.
#
# Output:
# Updated dataframe, where overlapping event cohorts are modified according to rules defined for switching / combinations.
doCombinationWindow <- function(data, combinationWindow, minStepDuration) {
  data$event_cohort_id <- as.character(data$event_cohort_id)
  
  # Find which rows contain some overlap
  data <- selectRowsCombinationWindow(data)
  
  # While rows that need modification exist:
  iterations <- 1
  while(sum(data$SELECTED_ROWS)!=0) {
    
    # Which have gap previous shorter than combination window OR min(current duration era, previous duration era) -> add column switch
    data[SELECTED_ROWS == 1 & (-GAP_PREVIOUS < combinationWindow  & !(-GAP_PREVIOUS == duration_era | -GAP_PREVIOUS == shift(duration_era, type = "lag"))), switch:=1]
    
    # For rows selected not in column switch -> if data[r - 1, event_end_date] <= data[r, event_end_date] -> add column combination first received, first stopped
    data[SELECTED_ROWS == 1 & is.na(switch) & shift(event_end_date, type = "lag") <= event_end_date, combination_FRFS:=1]
    
    # For rows selected not in column switch -> if data[r - 1, event_end_date] > data[r, event_end_date] -> add column combination last received, first stopped
    data[SELECTED_ROWS == 1 & is.na(switch) & shift(event_end_date, type = "lag") > event_end_date, combination_LRFS:=1]
    
    ParallelLogger::logInfo(print(paste0("Iteration ", iterations, " modifying  ", sum(data$SELECTED_ROWS), " selected rows out of ", nrow(data), ": ", sum(!is.na(data$switch)) , " switches, ", sum(!is.na(data$combination_FRFS)), " combinations FRFS and ", sum(!is.na(data$combination_LRFS)), " combinations LRFS")))
    if (sum(!is.na(data$switch)) + sum(!is.na(data$combination_FRFS)) +  sum(!is.na(data$combination_LRFS)) != sum(data$SELECTED_ROWS)) {
      warning(paste0(sum(data$SELECTED_ROWS), ' does not equal total sum ', sum(!is.na(data$switch)) +  sum(!is.na(data$combination_FRFS)) +  sum(!is.na(data$combination_LRFS))))
    }
    
    # Do transformations for each of the three newly added columns
    # Construct helpers
    data[,event_start_date_next:=shift(event_start_date, type = "lead"),by=person_id]
    data[,event_end_date_previous:=shift(event_end_date, type = "lag"),by=person_id]
    data[,event_end_date_next:=shift(event_end_date, type = "lead"),by=person_id]
    data[,event_cohort_id_previous:=shift(event_cohort_id, type = "lag"),by=person_id]
    
    # Case: switch
    # Change end data of previous row -> no minStepDuration
    data[shift(switch, type = "lead")==1,event_end_date:=event_start_date_next]
    
    # Case: combination_FRFS
    # Add a new row with start date (r) and end date (r-1) as combination (copy current row + change end date + update concept id) -> no minStepDuration
    add_rows_FRFS <- data[combination_FRFS==1,]
    add_rows_FRFS[,event_end_date:=event_end_date_previous]
    add_rows_FRFS[,event_cohort_id:=paste0(event_cohort_id, "+", event_cohort_id_previous)]
    
    # Change end date of previous row -> check minStepDuration
    data[shift(combination_FRFS, type = "lead")==1,c("event_end_date","check_duration"):=list(event_start_date_next, 1)]
    
    # Change start date of current row -> check minStepDuration 
    data[combination_FRFS==1,c("event_start_date", "check_duration"):=list(event_end_date_previous,1)]
    
    # Case: combination_LRFS
    # Change current row to combination -> no minStepDuration
    data[combination_LRFS==1,event_cohort_id:=paste0(event_cohort_id, "+", event_cohort_id_previous)]
    
    # Add a new row with end date (r) and end date (r-1) to split drug era (copy previous row + change end date) -> check minStepDuration 
    add_rows_LRFS <- data[shift(combination_LRFS, type = "lead")==1,]
    add_rows_LRFS[,c("event_start_date", "check_duration"):=list(event_end_date_next,1)]
    
    # Change end date of previous row -> check minStepDuration 
    data[shift(combination_LRFS, type = "lead")==1,c("event_end_date", "check_duration"):=list(event_start_date_next,1)]
    
    # Combine all rows and remove helper columns
    data <- rbind(data, add_rows_FRFS, fill=TRUE)
    data <- rbind(data, add_rows_LRFS)
    
    # Re-calculate duration_era
    data[,duration_era:=difftime(event_end_date, event_start_date, units = "days")]
    
    data <- doStepDuration(data, minStepDuration)
    
    data <- data[,c("person_id", "index_year", "event_cohort_id", "event_start_date", "event_end_date", "duration_era")]
    
    data <- selectRowsCombinationWindow(data)
    iterations <- iterations + 1
    
    gc()
  }
  
  ParallelLogger::logInfo(print(paste0("After combinationWindow: ", nrow(data))))
  
  data[,GAP_PREVIOUS:=NULL]
  data[,SELECTED_ROWS:=NULL]
  
  return(data)
}

# Help function for doCombinationWindow that selects one overlapping drug era per person to modify in next iteration of combination window. 
selectRowsCombinationWindow <- function(data) {
  # Order data by person_id, event_start_date, event_end_date
  data <- data[order(person_id, event_start_date, event_end_date),]
  
  # Calculate gap with previous treatment
  data[,GAP_PREVIOUS:=difftime(event_start_date, shift(event_end_date, type = "lag"), units = "days"), by = person_id]
  data$GAP_PREVIOUS <- as.integer(data$GAP_PREVIOUS)
  
  # Find all rows with gap_previous < 0
  data[data$GAP_PREVIOUS < 0, ALL_ROWS:=which(data$GAP_PREVIOUS < 0)]
  
  # Select one row per iteration for each person
  rows <- data[!is.na(ALL_ROWS),head(.SD,1), by=person_id]$ALL_ROWS
  
  data[rows,SELECTED_ROWS:=1]
  data[!rows,SELECTED_ROWS:=0]
  data[,ALL_ROWS:=NULL]
  
  return(data)
}

# Input:
# data Dataframe with event cohorts of the target cohort in different rows.
# filterTreatments Select first occurrence of / changes between / all event cohorts.
#
# Updated dataframe, where the desired event cohorts are maintained for the visualizations.
doFilterTreatments <- function(data, filterTreatments) {
  
  # Order data by person_id, event_start_date, event_end_date
  data <- data[order(person_id, event_start_date, event_end_date),]
  
  if (filterTreatments == "First") {
    data <- data[, head(.SD,1), by=.(person_id, event_cohort_id)]
    
  } else if (filterTreatments == "Changes") {
    
    # Group all rows per person for which previous treatment is same
    data <- data[, group:=rleid(person_id,event_cohort_id)]
    
    # Remove all rows with same sequential treatments
    data <- data[,.(event_start_date=min(event_start_date), event_end_date=max(event_end_date), duration_era=sum(duration_era)), by = .(person_id,index_year,event_cohort_id,group)]
    
    data[,group:=NULL]
    
  } else if (filterTreatments == "All") {} # Do nothing
  
  ParallelLogger::logInfo(print(paste0("After filterTreatments: ", nrow(data))))
  
  return(data)
}

# Adds back cohort names to concept ids.
addLabels <- function(data, outputFolder) {
  labels <- data.frame(readr::read_csv(paste(outputFolder, "/cohort.csv",sep=''), col_types = list("c", "c", "c", "c")))
  labels <- labels[labels$cohortType == "event",c("cohortId", "cohortName")]
  colnames(labels) <- c("event_cohort_id", "event_cohort_name")
  
  data <- merge(data, labels, all.x = TRUE, by = "event_cohort_id")
  
  data$event_cohort_name[is.na(data$event_cohort_name)] <- sapply(data$event_cohort_id[is.na(data$event_cohort_name)], function(x) {
    
    # Revert search to look for longest concept_ids first
    for (l in nrow(labels):1)
    {
      
      # If treatment occurs twice in a combination (as monotherapy and as part of fixed-combination) -> remove monotherapy occurrence
      if (any(grep(labels$event_cohort_name[l], x))) {
        x <- gsub(labels$event_cohort_id[l], "", x)
      } else {
        x <- gsub(labels$event_cohort_id[l], labels$event_cohort_name[l], x)
      }
    }
    
    return(x)
  })
  
  # Filter out + at beginning/end or repetitions
  data$event_cohort_name <- gsub("\\++", "+", data$event_cohort_name)
  data$event_cohort_name <- gsub("^\\+", "", data$event_cohort_name)
  data$event_cohort_name <- gsub("\\+$", "", data$event_cohort_name)
  
  return(data)
}
