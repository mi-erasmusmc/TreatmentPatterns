
#' Construct treatment pathways.
#'
#' @param dataSettings Settings object as created by createDataSettings().
#' @param pathwaySettings Settings object as created by createPathwaySettings().
#' @param saveSettings Settings object as created by createSaveSettings().
#'
#' @export
constructPathways <- function(dataSettings, pathwaySettings, saveSettings) {
  
  # Check if inputs correct
  if(!class(dataSettings)%in%c('dataSettings')){
    stop('Incorrect class for dataSettings')
  } 
  
  if(!class(pathwaySettings)%in%c('pathwaySettings')){
    stop('Incorrect class for pathwaySettings')
  } 
  
  if(!class(saveSettings)%in%c('saveSettings')){
    stop('Incorrect class for saveSettings')
  } 
  
  # Load already created cohorts
  if (dataSettings$OMOP_CDM) {
    
    # Connect to database
    connection <- DatabaseConnector::connect(dataSettings$connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
    
    # Get cohorts from database
    full_cohorts <- data.table::as.data.table(extractFile(connection, dataSettings$cohortTable, dataSettings$cohortDatabaseSchema, dataSettings$connectionDetails$dbms))
    
  } else {
    # Get cohorts from csv file
    # Required columns: cohortId, personId, startDate, endDate
    full_cohorts <- data.table::as.data.table(readr::read_csv(dataSettings$cohortLocation, col_types = list("d", "d", "D", "D")))
  }
  colnames(full_cohorts) <- c("cohort_id", "person_id", "start_date", "end_date")   
  
  # Save pathway settings
  pathwaySettings <- pathwaySettings$all_settings
  write.csv(pathwaySettings, file.path(saveSettings$outputFolder, "settings", "pathway_settings.csv"), row.names = FALSE)
  
  # For all different pathway settings
  settings <- colnames(pathwaySettings)[grepl("analysis", colnames(pathwaySettings))]
  
  for (s in settings) {
    studyName <- pathwaySettings[pathwaySettings$param == "studyName",s]
    
    # Check if directories exist and create if necessary
    tempFolder_s <- file.path(saveSettings$tempFolder, studyName)
    if (!file.exists(tempFolder_s))
      dir.create(tempFolder_s, recursive = TRUE)
    
    ParallelLogger::logInfo(print(paste0("Constructing treatment pathways: ", studyName)))
    
    # Select cohorts included
    targetCohortId <- pathwaySettings[pathwaySettings$param == "targetCohortId",s]
    eventCohortIds <- pathwaySettings[pathwaySettings$param == "eventCohortIds",s]
    eventCohortIds <- unlist(strsplit(eventCohortIds, split = c(";|,")))
    
    # Analysis settings
    includeTreatments <- pathwaySettings[pathwaySettings$param == "includeTreatments",s]
    periodPriorToIndex <- as.integer(pathwaySettings[pathwaySettings$param == "periodPriorToIndex",s])
    minEraDuration <-  as.integer(pathwaySettings[pathwaySettings$param == "minEraDuration",s])
    splitEventCohorts <-  pathwaySettings[pathwaySettings$param == "splitEventCohorts",s] 
    splitTime <-  pathwaySettings[pathwaySettings$param == "splitTime",s] 
    eraCollapseSize <-  as.integer(pathwaySettings[pathwaySettings$param == "eraCollapseSize",s])
    combinationWindow <-  as.integer(pathwaySettings[pathwaySettings$param == "combinationWindow",s]) 
    minPostCombinationDuration <-  as.integer(pathwaySettings[pathwaySettings$param == "minPostCombinationDuration",s]) 
    filterTreatments <-  pathwaySettings[pathwaySettings$param == "filterTreatments",s]
    maxPathLength <- as.integer(pathwaySettings[pathwaySettings$param == "maxPathLength",s])
    
    # Select subset of full cohort including only data for the current target cohort
    select_people <- full_cohorts$person_id[full_cohorts$cohort_id == targetCohortId]
    current_cohorts <- full_cohorts[full_cohorts$person_id %in% select_people, ]
    
    if (nrow(current_cohorts) != 0) {
      
      # Preprocess the target/event cohorts to create treatment history
      treatment_history <- doCreateTreatmentHistory(current_cohorts, targetCohortId, eventCohortIds, periodPriorToIndex, includeTreatments)
      
      # Apply pathway settings to create treatment pathways
      ParallelLogger::logInfo("Construct treatment pathways, this may take a while for larger datasets.")
      writeLines(paste0("Original number of rows: ", nrow(treatment_history)))
      
      # TODO: check what happens if treatment_history zero or few rows (throw errors)
      
      treatment_history <- doEraDuration(treatment_history, minEraDuration)
      treatment_history <- doSplitEventCohorts(treatment_history, splitEventCohorts, splitTime, saveSettings$outputFolder)
      treatment_history <- doEraCollapse(treatment_history, eraCollapseSize)
      treatment_history <- doCombinationWindow(treatment_history, combinationWindow, minPostCombinationDuration)
      treatment_history <- doFilterTreatments(treatment_history, filterTreatments)
      
      if (nrow(treatment_history) != 0) {
        # Add event_seq number to determine order of treatments in pathway
        ParallelLogger::logInfo("Adding drug sequence number.")
        treatment_history <- treatment_history[order(person_id, event_start_date, event_end_date),]
        treatment_history[, event_seq:=seq_len(.N), by= .(person_id)]
        
        treatment_history <- doMaxPathLength(treatment_history, maxPathLength)
        
        # Add event_cohort_name (instead of only event_cohort_id)
        ParallelLogger::logInfo("Adding concept names.")
        treatment_history <- addLabels(treatment_history, saveSettings$outputFolder)
        
        # Order the combinations
        ParallelLogger::logInfo("Ordering the combinations.")
        combi <- grep("+", treatment_history$event_cohort_name, fixed=TRUE)
        cohort_names <- strsplit(treatment_history$event_cohort_name[combi], split="+", fixed=TRUE)
        treatment_history$event_cohort_name[combi] <- sapply(cohort_names, function(x) paste(sort(x), collapse = "+"))
        treatment_history$event_cohort_name <- unlist(treatment_history$event_cohort_name)
      }
      
      # Save the processed treatment history
      write.csv(treatment_history, file.path(tempFolder_s, paste0(saveSettings$databaseName, "_", studyName, "_event_seq_processed.csv")), row.names = FALSE) 
      
      # Save the treatment pathways
      if (nrow(treatment_history) != 0) {
        treatment_pathways <- as.data.table(reshape2::dcast(data = treatment_history, person_id + index_year ~ event_seq, value.var = "event_cohort_name"))
        colnames(treatment_pathways)[3:ncol(treatment_pathways)] <- paste0("event_cohort_name", colnames(treatment_pathways)[3:ncol(treatment_pathways)])
        
        layers <- c(colnames(treatment_pathways))[3:min(7,ncol(treatment_pathways))] # max first 5
        treatment_pathways <- treatment_pathways[, .(freq=length((person_id))), by = c(layers, "index_year")]
        write.csv(treatment_pathways, file.path(tempFolder_s, paste0(saveSettings$databaseName, "_", studyName, "_paths.csv")), row.names = FALSE) 
        
        # Calculate counts of the number of persons in target cohort / with pathways, in total / per year
        targetCohort <- current_cohorts[current_cohorts$cohort_id %in% targetCohortId,,]
        targetCohort$index_year <- as.numeric(format(targetCohort$start_date, "%Y"))
        counts_targetcohort <- data.table::rollup(targetCohort, .N, by = c("index_year"))
        counts_targetcohort$index_year <- paste0("Number of persons in target cohort ", counts_targetcohort$index_year)
        
        counts_pathways <- rollup(treatment_pathways, sum(freq), by = c("index_year"))
        counts_pathways$index_year <- paste0("Number of pathways (before minCellCount) in ", counts_pathways$index_year)
        
        colnames(counts_pathways) <- colnames(counts_targetcohort)
        counts <- rbind(counts_targetcohort, counts_pathways)
        
        write.csv(counts, file.path(tempFolder_s, paste0(saveSettings$databaseName, "_", studyName, "_summary_cnt.csv")), row.names = FALSE) 
        
      }
    }
  }
  
  ParallelLogger::logInfo("constructPathways done.")
}


# Input:
# current_cohorts Dataframe with target and event cohorts of current study settings.
# targetCohortId Target cohort ID of current study settings.
# eventCohortIds Event cohort IDs of current study settings.
# periodPriorToIndex Number of days prior to the index date of the target cohort that event cohorts are allowed to start
# includeTreatments Include treatments starting ('startDate') or ending ('endDate') after target cohort start date
#
# Output: Updated dataframe, including only event cohorts after target cohort start date and with added index year, duration, gap same columns.
doCreateTreatmentHistory <- function(current_cohorts, targetCohortId, eventCohortIds, periodPriorToIndex, includeTreatments) {
  
  # Add index year column based on start date target cohort
  targetCohort <- current_cohorts[current_cohorts$cohort_id %in% targetCohortId,,]
  targetCohort$index_year <- as.numeric(format(targetCohort$start_date, "%Y"))
  
  # Select event cohorts for target cohort and merge with start/end date and index year
  eventCohorts <- current_cohorts[current_cohorts$cohort_id %in% eventCohortIds,,]
  current_cohorts <- merge(x = eventCohorts, y = targetCohort, by = c("person_id"), all.x = TRUE, allow.cartesian = TRUE)
  
  # Only keep event cohorts starting (startDate) or ending (endDate) after target cohort start date
  if (includeTreatments == "startDate") {
    current_cohorts <- current_cohorts[current_cohorts$start_date.y - as.difftime(periodPriorToIndex, unit="days") <= current_cohorts$start_date.x & current_cohorts$start_date.x < current_cohorts$end_date.y,]
  } else if (includeTreatments == "endDate") {
    current_cohorts <- current_cohorts[current_cohorts$start_date.y - as.difftime(periodPriorToIndex, unit="days") <= current_cohorts$end_date.x & current_cohorts$start_date.x < current_cohorts$end_date.y,]
    current_cohorts$start_date.x <- pmax(current_cohorts$start_date.y - as.difftime(periodPriorToIndex, unit="days"), current_cohorts$start_date.x)
  } else {
    warning("includeTreatments input incorrect, return all event cohorts ('includeTreatments')")
    current_cohorts <- current_cohorts[current_cohorts$start_date.y - as.difftime(periodPriorToIndex, unit="days") <= current_cohorts$start_date.x & current_cohorts$start_date.x < current_cohorts$end_date.y,]
  }
  
  # Remove unnecessary columns
  current_cohorts <- current_cohorts[,c("person_id", "index_year", "cohort_id.x", "start_date.x", "end_date.x")]
  colnames(current_cohorts) <- c("person_id", "index_year", "event_cohort_id", "event_start_date", "event_end_date")
  
  # Calculate duration and gap same
  current_cohorts[,duration_era:=difftime(event_end_date, event_start_date, units = "days")]
  
  current_cohorts <- current_cohorts[order(event_start_date, event_end_date),]
  current_cohorts[,lag_variable:=shift(event_end_date, type = "lag"), by=c("person_id", "event_cohort_id")]
  current_cohorts[,gap_same:=difftime(event_start_date, lag_variable, units = "days"),]
  current_cohorts$lag_variable <- NULL
  
  return(current_cohorts)
}


# Input:
# treatment_history Dataframe with event cohorts of the target cohort in different rows.
# minEraDuration Minimum time an event era should last to be included in analysis.
#
# Output: Updated dataframe, rows with duration < minEraDuration filtered out.
doEraDuration <- function(treatment_history, minEraDuration) {
  treatment_history <- treatment_history[duration_era >= minEraDuration,]
  ParallelLogger::logInfo(print(paste0("After minEraDuration: ", nrow(treatment_history))))
  
  return(treatment_history)
}

# Input:
# treatment_history Dataframe with event cohorts of the target cohort in different rows.
# minPostCombinationDuration Minimum time an event era before or after a generated combination treatment should last to be included in analysis.
#
# Output: Updated dataframe, rows with duration_era < minPostCombinationDuration filtered out.
doStepDuration <- function(treatment_history, minPostCombinationDuration) {
  treatment_history <- treatment_history[(is.na(check_duration) | duration_era >= minPostCombinationDuration),]
  ParallelLogger::logInfo(print(paste0("After minPostCombinationDuration: ", nrow(treatment_history))))
  
  return(treatment_history)
}

# Input:
# treatment_history Dataframe with event cohorts of the target cohort in different rows.
# splitEventCohorts Specify event cohort to split in acute (< X days) and therapy (>= X days).
# splitTime Specify number of days (X) at which each of the split event cohorts should be split in acute and therapy
# outputFolder Name of local folder to place results; make sure to use forward slashes (/).
#
# Output: Updated dataframe, with specified event cohorts now split in two different event cohorts (acute and therapy).
doSplitEventCohorts <- function(treatment_history, splitEventCohorts, splitTime, outputFolder) {
  
  if (!is.na(splitEventCohorts)) {
    # Load in labels cohorts
    labels <- data.table(readr::read_csv(file.path(outputFolder, "settings", "cohorts_to_create.csv"), col_types = list("c","c","c","i","c")))
    
    for (c in 1:length(splitEventCohorts)) {
      cohort <- splitEventCohorts[c]
      cutoff <- splitTime[c]
      
      # Label as acute
      treatment_history[event_cohort_id == cohort & duration_era < cutoff, "event_cohort_id"] <- as.integer(paste0(cohort,1))
      
      # Label as therapy
      treatment_history[event_cohort_id == cohort & duration_era >= cutoff, "event_cohort_id"] <- as.integer(paste0(cohort,2))
      
      # Add new labels
      original <- labels[cohortId == as.integer(cohort),]
      
      acute <- original
      acute$cohortId <- as.integer(paste0(cohort,1))
      acute$cohortName <- paste0(acute$cohortName, " (acute)")
      
      therapy <- original
      therapy$cohortId <- as.integer(paste0(cohort,2))
      therapy$cohortName <- paste0(therapy$cohortName, " (therapy)")
      
      labels <- labels[cohortId != as.integer(cohort),]
      labels <- rbind(labels, acute, therapy)
    }
    
    # Save new labels cohorts
    write.csv(labels, file.path(outputFolder, "cohort.csv"), row.names = FALSE)
  }
  
  return(treatment_history) 
}

# Input:
# treatment_history Dataframe with event cohorts of the target cohort in different rows.
# eraCollapseSize Window of time between which two eras of the same event cohort are collapsed into one era.
#
# Output:
# Updated dataframe, where event cohorts with gap_same < eraCollapseSize are collapsed.
doEraCollapse <- function(treatment_history, eraCollapseSize) {
  # Order treatment_history by person_id, event_cohort_id, start_date, end_date
  treatment_history <- treatment_history[order(person_id, event_cohort_id,event_start_date, event_end_date),]
  
  # Find all rows with gap_same < eraCollapseSize
  rows <- which(treatment_history$gap_same < eraCollapseSize)
  
  # For all rows, modify the row preceding, loop backwards in case more than one collapse
  for (r in rev(rows)) {
    treatment_history[r - 1,"event_end_date"] <- treatment_history[r,event_end_date]
  }
  
  # Remove all rows with gap_same < eraCollapseSize
  treatment_history <- treatment_history[!rows,]
  treatment_history[,gap_same:=NULL]
  
  # Re-calculate duration_era
  treatment_history[,duration_era:=difftime(event_end_date , event_start_date, units = "days")]
  
  ParallelLogger::logInfo(print(paste0("After eraCollapseSize: ", nrow(treatment_history))))
  return(treatment_history)
}

# Input:
# treatment_history Dataframe with event cohorts of the target cohort in different rows.
# combinationWindow Window of time two event cohorts need to overlap to be considered a combination treatment.
# minPostCombinationDuration Minimum time an event era before or after a generated combination treatment should last to be included in analysis.
#
# Output:
# Updated dataframe, where overlapping event cohorts are modified according to rules defined for switching / combinations.
doCombinationWindow <- function(treatment_history, combinationWindow, minPostCombinationDuration) {
  
  time1 <- Sys.time()
  
  treatment_history$event_cohort_id <- as.character(treatment_history$event_cohort_id)
  
  # Find which rows contain some overlap
  treatment_history <- selectRowsCombinationWindow(treatment_history)
  
  # While rows that need modification exist:
  iterations <- 1
  while(sum(treatment_history$SELECTED_ROWS)!=0) {
    
    # Which have gap previous shorter than combination window OR min(current duration era, previous duration era) -> add column switch
    treatment_history[SELECTED_ROWS == 1 & (-GAP_PREVIOUS < combinationWindow  & !(-GAP_PREVIOUS == duration_era | -GAP_PREVIOUS == shift(duration_era, type = "lag"))), switch:=1]
    
    # For rows selected not in column switch -> if treatment_history[r - 1, event_end_date] <= treatment_history[r, event_end_date] -> add column combination first received, first stopped
    treatment_history[SELECTED_ROWS == 1 & is.na(switch) & shift(event_end_date, type = "lag") <= event_end_date, combination_FRFS:=1]
    
    # For rows selected not in column switch -> if treatment_history[r - 1, event_end_date] > treatment_history[r, event_end_date] -> add column combination last received, first stopped
    treatment_history[SELECTED_ROWS == 1 & is.na(switch) & shift(event_end_date, type = "lag") > event_end_date, combination_LRFS:=1]
    
    ParallelLogger::logInfo(print(paste0("Iteration ", iterations, " modifying  ", sum(treatment_history$SELECTED_ROWS), " selected rows out of ", nrow(treatment_history), ": ", sum(!is.na(treatment_history$switch)) , " switches, ", sum(!is.na(treatment_history$combination_FRFS)), " combinations FRFS and ", sum(!is.na(treatment_history$combination_LRFS)), " combinations LRFS")))
    if (sum(!is.na(treatment_history$switch)) + sum(!is.na(treatment_history$combination_FRFS)) +  sum(!is.na(treatment_history$combination_LRFS)) != sum(treatment_history$SELECTED_ROWS)) {
      warning(paste0(sum(treatment_history$SELECTED_ROWS), ' does not equal total sum ', sum(!is.na(treatment_history$switch)) +  sum(!is.na(treatment_history$combination_FRFS)) +  sum(!is.na(treatment_history$combination_LRFS))))
    }
    
    # Do transformations for each of the three newly added columns
    # Construct helpers
    treatment_history[,event_start_date_next:=shift(event_start_date, type = "lead"),by=person_id]
    treatment_history[,event_end_date_previous:=shift(event_end_date, type = "lag"),by=person_id]
    treatment_history[,event_end_date_next:=shift(event_end_date, type = "lead"),by=person_id]
    treatment_history[,event_cohort_id_previous:=shift(event_cohort_id, type = "lag"),by=person_id]
    
    # Case: switch
    # Change end treatment_history of previous row -> no minPostCombinationDuration
    treatment_history[shift(switch, type = "lead")==1,event_end_date:=event_start_date_next]
    
    # Case: combination_FRFS
    # Add a new row with start date (r) and end date (r-1) as combination (copy current row + change end date + update concept id) -> no minPostCombinationDuration
    add_rows_FRFS <- treatment_history[combination_FRFS==1,]
    add_rows_FRFS[,event_end_date:=event_end_date_previous]
    add_rows_FRFS[,event_cohort_id:=paste0(event_cohort_id, "+", event_cohort_id_previous)]
    
    # Change end date of previous row -> check minPostCombinationDuration
    treatment_history[shift(combination_FRFS, type = "lead")==1,c("event_end_date","check_duration"):=list(event_start_date_next, 1)]
    
    # Change start date of current row -> check minPostCombinationDuration 
    treatment_history[combination_FRFS==1,c("event_start_date", "check_duration"):=list(event_end_date_previous,1)]
    
    # Case: combination_LRFS
    # Change current row to combination -> no minPostCombinationDuration
    treatment_history[combination_LRFS==1,event_cohort_id:=paste0(event_cohort_id, "+", event_cohort_id_previous)]
    
    # Add a new row with end date (r) and end date (r-1) to split drug era (copy previous row + change end date) -> check minPostCombinationDuration 
    add_rows_LRFS <- treatment_history[shift(combination_LRFS, type = "lead")==1,]
    add_rows_LRFS[,c("event_start_date", "check_duration"):=list(event_end_date_next,1)]
    
    # Change end date of previous row -> check minPostCombinationDuration 
    treatment_history[shift(combination_LRFS, type = "lead")==1,c("event_end_date", "check_duration"):=list(event_start_date_next,1)]
    
    # Combine all rows and remove helper columns
    treatment_history <- rbind(treatment_history, add_rows_FRFS, fill=TRUE)
    treatment_history <- rbind(treatment_history, add_rows_LRFS)
    
    # Re-calculate duration_era
    treatment_history[,duration_era:=difftime(event_end_date, event_start_date, units = "days")]
    
    # Check duration drug eras before/after generated combination treatments
    treatment_history <- doStepDuration(treatment_history, minPostCombinationDuration)
    
    # Preparations for next iteration
    treatment_history <- treatment_history[,c("person_id", "index_year", "event_cohort_id", "event_start_date", "event_end_date", "duration_era")]
    treatment_history <- selectRowsCombinationWindow(treatment_history)
    iterations <- iterations + 1
    
    gc()
  }
  
  ParallelLogger::logInfo(print(paste0("After combinationWindow: ", nrow(treatment_history))))
  
  treatment_history[,GAP_PREVIOUS:=NULL]
  treatment_history[,SELECTED_ROWS:=NULL]
  
  time2 <- Sys.time()
  ParallelLogger::logInfo(paste0("Time needed to execute combination window ", difftime(time2, time1, units = "mins")))
  
  return(treatment_history)
}

# Help function for doCombinationWindow that selects one overlapping drug era per person to modify in next iteration of combination window. 
selectRowsCombinationWindow <- function(treatment_history) {
  # Order treatment_history by person_id, event_start_date, event_end_date
  treatment_history <- treatment_history[order(person_id, event_start_date, event_end_date),]
  
  # Calculate gap with previous treatment
  treatment_history[,GAP_PREVIOUS:=difftime(event_start_date, shift(event_end_date, type = "lag"), units = "days"), by = person_id]
  treatment_history$GAP_PREVIOUS <- as.integer(treatment_history$GAP_PREVIOUS)
  
  # Find all rows with gap_previous < 0
  treatment_history[treatment_history$GAP_PREVIOUS < 0, ALL_ROWS:=which(treatment_history$GAP_PREVIOUS < 0)]
  
  # Select one row per iteration for each person
  rows <- treatment_history[!is.na(ALL_ROWS),head(.SD,1), by=person_id]$ALL_ROWS
  
  treatment_history[rows,SELECTED_ROWS:=1]
  treatment_history[!rows,SELECTED_ROWS:=0]
  treatment_history[,ALL_ROWS:=NULL]
  
  return(treatment_history)
}

# Input:
# treatment_history Dataframe with event cohorts of the target cohort in different rows.
# filterTreatments Select first occurrence of ('First') / changes between ('Changes') / all event cohorts ('All').
#
# Updated dataframe, where the desired event cohorts are maintained for the visualizations.
doFilterTreatments <- function(treatment_history, filterTreatments) {
  
  # Order treatment_history by person_id, event_start_date, event_end_date
  treatment_history <- treatment_history[order(person_id, event_start_date, event_end_date),]
  
  if (filterTreatments == "All") {} # Do nothing
  else {
    # Order the combinations
    ParallelLogger::logInfo("Order the combinations.")
    combi <- grep("+", treatment_history$event_cohort_id, fixed=TRUE)
    if (length(combi) != 0) {
      concept_ids <- strsplit(treatment_history$event_cohort_id[combi], split="+", fixed=TRUE)
      treatment_history$event_cohort_id[combi] <- sapply(concept_ids, function(x) paste(sort(x), collapse = "+"))
    }
    
    if (filterTreatments == "First") {
      treatment_history <- treatment_history[, head(.SD,1), by=.(person_id, event_cohort_id)]
      
    } else if (filterTreatments == "Changes") {
      # Group all rows per person for which previous treatment is same
      tryCatch(treatment_history <- treatment_history[, group:=rleid(person_id,event_cohort_id)],
               error = function(e){print(paste0("Check if treatment_history contains sufficient records: ", e))})
      
      # Remove all rows with same sequential treatments
      treatment_history <- treatment_history[,.(event_start_date=min(event_start_date), event_end_date=max(event_end_date), duration_era=sum(duration_era)), by = .(person_id,index_year,event_cohort_id,group)]
      treatment_history[,group:=NULL]
    } else {
      warning("filterTreatments input incorrect, return all event cohorts ('All')")
    }
  }
  
  ParallelLogger::logInfo(print(paste0("After filterTreatments: ", nrow(treatment_history))))
  
  return(treatment_history)
}


# Input:
# treatment_history Dataframe with event cohorts of the target cohort in different rows.
# maxPathLength Maximum number of steps included in treatment pathway.
#
# Updated dataframe, where the desired event cohorts are maintained for the visualizations.
doMaxPathLength <- function(treatment_history, maxPathLength) {
  
  # Apply maxPathLength
  treatment_history <- treatment_history[event_seq <= maxPathLength,]
  
  ParallelLogger::logInfo(print(paste0("After maxPathLength: ", nrow(treatment_history))))
  
  return(treatment_history)
}

# Adds back cohort names to concept ids.
addLabels <- function(treatment_history, outputFolder) {
  labels <- data.frame(readr::read_csv(file.path(outputFolder, "settings", "cohorts_to_create.csv"), col_types = list("c","c","c","i","c")))
  labels <- labels[labels$cohortType == "event",c("cohortId", "cohortName")]
  colnames(labels) <- c("event_cohort_id", "event_cohort_name")
  
  treatment_history <- merge(treatment_history, labels, all.x = TRUE, by = "event_cohort_id")
  treatment_history$event_cohort_name[is.na(treatment_history$event_cohort_name)] <- sapply(treatment_history$event_cohort_id[is.na(treatment_history$event_cohort_name)], function(x) {
    
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
  treatment_history$event_cohort_name <- gsub("\\++", "+", treatment_history$event_cohort_name)
  treatment_history$event_cohort_name <- gsub("^\\+", "", treatment_history$event_cohort_name)
  treatment_history$event_cohort_name <- gsub("\\+$", "", treatment_history$event_cohort_name)
  
  return(treatment_history)
}
