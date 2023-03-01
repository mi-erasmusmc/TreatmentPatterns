connection <- DatabaseConnector::connect(dataSettings$connectionDetails)

fullCohorts <- data.table::as.data.table(extractFile(
  connection, 
  dataSettings$cohortTable, 
  dataSettings$resultSchema, 
  dataSettings$connectionDetails$dbms))

DatabaseConnector::disconnect(connection)

colnames(fullCohorts) <- c(
  "cohort_id", "person_id", "start_date", "end_date")

settings <- pathwaySettings$all_settings

targetCohortId <- settings[
  settings$param == "targetCohortId", "analysis1"]

selectPeople <- fullCohorts$person_id[
  fullCohorts$cohort_id == targetCohortId]

currentCohorts <- fullCohorts[
  fullCohorts$person_id %in% selectPeople, ]

eventCohortIds <- settings[
  settings$param == "eventCohortIds", "analysis1"]

eventCohortIds <- unlist(strsplit(eventCohortIds, split = c(";|,")))

periodPriorToIndex <- as.integer(settings[
  settings$param == "periodPriorToIndex", "analysis1"])

includeTreatments <- settings[
  settings$param == "includeTreatments", "analysis1"]

treatmentHistory <- TreatmentPatterns:::doCreateTreatmentHistory(
  currentCohorts = currentCohorts,
  targetCohortId = targetCohortId,
  eventCohortIds = eventCohortIds,
  periodPriorToIndex = periodPriorToIndex,
  includeTreatments = includeTreatments)

minEraDuration <- as.integer(settings[
  settings$param == "minEraDuration", "analysis1"])

doEraDurationTH <- TreatmentPatterns:::doEraDuration(
  treatment_history = treatmentHistory,
  minEraDuration = minEraDuration)

splitEventCohorts <- settings[
  settings$param == "splitEventCohorts", "analysis1"]

splitTime <- settings[
  settings$param == "splitTime", "analysis1"]

doSplitEventCohortsTH <- TreatmentPatterns:::doSplitEventCohorts(
  treatment_history = doEraDurationTH,
  splitEventCohorts = splitEventCohorts,
  splitTime = splitTime,
  outputFolder = saveSettings$outputFolder)

eraCollapseSize <- as.numeric(settings[
  settings$param == "eraCollapseSize", "analysis1"])

doEraCollapseTH <- TreatmentPatterns:::doEraCollapse(
  doSplitEventCohortsTH,
  eraCollapseSize)

combinationWindow <- settings[
  settings$param == "combinationWindow", "analysis1"]

minPostCombinationDuration <- as.numeric(settings[
  settings$param == "minPostCombinationDuration", "analysis1"])

doCombinationWindowTH <- TreatmentPatterns:::doCombinationWindow(
  doEraCollapseTH,
  combinationWindow,
  minPostCombinationDuration)

filterTreatments <- settings[
  settings$param == "filterTreatments", "analysis1"]

doFilterTreatmentsTH <- TreatmentPatterns:::doFilterTreatments(
  doCombinationWindowTH,
  filterTreatments)

maxPathLength <- as.integer(settings[
  settings$param == "maxPathLength", "analysis1"])

doFilterTreatmentsTHOrdered <- doFilterTreatmentsTH[
  order(person_id, event_start_date, event_end_date), ]

doFilterTreatmentsTHPP <- doFilterTreatmentsTHOrdered[,
                      event_seq := seq_len(.N), by = .(person_id)]

doMaxPathLengthTH <- TreatmentPatterns:::doMaxPathLength(
  doFilterTreatmentsTHPP, 
  maxPathLength)

addLabelsTH <- TreatmentPatterns:::addLabels(
  doMaxPathLengthTH, 
  saveSettings$outputFolder)
