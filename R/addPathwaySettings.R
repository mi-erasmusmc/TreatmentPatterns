#' addPathwaySettingsCheck
#'
#' Asserts that parameters fall within specified bounds.
#'
#' @param studyName
#'     Name identifying the set of study parameters.
#'
#' @param targetCohortId
#'     Target cohort ID of current study settings.
#'
#' @param eventCohortIds
#'     Event cohort IDs of current study settings.
#'
#' @param includeTreatments
#'     Include treatments starting ('startDate') or ending ('endDate') after
#'     target cohort start date.
#'
#' @param periodPriorToIndex
#'     Number of days prior to the index date of the target cohort that event
#'     cohorts are allowed to start.
#'
#' @param minEraDuration
#'     Minimum time an event era should last to be included in analysis.
#'
#' @param splitEventCohorts
#'     Specify event cohort ID's to split in acute (< X days) and therapy
#'     (>= X days).
#'
#' @param splitTime
#'     Specify number of days (X) at which each of the split event cohorts
#'     should be split in acute and therapy.
#'
#' @param eraCollapseSize
#'     Window of time between which two eras of the same event cohort are
#'     collapsed into one era.
#'
#' @param combinationWindow
#'     Window of time two event cohorts need to overlap to be considered a
#'     combination treatment.
#'
#' @param minPostCombinationDuration
#'     Minimum time an event era before or after a generated combination
#'     treatment should last to be included in analysis.
#'
#' @param filterTreatments
#'     Select first occurrence of ("First") / changes between ("Changes') / all
#'     event cohorts ("All").
#'
#' @param maxPathLength
#'     Maximum number of steps included in treatment pathway (max 5).
#'
#' @param minCellCount
#'     Minimum number of persons with a specific treatment pathway for the
#'     pathway to be included in analysis.
#'
#' @param minCellMethod
#'     Select to completely remove / sequentially adjust (by removing last step
#'     as often as necessary) treatment pathways below minCellCount.
#'
#' @param groupCombinations
#'     Select to group all non-fixed combinations in one category 'other’ in
#'     the sunburst plot.
#'
#' @param addNoPaths
#'     Select to include untreated persons without treatment pathway in the
#'     sunburst plot
#'
#' @import checkmate
#'
#' @return TRUE if all assertions pass
#'
#' @examples
#' \dontrun{
#' TreatmentPatterns:::addPathwaySettingsCheck(
#'   studyName = "myCoolStudy",
#'   targetCohortId = 1,
#'   eventCohortIds = c(1, 2, 3),
#'   includeTreatments = "startDate",
#'   periodPriorToIndex = 0,
#'   minEraDuration = 0,
#'   splitEventCohorts = "",
#'   splitTime = 30,
#'   eraCollapseSize = 30,
#'   combinationWindow = 30,
#'   minPostCombinationDuration = 30,
#'   filterTreatments = "First",
#'   maxPathLength = 5,
#'   minCellCount = 5,
#'   minCellMethod = "Remove",
#'   groupCombinations = 10,
#'   addNoPaths = FALSE)
#'}
addPathwaySettingsCheck <- function(
    studyName,
    targetCohortId,
    eventCohortIds,
    includeTreatments,
    periodPriorToIndex,
    minEraDuration,
    splitEventCohorts,
    splitTime,
    eraCollapseSize,
    combinationWindow,
    minPostCombinationDuration,
    filterTreatments,
    maxPathLength,
    minCellCount,
    minCellMethod,
    groupCombinations,
    addNoPaths) {

  # studyName
  checkmate::assertCharacter(x = studyName,
                             len = 1,
                             null.ok = FALSE)

  # targetCohortId
  checkmate::assertNumeric(x = targetCohortId,
                           min.len = 1,
                           unique = TRUE,
                           null.ok = FALSE)

  # eventCohortIds
  checkmate::assertNumeric(x = eventCohortIds,
                           min.len = 1,
                           unique = TRUE,
                           null.ok = FALSE)

  # includeTreatments
  checkmate::assertCharacter(x = includeTreatments,
                             len = 1)
  checkmate::assertSubset(x = includeTreatments,
                          choices = c("startDate", "endDate"))

  # periodPriorToIndex
  checkmate::assertNumeric(x = periodPriorToIndex,
                           # lower = 0, # Can it be negative?
                           len = 1,
                           finite = TRUE,
                           null.ok = FALSE)

  # minEraDuration
  checkmate::assertNumeric(x = minEraDuration,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # splitEventCohorts
  checkmate::assertCharacter(x = splitEventCohorts,
                             len = 1)

  # splitTime
  checkmate::assertNumeric(x = splitTime,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # eraCollapseSize
  checkmate::assertNumeric(x = eraCollapseSize,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # combinationWindow
  checkmate::assertNumeric(x = combinationWindow,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # minPostCombinationDuration
  checkmate::assertNumeric(x = minPostCombinationDuration,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # filterTreatments
  checkmate::assertCharacter(x = filterTreatments,
                             len = 1)
  checkmate::assertSubset(x = filterTreatments,
                          choices = c("First", "Changes", "All"))

  # maxPathLength
  checkmate::assertNumeric(x = maxPathLength,
                           lower = 0,
                           upper = 5,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # minCellCount
  checkmate::assertNumeric(x = minCellCount,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # minCellMethod
  # Not used in ConstructPathways.R
  checkmate::assertCharacter(x = minCellMethod,
                             len = 1)

  # groupCombinations
  checkmate::assertNumeric(x = groupCombinations,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # addNoPaths
  checkmate::assertLogical(x = addNoPaths,
                           any.missing = FALSE,
                           len = 1)

  return(TRUE)
}

#' addPathwaySettings
#'
#' Defines and returns a data.frame specifying different parameters how to compute 
#' the existing pathways in the function call.
#'
#' @param studyName
#'     Name identifying the set of study parameters. Default value is "name_unknown".
#'
#' @param targetCohortId
#'     Target cohort ID of current study settings.
#'
#' @param eventCohortIds
#'     Event cohort IDs of current study settings.
#'
#' @param includeTreatments
#'     Include treatments starting ('startDate') or ending ('endDate') after
#'     target cohort start date. Default value is "startDate".
#'
#' @param periodPriorToIndex
#'     Number of days prior to the index date of the target cohort that event
#'     cohorts are allowed to start. Default value is 0.
#'
#' @param minEraDuration
#'     Minimum time an event era should last to be included in analysis. Default value is 0.
#'
#' @param splitEventCohorts
#'     Specify event cohort ID's to split in acute (< X days) and therapy
#'     (>= X days).
#'
#' @param splitTime
#'     Specify number of days (X) at which each of the split event cohorts
#'     should be split in acute and therapy. Default value is 30.
#'
#' @param eraCollapseSize
#'     Window of time between which two eras of the same event cohort are
#'     collapsed into one era. Default value is 30.
#'
#' @param combinationWindow
#'     Window of time two event cohorts need to overlap to be considered a
#'     combination treatment. Default value is 30.
#'
#' @param minPostCombinationDuration
#'     Minimum time an event era before or after a generated combination
#'     treatment should last to be included in analysis. Default value is 30.
#'
#' @param filterTreatments
#'     Select first occurrence of ("First") / changes between ("Changes') / all
#'     event cohorts ("All"). Default value is "First".
#'
#' @param maxPathLength
#'     Maximum number of steps included in treatment pathway (max 5). Default value is 5.
#'
#' @param minCellCount
#'     Minimum number of persons with a specific treatment pathway for the
#'     pathway to be included in analysis. Default value is 5.
#'
#' @param minCellMethod
#'     Select to completely remove / sequentially adjust (by removing last step
#'     as often as necessary) treatment pathways below minCellCount. Default value is "Remove".
#'
#' @param groupCombinations
#'     Select to group all non-fixed combinations in one category 'other’ in
#'     the sunburst plot. Default value is 10.
#'
#' @param addNoPaths
#'     Select to include untreated persons without treatment pathway in the
#'     sunburst plot. Default value is FALSE.
#'     
#' @return a data.frame containing the pathway settings
#'
#' @export
#' @examples 
#' \dontrun{
#' pathwaySettings <- addPathwaySettings(
#'   studyName = "myCoolStudy",
#'   targetCohortId = 1,
#'   eventCohortIds = c(1,2,3))
#' }
addPathwaySettings <- function(
    studyName = "name_unknown",
    targetCohortId,
    eventCohortIds,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5,
    minCellCount = 5,
    minCellMethod = "Remove",
    groupCombinations = 10,
    addNoPaths = TRUE) {
  
  check <- addPathwaySettingsCheck(
    studyName,
    targetCohortId,
    eventCohortIds,
    includeTreatments,
    periodPriorToIndex,
    minEraDuration,
    splitEventCohorts,
    splitTime,
    eraCollapseSize,
    combinationWindow,
    minPostCombinationDuration,
    filterTreatments,
    maxPathLength,
    minCellCount,
    minCellMethod,
    groupCombinations,
    addNoPaths
  )

  if (check) {
    settings <- data.frame(
      studyName = studyName,
      targetCohortId = targetCohortId,
      eventCohortIds = paste(eventCohortIds, collapse = ","),
      includeTreatments = includeTreatments,
      periodPriorToIndex = periodPriorToIndex,
      minEraDuration = minEraDuration,
      splitEventCohorts = splitEventCohorts,
      splitTime = splitTime,
      eraCollapseSize = eraCollapseSize,
      combinationWindow = combinationWindow,
      minPostCombinationDuration = minPostCombinationDuration,
      filterTreatments = filterTreatments,
      maxPathLength = maxPathLength,
      minCellCount = minCellCount,
      minCellMethod = minCellMethod,
      groupCombinations = groupCombinations,
      addNoPaths = addNoPaths
    )

    return(settings)
  }
}
