#' checkAddPathwaySettings
#'
#' Asserts that parameters fall within specified bounds.
#'
#' @param env Environment containging all the function environment variables.
#'
#' @return TRUE if all assertions pass
#'
#' @examples
#' \dontrun{
#' TreatmentPatterns:::checkAddPathwaySettings(env)
#'}
checkAddPathwaySettings <- function(env) {

  # studyName
  checkmate::assertCharacter(x = env$studyName,
                             len = 1,
                             null.ok = FALSE)

  # targetCohortId
  checkmate::assertNumeric(x = env$targetCohortId,
                           min.len = 1,
                           unique = TRUE,
                           null.ok = FALSE)

  # eventCohortIds
  checkmate::assertNumeric(x = env$eventCohortIds,
                           min.len = 1,
                           unique = TRUE,
                           null.ok = FALSE)

  # includeTreatments
  checkmate::assertCharacter(x = env$includeTreatments,
                             len = 1)
  
  checkmate::assertSubset(x = env$includeTreatments,
                          choices = c("startDate", "endDate"))

  # periodPriorToIndex
  checkmate::assertNumeric(x = env$periodPriorToIndex,
                           # lower = 0, # Can it be negative?
                           len = 1,
                           finite = TRUE,
                           null.ok = FALSE)

  # minEraDuration
  checkmate::assertNumeric(x = env$minEraDuration,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # splitEventCohorts
  checkmate::assertCharacter(x = env$splitEventCohorts,
                             len = 1)

  # splitTime
  checkmate::assertNumeric(x = env$splitTime,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # eraCollapseSize
  checkmate::assertNumeric(x = env$eraCollapseSize,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # combinationWindow
  checkmate::assertNumeric(x = env$combinationWindow,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # minPostCombinationDuration
  checkmate::assertNumeric(x = env$minPostCombinationDuration,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # filterTreatments
  checkmate::assertCharacter(x = env$filterTreatments,
                             len = 1)
  
  checkmate::assertSubset(x = env$filterTreatments,
                          choices = c("First", "Changes", "All"))

  # maxPathLength
  checkmate::assertNumeric(x = env$maxPathLength,
                           lower = 0,
                           upper = 5,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # minCellCount
  checkmate::assertNumeric(x = env$minCellCount,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # minCellMethod
  # Not used in ConstructPathways.R
  checkmate::assertCharacter(x = env$minCellMethod,
                             len = 1)

  # groupCombinations
  checkmate::assertNumeric(x = env$groupCombinations,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # addNoPaths
  checkmate::assertLogical(x = env$addNoPaths,
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
  
  check <- checkAddPathwaySettings(environment())

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
