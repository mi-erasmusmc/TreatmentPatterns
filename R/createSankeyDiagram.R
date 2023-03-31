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

#' createSankeyDiagram
#'
#' @param data
#'     Dataframe with event cohorts of the target cohort in different columns.
#' @param outputFolder
#'     Path to the output folder.
#' @param groupCombinations
#'     Select to group all non-fixed combinations in one category "other" in
#'     the sunburst plot.
#' @param fileName
#'     File name of the html-file to output.
#'
#' @export
#'
#' @returns NULL
createSankeyDiagram <- function(
    data,
    outputFolder,
    groupCombinations,
    fileName = "sankeyDiagram.html") {
  # Group non-fixed combinations in one group according to groupCobinations
  data <- groupInfrequentCombinations(data, groupCombinations)
  
  # Define stop treatment
  data[is.na(data)] <- "Stopped"
  
  # Sankey diagram for first three treatment layers
  data$event_cohort_name1 <- paste0("1. ", data$event_cohort_name1)
  data$event_cohort_name2 <- paste0("2. ", data$event_cohort_name2)
  data$event_cohort_name3 <- paste0("3. ", data$event_cohort_name3)
  
  results1 <- data %>%
    dplyr::group_by(event_cohort_name1, event_cohort_name2) %>%
    dplyr::summarise(freq = sum(freq))
  
  results2 <- data %>%
    dplyr::group_by(event_cohort_name2, event_cohort_name3) %>%
    dplyr::summarise(freq = sum(freq))
  
  # Format in prep for sankey diagram
  colnames(results1) <- c("source", "target", "value")
  colnames(results2) <- c("source", "target", "value")
  links <- as.data.frame(rbind(results1, results2))
  
  # Draw sankey network
  plot <- googleVis::gvisSankey(
    links,
    from = "source",
    to = "target",
    weight = "value",
    chartid = 1,
    options = list(sankey = "{node: { colors: ['#B5482A'], width: 5}}")
  )
  
  writeLines(
    text = plot$html$chart,
    con = normalizePath(paste0(outputFolder, fileName), mustWork = FALSE)
  )
}