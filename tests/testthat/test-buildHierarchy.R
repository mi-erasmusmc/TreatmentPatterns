library(TreatmentPatterns)
library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

# === prerequisites ====
data <- TreatmentPatterns:::inputSunburstPlot(
  treatment_pathways[[1]],
  saveSettings$outputFolder,
  saveSettings$databaseName,
  "Viral_Sinusitis",
  FALSE,
  index_year = 'all')

outcomes <- 3
folder = "output"
file_name = "stuff.json"


bitwiseNumbers <- sapply(
  X = 1:length(outcomes),
  FUN = function(o) {
    2^(o-1)
  })

linking <- data.frame(outcomes, bitwiseNumbers)

# Generate lookup file
series <- sapply(
  X = 1:nrow(linking),
  FUN = function (row) {
    paste0(
      '{ "key": "', linking$bitwiseNumbers[row],
      '", "value": "', linking$outcomes[row],'"}')
  })

series <- c(series, '{ "key": "End", "value": "End"}')

linking <- linking[
  order(-sapply(linking$outcomes, function(x) stringr::str_length(x))),]

updated_path <- sapply(
  X = data$path, 
  FUN = function(p) {
    stringr::str_replace_all(
      string = p,
      replacement = as.character(linking$bitwiseNumbers),
      pattern = as.character(linking$outcomes))
  })

updated_path <- sapply(
  X = updated_path,
  FUN = function(p) {
    while(!is.na(stringr::str_extract(p, "[[:digit:]]+[+][[:digit:]]+"))) {
      pattern <- stringr::str_extract(p, "[[:digit:]]+[+][[:digit:]]+")
      p <- sub("[[:digit:]]+[+][[:digit:]]+", eval(parse(text=pattern)), p)
    }
    return(p)
  })

transformed_csv <- cbind(oath = updated_path, freq = data$freq)

# === Testing ====
test_that("void", {
  expect_error(
    TreatmentPatterns:::buildHierarchy()
  )
})

test_that("minimal", {
  expect_character(
    TreatmentPatterns:::buildHierarchy(transformed_csv)
  )
})
