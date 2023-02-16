library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

data <- TreatmentPatterns:::inputSunburstPlot(
  treatment_pathways[[1]],
  saveSettings$outputFolder,
  saveSettings$databaseName,
  "Viral_Sinusitis",
  FALSE,
  index_year = 'all')

test_that("void", {
  expect_error(TreatmentPatterns:::transformCSVtoJSON())
})

test_that("minimal", {
  jsonResult <- TreatmentPatterns:::transformCSVtoJSON(data = data,
                                                       outcomes = c("3"),
                                                       folder = "output",
                                                       fileName = "stuff.json")
  expect_equal(class(jsonResult), "character")
  result <- jsonlite::fromJSON(jsonResult)
  expect_equal(class(result), "list")
  expect_equal(names(result), c("data", "lookup"))
})

test_that("invalid input", {
  expect_error(TreatmentPatterns:::transformCSVtoJSON(data = NULL,
                                                      outcomes = c("3"),
                                                      folder = "output",
                                                      file_name = "stuff.json"))
  
  expect_error(TreatmentPatterns:::transformCSVtoJSON(data = data,
                                                      outcomes = "Test",
                                                      folder = "output",
                                                      file_name = "stuff.json"))
  
  expect_error(TreatmentPatterns:::transformCSVtoJSON(data = data,
                                                      outcomes = c("3"),
                                                      folder = -1,
                                                      file_name = "stuff.json"))
  
  expect_error(TreatmentPatterns:::transformCSVtoJSON(data = NULL,
                                                      outcomes = c("3"),
                                                      folder = "output",
                                                      file_name = NULL))
})