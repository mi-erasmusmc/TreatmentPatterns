library(TreatmentPatterns)
library(testthat)

test_that("Void", {
  expect_error(TreatmentPatterns::saveAsFile())
})

test_that("Minimal", {
  inputFile  <- system.file("shiny/html/custom.html", package= "TreatmentPatterns")
  pdfOutFile <- tempfile(fileext = ".pdf")
  pngOutFile <- tempfile(fileext = ".png")
  
  # pdf
  TreatmentPatterns::saveAsFile(
    fileName = inputFile,
    fileNameOut = pdfOutFile)
  
  expect_true(file.exists(pdfOutFile))
  
  # png
  TreatmentPatterns::saveAsFile(
    fileName = inputFile,
    fileNameOut = pngOutFile)
  
  expect_true(file.exists(pngOutFile))

  # cleanup
  unlink(x = c(pdfOutFile, pngOutFile))
})

test_that("Invalid input", {
  inputFile  <- system.file("shiny/html/custom.html", package= "TreatmentPatterns")
  
  expect_error(TreatmentPatterns::saveAsFile(fileName = NULL, fileNameOut = NULL))
  expect_error(TreatmentPatterns::saveAsFile(fileName = inputFile, fileNameOut = NULL))
  expect_error(TreatmentPatterns::saveAsFile(fileName = inputFile, fileNameOut = -1))
})