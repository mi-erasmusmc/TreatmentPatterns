library(TreatmentPatterns)
library(testthat)

test_that("Void", {
  expect_error(TreatmentPatterns::saveAsFile())
})

test_that("pdf", {
  testthat::skip_if(.Platform$OS.type == "windows", message = "Debug port unstable on Windows")
  inputFile <- system.file("shiny/html/custom.html", package= "TreatmentPatterns")
  pdfOutFile <- tempfile(fileext = ".pdf")

  TreatmentPatterns::saveAsFile(
    fileName = inputFile,
    fileNameOut = pdfOutFile)
  
  expect_true(file.exists(pdfOutFile))

  # cleanup
  unlink(x = c(pdfOutFile))
})

test_that("png", {
  testthat::skip_if(.Platform$OS.type == "windows", message = "Debug port unstable on Windows")
  inputFile <- system.file("shiny/html/custom.html", package= "TreatmentPatterns")
  pngOutFile <- tempfile(fileext = ".png")
  
  TreatmentPatterns::saveAsFile(
    fileName = inputFile,
    fileNameOut = pngOutFile,
    selector = NULL)
  
  expect_true(file.exists(pngOutFile))
  
  # cleanup
  unlink(x = c(pngOutFile))
})

test_that("Invalid input", {
  inputFile  <- system.file("shiny/html/custom.html", package= "TreatmentPatterns")
  
  expect_error(TreatmentPatterns::saveAsFile(fileName = NULL, fileNameOut = NULL))
  expect_error(TreatmentPatterns::saveAsFile(fileName = inputFile, fileNameOut = NULL))
  expect_error(TreatmentPatterns::saveAsFile(fileName = inputFile, fileNameOut = -1))
})
