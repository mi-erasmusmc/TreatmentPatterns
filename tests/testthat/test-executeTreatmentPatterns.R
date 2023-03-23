library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParams.R"))

test_that("void", {
  expect_error(TreatmentPatterns::executeTreatmentPatterns())
})

test_that("minimal", {
  TreatmentPatterns::executeTreatmentPatterns(
    dataSettings = dataSettings,
    pathwaySettings = pathwaySettings,
    saveSettings = saveSettings)

  expect_true(
    file.exists(normalizePath(file.path(saveSettings$rootFolder, "output.zip")))
    )
})
