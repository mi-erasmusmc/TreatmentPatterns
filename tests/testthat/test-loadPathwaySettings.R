library(TreatmentPatterns)
library(testthat)

test_that("void", {
  expect_error(loadPathwaySettings())
})

test_that("minimal", {
  pathwaySettings <- loadPathwaySettings(file.path(system.file(
    package = "TreatmentPatterns"),
    "examples", "CDM", "settings", "pathway_settings.csv"))

  expect_s3_class(pathwaySettings, "pathwaySettings")
})

test_that("invalid path", {
  expect_warning(
    expect_error(
      loadPathwaySettings("some/random/file/path.csv"),
      "cannot open the connection"),
    "No such file or directory")
})

test_that("invalid file", {
  expect_error(
    loadPathwaySettings("output/cohort_table.csv"),
    "Must have exactly 15 rows"
  )
})
