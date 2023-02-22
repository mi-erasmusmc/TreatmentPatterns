library(TreatmentPatterns)
library(testthat)

tmpFile <- tempfile(pattern = "sqlOut", fileext = ".sql")

test_that("void", {
  expect_error(TreatmentPatterns:::loadRenderTranslateSql())
})

test_that("minimal", {
  expect_type(TreatmentPatterns:::loadRenderTranslateSql(
    sql = "SELECT TOP 10 * FROM person;"), "character")
})
