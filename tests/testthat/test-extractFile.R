library(testthat)
library(TreatmentPatterns)
library(Eunomia)
library(DatabaseConnector)

connection <- connect(getEunomiaConnectionDetails())

test_that("void", {
  expect_error(
    TreatmentPatterns:::extractFile())
  })

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::extractFile(
    connection = connection,
    tableName = "person",
    resultsSchema = "main",
    dbms = "sqlite"), "data.frame")
})
