library(testthat)
library(TreatmentPatterns)
library(Eunomia)
library(DatabaseConnector)

connection <- connect(getEunomiaConnectionDetails())

test_that("void", {
  expect_error(
    TreatmentPatterns::extractFile())
  })

test_that("minimal", {
  data <- TreatmentPatterns::extractFile(
    connection = connection,
    tableName = "person",
    resultsSchema = "main",
    dbms = "sqlite")
  
  expect_s3_class(data, "data.frame")
  expect_equal(dim(data), c(2694, 18))
  expect_equal(colnames(data)[1], "PERSON_ID")
})

test_that("invalid input", {
  expect_error(TreatmentPatterns::extractFile(connection = NULL, 
                                              tableName = "person", 
                                              resultsSchema = "main",
                                              dbms = "sqlite"))
  expect_error(TreatmentPatterns::extractFile(connection = connection, 
                                              tableName = NULL, 
                                              resultsSchema = "main",
                                              dbms = "sqlite"))
  expect_error(TreatmentPatterns::extractFile(connection = connection, 
                                              tableName = "person", 
                                              resultsSchema = NULL,
                                              dbms = "sqlite"))
  expect_error(TreatmentPatterns::extractFile(connection = connection, 
                                              tableName = "person", 
                                              resultsSchema = "main",
                                              dbms = NULL))
})
