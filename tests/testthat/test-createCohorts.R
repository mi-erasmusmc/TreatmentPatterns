library(testthat)
library(TreatmentPatterns)

source("inst/examples/R Settings Objects/createDummySettings.R")

test_that("Void", {
  expect_error(createCohorts())
})

test_that("Minimal", {
  dataSettings$connectionDetails <-
    Eunomia::getEunomiaConnectionDetails()
  
  expect_warning(
    createCohorts(
      dataSettings = dataSettings,
      cohortSettings = cohortSettings,
      saveSettings = saveSettings
    ),
    c("Cohort definition 1,")
  )
})

test_that("Parameter order", {
  dataSettings$connectionDetails <-
    Eunomia::getEunomiaConnectionDetails()
  
  expect_warning(
    createCohorts(dataSettings,
                  cohortSettings,
                  saveSettings),
    c("Cohort definition 1,")
  )
})

test_that("Wrong dataSettings class", {
  expect_error(createCohorts(1,
                             cohortSettings,
                             saveSettings),
               c("Incorrect class"))
})

test_that("Wrong cohortSettings class", {
  dataSettings$connectionDetails <-
    Eunomia::getEunomiaConnectionDetails()
  
  expect_error(createCohorts(dataSettings,
                             1,
                             saveSettings),
               c("Incorrect class"))
})

test_that("Wrong saveSettings class", {
  dataSettings$connectionDetails <-
    Eunomia::getEunomiaConnectionDetails()
  
  expect_error(createCohorts(dataSettings,
                             cohortSettings,
                             1),
               c("Incorrect class"))
})

test_that("Table check", {
  dataSettings$connectionDetails <-
    Eunomia::getEunomiaConnectionDetails()
  
  expect_warning(
    createCohorts(dataSettings,
                  cohortSettings,
                  saveSettings),
    c("Cohort definition 1,")
  )
  
  connection <-
    DatabaseConnector::connect(dataSettings$connectionDetails)
  
  sql <- paste0(
    "SELECT name ",
    "FROM sqlite_master ",
    "WHERE type='table' ",
    "AND name='",
    dataSettings$cohortTable,
    "';"
  )
  
  expect_equal(
    DatabaseConnector::renderTranslateQuerySql(connection, sql)$NAME,
    dataSettings$cohortTable
  )
  
  DatabaseConnector::disconnect(connection)
})
