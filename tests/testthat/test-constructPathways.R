
context("constructPathways")

### Help functions
getDefaultPopulationSettings <- function(){
  default_settings <- formals(TreatmentPatterns::constructPathways)
  return(as.list(default_settings))
}

default_settings <- getDefaultPopulationSettings()

# TODO: add more informative error messages (return actual numbers?)
check_dataframe <- function(dataframe_result, dataframe_expected) {
  
  expect_true(all.equal(dataframe_result, dataframe_expected), info = "...")
  expect_true(identical(dataframe_result, dataframe_expected), info = "...")
}

# Test includeTreatmentsPriorToIndex

# Test minEraDuration

# Test splitEventCohorts

# Test eraCollapseSize

# Test combinationWindow

# Test minStepDuration

# Test filterTreatments

dataframe_expected <- data.frame(person_id = c(1, 1, 1, 1, 1, 2, 3),
                                 index_year = c(2010, 2010, 2010, 2010, 2010, 2011, 2011),
                                 event_cohort_id = c("11", "11", "12", "13" , "11", "12", "14"),
                                 event_start_date = c("2011-02-30", "2011-02-30", "2011-02-30", "2011-02-30", "2011-02-30","2011-02-30","2011-02-30"),
                                 event_end_date = c("2011-02-30", "2011-02-30", "2011-02-30", "2011-02-30", "2011-02-30","2011-02-30","2011-02-30"))

dataframe_result <- dataframe_expected

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



