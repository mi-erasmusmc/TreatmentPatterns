library(TreatmentPatterns)
library(testthat)

source(system.file(
  package = "TreatmentPatterns",
  "examples", "SettingObjects", "createDummySettings.R"))

source(system.file(
  package = "TreatmentPatterns",
  "testing",
  "testParams.R"))

test_that("void", {
  expect_error(TreatmentPatterns:::doCombinationWindow())
})

test_that("minimal", {
  
  invisible(capture.output({
    treatment_history <- TreatmentPatterns:::doCombinationWindow(
      doEraCollapseTH,
      combinationWindow,
      minPostCombinationDuration)
  }))
    
  expect_s3_class(treatment_history, "data.frame")
})


# Test cases: Allen's interval algebra
# https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

# Assume there are two eras to be combined. They are different events.
# Without loss of generality, assume A always starts before or on the 
# same day that B starts.

## Possible relations for two intervals A and B (* means overlap)
# A equals B              *************
# A precedes B            aaaaaaa   bbb
# A meets B               aaaaaaabbbbbb
# A overlaps with B       aaaa****bbbbb
# A starts B              ********bbbbb
# A contains B            aaa*****aaaaa
# A is finished by B      aaaaaaaa*****

test_that("case: A is equal to B", {
  
  treatment_history <- tibble::tribble(
    ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-05-01",       "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  treatment_history[,
     `:=`(duration_era = difftime(event_end_date, event_start_date, units = "days"),
          index_year = as.numeric(format(event_start_date, "%Y")))]
  
  invisible(capture.output({
    result <- doCombinationWindow(treatment_history, 
                                  combinationWindow = 1, 
                                  minPostCombinationDuration = 1)[,
     c("person_id", "event_cohort_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
   ~person_id, ~event_cohort_id, ~event_start_date, ~event_end_date,
   1,          "102+101",        "2020-05-01",      "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result, expected_result)
  
  # case when eras are too short. But which will be chosen? A or B?
  invisible(capture.output({
    result2 <- doCombinationWindow(treatment_history, 
                                  combinationWindow = 1000, 
                                  minPostCombinationDuration = 1)[,
    c("person_id", "event_cohort_id", "event_start_date", "event_end_date")]
  }))
  
  # TODO what should the result be in this case
  # expected_result <- tibble::tribble(
  #   ~person_id, ~event_cohort_id, ~event_start_date, ~event_end_date,
  #   1,          "102+101",        "2020-05-01",      "2020-06-01"
  # ) %>%
  #   dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
  #   data.table::data.table()
  
  
  # expect_equal(result, expected_result)
  
})


test_that("case: A precedes B", {
  
  treatment_history <- tibble::tribble(
    ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-06-02",       "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  treatment_history[,
   `:=`(duration_era = difftime(event_end_date, event_start_date, units = "days"),
        index_year = as.numeric(format(event_start_date, "%Y")))]
  
  invisible(capture.output({
    result <- doCombinationWindow(treatment_history, 
                                  combinationWindow = 1, 
                                  minPostCombinationDuration = 1)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "101",            1,          "2020-05-01",      "2020-06-01",
    "102",            1,          "2020-06-02",      "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result, expected_result)
  
  # case when min combination lenght and min post combination duration are large
  invisible(capture.output({
    result2 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 1000, 
                                   minPostCombinationDuration = 1000)[,
    c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expect_equal(result2, expected_result)
})


test_that("case: A meets B", {
  skip("failing test")
  
  treatment_history <- tibble::tribble(
    ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-06-01",       "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  treatment_history[,
                    `:=`(duration_era = difftime(event_end_date, event_start_date, units = "days"),
                         index_year = as.numeric(format(event_start_date, "%Y")))]
  
  invisible(capture.output({
    result <- doCombinationWindow(treatment_history, 
                                  combinationWindow = 0, 
                                  minPostCombinationDuration = 0)[,
    c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "101",            1,          "2020-05-01",      "2020-05-31",
    "102+101",        1,          "2020-06-01",      "2020-06-01",
    "102",            1,          "2020-06-02",      "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result, expected_result)
  
  # case when overlap < combinationWindow
  invisible(capture.output({
    result2 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 2, 
                                   minPostCombinationDuration = 0)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "101",            1,          "2020-05-01",      "2020-05-31",
    "102",            1,          "2020-06-01",      "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result2, expected_result)
  
  # case when min post combination duration are large
  invisible(capture.output({
    result3 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 0, 
                                   minPostCombinationDuration = 1000)[,
    c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  # Note that minPostCombinationDuration restrict pre and post duration
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "102+101",        1,          "2020-06-01",      "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result3, expected_result)
})



test_that("case: A overlaps B", {
  
  skip("failing test")
  
  treatment_history <- tibble::tribble(
    ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
    101,                   1,           "2020-05-01",       "2020-06-20",
    102,                   1,           "2020-06-15",       "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  treatment_history[,
     `:=`(duration_era = difftime(event_end_date, event_start_date, units = "days"),
          index_year = as.numeric(format(event_start_date, "%Y")))]
  
  invisible(capture.output({
    result <- doCombinationWindow(treatment_history, 
                                  combinationWindow = 0, 
                                  minPostCombinationDuration = 0)[,
     c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  # This person should not be in an event an a combo event on the same day
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "101",            1,          "2020-05-01",      "2020-06-14",
    "102+101",        1,          "2020-06-15",      "2020-06-20",
    "102",            1,          "2020-06-21",      "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result, expected_result)
  
  # case when overlap < combinationWindow
  invisible(capture.output({
    result2 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 30, 
                                   minPostCombinationDuration = 0)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "101",            1,          "2020-05-01",      "2020-06-14",
    "102",            1,          "2020-06-15",      "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result2, expected_result)
  
  # case when minPostCombinationDuration is large
  invisible(capture.output({
    result3 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 0, 
                                   minPostCombinationDuration = 1000)[,
     c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "102+101",        1,          "2020-06-15",      "2020-06-20"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result3, expected_result)
})


test_that("case: A starts B", {
  
  skip("failing test")
  
  treatment_history <- tibble::tribble(
    ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-05-01",       "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  treatment_history[,
    `:=`(duration_era = difftime(event_end_date, event_start_date, units = "days"),
         index_year = as.numeric(format(event_start_date, "%Y")))]
  
  invisible(capture.output({
    result <- doCombinationWindow(treatment_history, 
                                  combinationWindow = 0, 
                                  minPostCombinationDuration = 0)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  # This person should not be in an event an a combo event on the same day
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "102+101",        1,          "2020-05-01",      "2020-06-01",
    "102",            1,          "2020-06-02",      "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result, expected_result)
  
  # case when overlap < combinationWindow (i.e. A is contained in B but 
  # the overlap is not long enough to count as a combination)
  invisible(capture.output({
    result2 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 100, 
                                   minPostCombinationDuration = 0)[,
     c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "102",        1,          "2020-05-01",      "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result2, expected_result)
  
  # case when minPostCombinationDuration is large
  invisible(capture.output({
    result3 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 0, 
                                   minPostCombinationDuration = 1000)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "102+101",        1,          "2020-05-01",      "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result3, expected_result)
  
  # TODO what should the output be in this case?
  # case when both combinationWindow and minPostCombinationDuration are large
  # invisible(capture.output({
  #   result4 <- doCombinationWindow(treatment_history, 
  #                                  combinationWindow = 1000, 
  #                                  minPostCombinationDuration = 1000)[,
  #     c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  # }))
  
  # expected_result <- tibble::tribble(
  #   ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
  #   "102+101",        1,          "2020-05-01",      "2020-06-01"
  # ) %>%
  #   dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
  #   data.table::data.table()
  # 
  # expect_equal(result3, expected_result)
})



test_that("case: A contains B", {
  
  skip("failing test")
  
  treatment_history <- tibble::tribble(
    ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-05-11",       "2020-05-21"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  treatment_history[,
    `:=`(duration_era = difftime(event_end_date, event_start_date, units = "days"),
         index_year = as.numeric(format(event_start_date, "%Y")))]
  
  invisible(capture.output({
    result <- doCombinationWindow(treatment_history, 
                                  combinationWindow = 0, 
                                  minPostCombinationDuration = 0)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  # This person should not be in an event an a combo event on the same day
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "101",            1,          "2020-05-01",      "2020-05-10",
    "102+101",        1,          "2020-05-11",      "2020-05-21",
    "101",            1,          "2020-05-22",      "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result, expected_result)
  
  # TODO: what should the result be in this case?
  # case when overlap < combinationWindow (i.e. A is contained in B but 
  # the overlap is not long enough to count as a combination)
  invisible(capture.output({
    result2 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 100, 
                                   minPostCombinationDuration = 0)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "101",        1,          "2020-05-01",      "2020-05-10",
    "102",        1,          "2020-05-11",      "2020-05-21",
    "101",        1,          "2020-05-22",      "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result2, expected_result)
  
  # case when minPostCombinationDuration is large
  invisible(capture.output({
    result3 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 0, 
                                   minPostCombinationDuration = 1000)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "102+101",        1,          "2020-05-11",      "2020-05-21"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result3, expected_result)
  
  # TODO what should the output be in this case?
  # case when both combinationWindow and minPostCombinationDuration are large
  # invisible(capture.output({
  #   result4 <- doCombinationWindow(treatment_history, 
  #                                  combinationWindow = 1000, 
  #                                  minPostCombinationDuration = 1000)[,
  #     c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  # }))
  
  # expected_result <- tibble::tribble(
  #   ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
  #   "102+101",        1,          "2020-05-01",      "2020-06-01"
  # ) %>%
  #   dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
  #   data.table::data.table()
  # 
  # expect_equal(result3, expected_result)
})

test_that("case: A is finished by B", {
  
  skip("failing test")
  
  treatment_history <- tibble::tribble(
    ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-05-11",       "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  treatment_history[,
    `:=`(duration_era = difftime(event_end_date, event_start_date, units = "days"),
         index_year = as.numeric(format(event_start_date, "%Y")))]
  
  invisible(capture.output({
    result <- doCombinationWindow(treatment_history, 
                                  combinationWindow = 0, 
                                  minPostCombinationDuration = 0)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  # This person should not be in an event an a combo event on the same day
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "101",            1,          "2020-05-01",      "2020-05-10",
    "102+101",        1,          "2020-05-11",      "2020-06-01",
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result, expected_result)
  
  # TODO: what should the result be in this case?
  # case when overlap < combinationWindow (i.e. A is contained in B but 
  # the overlap is not long enough to count as a combination)
  invisible(capture.output({
    result2 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 100, 
                                   minPostCombinationDuration = 0)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "101",            1,          "2020-05-01",      "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result2, expected_result)
  
  # case when minPostCombinationDuration is large
  invisible(capture.output({
    result3 <- doCombinationWindow(treatment_history, 
                                   combinationWindow = 0, 
                                   minPostCombinationDuration = 1000)[,
     c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  expected_result <- tibble::tribble(
    ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
    "102+101",        1,          "2020-05-11",      "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
    data.table::data.table()
  
  expect_equal(result3, expected_result)
  
  # TODO what should the output be in this case?
  # case when both combinationWindow and minPostCombinationDuration are large
  invisible(capture.output({
    result4 <- doCombinationWindow(treatment_history,
                                   combinationWindow = 1000,
                                   minPostCombinationDuration = 1000)[,
      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
  }))
  
  # expected_result <- tibble::tribble(
  #   ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
  #   "102+101",        1,          "2020-05-01",      "2020-06-01"
  # ) %>%
  #   dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>% 
  #   data.table::data.table()
  # 
  expect_equal(nrow(result3), 0)
})


