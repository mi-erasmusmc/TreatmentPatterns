library(testthat)
library(TreatmentPatterns)

simpleList <- list("a")

nestedList <- c(simpleList,
  list(list("c", "d")))

test_that("void", {
  expect_error(TreatmentPatterns:::depth())
})

test_that("minimal", {
  expect_equal(TreatmentPatterns:::depth(x = c("a")), 0)
  expect_equal(TreatmentPatterns:::depth(x = simpleList), 1)
  expect_equal(TreatmentPatterns:::depth(x = nestedList), 2)
})

test_that("invalid input", {
  expect_error(TreatmentPatterns:::depth(x = NULL))
  expect_error(TreatmentPatterns:::depth(x = simpleList, thisdepth = -1))
})

