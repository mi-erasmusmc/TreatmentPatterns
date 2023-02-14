library(testthat)
library(TreatmentPatterns)

test_that("void", {
  expect_error(TreatmentPatterns:::depth())
})

test_that("minimal", {
  nestedList <- list(
    "a",
    list("b", "c"))
  
  expect_equal(TreatmentPatterns:::depth(nestedList), 2)
})
