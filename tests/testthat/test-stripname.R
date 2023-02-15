library(testthat)
library(TreatmentPatterns)

simpleList <- list(
  one = 1,
  two = 2,
  three = 3)

nestedList <- c(simpleList, nest = list(
  None = 1,
  Ntwo = 2,
  Nthree = 3))

test_that("void", {
  expect_error(TreatmentPatterns:::stripname())
})

test_that("minimal", {
  out <- TreatmentPatterns:::stripname(x = nestedList, name = "nest")
  expect_null(names(out$nest))
  
  out <- TreatmentPatterns:::stripname(x = simpleList, name = "nest")
  expect_equal(out, simpleList)
})

test_that("invalid input", {
  expect_error(TreatmentPatterns:::stripname(x = nestedList, name = NULL))
  expect_error(TreatmentPatterns:::stripname(x = NULL, name = -1))
})
