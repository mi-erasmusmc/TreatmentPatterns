library(testthat)
library(TreatmentPatterns)

test_that("void", {
  expect_error(TreatmentPatterns:::stripname())
})

test_that("minimal", {
  nestedList <- list(
    one = 1,
    two = 2,
    three = 3,
    nest = list(
      None = 1,
      Ntwo = 2,
      Nthree = 3))
  
  out <- TreatmentPatterns:::stripname(
    nestedList, "nest")
  
  expect_null(names(out$nest))
})
