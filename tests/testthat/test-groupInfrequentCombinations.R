library(testthat)
library(TreatmentPatterns)

source(system.file(
  package = "TreatmentPatterns",
  "testing", "testParamsOutput.R"))

test_that("void", {
  expect_error(
    TreatmentPatterns:::groupInfrequentCombinations()
    )
})

test_that("minimal", {
  expect_s3_class(
    TreatmentPatterns:::groupInfrequentCombinations(
      data = treatment_pathways[[1]],
      groupCombinations = groupCombinations),
    "data.frame")
})



# get findCombinations
data <- treatment_pathways[[1]][1:5, ]

findCombinations <- apply(
  X = data,
  MARGIN = 2,
  FUN = function(x) {
    grepl("+", x, fixed = TRUE)
  })

combinations <- as.matrix(data)[findCombinations == TRUE]

test_that("validate grep \\+", {
  # Expect 3, hard coded from subset
  expect_equal(sum(findCombinations), length(combinations))
})

freqCombinations <- matrix(
  rep(data$freq, times = ncol(data)),
  ncol = ncol(data))[findCombinations == TRUE]

test_that("validate freq", {
  expect_true(
    all(freqCombinations %in% data$freq, TRUE))
})

summaryCombinations <- data.table::data.table(
  combination = combinations,
  freq = freqCombinations)

test_that("validate summary", {
  expect_true(
    all(grepl("\\+", summaryCombinations$combination), TRUE))
  
  expect_true(
    all(summaryCombinations$freq %in% data$freq, TRUE))
})

out <- summaryCombinations
summaryCombinations <- summaryCombinations[
  , .(freq = sum(freq)), by = combination][order(-freq)]


test_that("validate merge", {
  expect_true(
    all(unique(out$combination) %in% summaryCombinations$combination, TRUE))
})

summarizeCombinations <- summaryCombinations$combination[
  summaryCombinations$freq >= as.numeric(groupCombinations)]

test_that("validate subset", {
  expect_equal(
    summarizeCombinations,
    summaryCombinations$combination[
      summaryCombinations$freq == min(summaryCombinations$freq)])
})

selectedCombinations <- apply(
  X = data,
  MARGIN = 2,
  FUN = function(x) {
    x %in% summarizeCombinations
  })

out <- data
data[selectedCombinations] <- "Other"

test_that("validate other subset", {
  expect_equal(
    summarizeCombinations,
    as.data.frame(out)[selectedCombinations][1]
  )
  
  expect_equal(
    as.data.frame(data)[selectedCombinations][1],
    "Other"
  )
})
