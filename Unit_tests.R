## Unit Tests for all valid functions 

########## Log: ##########
## All passed on December 17th, 2022 
## R version 4.2.1 (2022-06-23)
## 13.0.1 (22A400)

library(testthat)
input_test <- c(1, 2, 3, 4, 5, 6, 7)
lag_val<- 1


lagR(input_test, lag_val = -1,method='wrap')

test_that("wrap positive", {
  input_test <- c(1, 2, 3, 4, 5, 6, 7)
  lag_val<- 1
  expected <- c( 7,1, 2, 3, 4, 5, 6)
  output<-lagR(input_test, lag_val = 1,method='wrap')
  
  expect_equal(output$lagged, expected)
})


test_that("wrap negative", {
  input_test <- c(1, 2, 3, 4, 5, 6, 7)
  lag_val<- 1
  expected <- c(2, 3, 4, 5, 6, 7, 1)
  output<-lagR(input_test, lag_val = -1,method='wrap')
  
  expect_equal(output$lagged, expected)
})


test_that("blank positive no replacement (while dropping the NaN in each)", {
  input_test <- c(1, 2, 3, 4, 5, 6, 7)
  lag_val<- 1
  expected <- c(NaN,   1,   2,   3,   4,   5,   6,   7)
  cat("The original expectation is", expected)
  expected<- tail(expected,-1)
  cat("The updated expectation is", expected)
  output<-lagR(input_test, lag_val = 1,method='blank')
  cat("The original output is", output$lagged)
  output$lagged<-tail(output$lagged,-1)
  cat("The updated output is", output$lagged)
  
  
  expect_equal(output$lagged, expected)
})


test_that("blank negative no replacement (while dropping the NaN in each)", {
  ## NOTE THAT THE WARNING IS INTRINSIC HERE, SO IT IS SUPRESSED
  input_test <- c(1, 2, 3, 4, 5, 6, 7)
  lag_val<- 1
  expected <- c( 2,   3,   4,   5,   6,   7,NaN)
  cat("The original expectation is", expected)
  expected<- head(expected,-1)
  cat("The updated expectation is", expected)
  suppressWarnings({output<-lagR(input_test, lag_val = -1,method='blank')})
  
  cat("The original output is", output$lagged)
  output$lagged<-head(output$lagged,-1)
  cat("The updated output is", output$lagged)
  
  
  expect_equal(output$lagged, expected)
})

test_that("blank negative with replacement ", {
  ## NOTE THAT THE WARNING IS INTRINSIC HERE, SO IT IS SUPRESSED
  input_test <- c(1, 2, 3, 4, 5, 6, 7)
  lag_val<- 1
  replacement_vector=11
  expected <- c( 2,   3,   4,   5,   6,   7,11)
  cat("The original expectation is", expected)

  suppressWarnings({output<-lagR(input_test, lag_val = -1,method='blank',replace = TRUE, replacement_vector)})
  
  cat("The original output is", output$lagged)

  expect_equal(output$lagged, expected)
})

test_that("blank positive with replacement ", {
  input_test <- c(1, 2, 3, 4, 5, 6, 7)
  lag_val<- 1
  replacement_vector=11
  expected <- c( 11, 1, 2,   3,   4,   5,   6,   7)
  cat("The original expectation is", expected)

  output<-lagR(input_test, lag_val = 1,method='blank',replace =TRUE ,replacement_vector)
  
  cat("The original output is", output$lagged)

  
  
  expect_equal(output$lagged, expected)
})



