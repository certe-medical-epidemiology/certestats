# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

test_that("moving averages work", {
  x <- rnorm(1000, mean = 10, sd = 1)
  expect_lt(cv(moving_average(x, 7)), cv(x))
  expect_lt(cv(moving_average(x, 7, side = "left")), cv(x))
  expect_lt(cv(moving_average(x, 7, side = "right")), cv(x))
  expect_lt(cv(moving_average(x, 7, side = "center")), cv(x))
  
  expect_gt(sum(moving_sum(x, 7)), sum(x))
  
  expect_lt(median(moving_Q1(x, 7)), median(x))
  expect_gt(median(moving_Q3(x, 7)), median(x))
})
