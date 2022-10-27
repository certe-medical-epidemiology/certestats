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

test_that("weighted mean works", {
  x <- c(0:1000)
  y <- c(0:1000)
  expect_equal(weighted_mean(x, y), 667)
  expect_equal(weighted_median(x, y), 707)
  expect_equal(weighted_Q1(x, y), 500)
  expect_equal(weighted_Q3(x, y), 866)
  expect_equal(weighted_fn(x, y, quantile, c(0.01, 0.99)), c("1%" = 100, "99%" = 995))
})
