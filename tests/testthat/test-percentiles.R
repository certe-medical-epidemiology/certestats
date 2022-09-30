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

test_that("percentiles work", {
  x <- c(0, 1, 2, 3, 4, 5, 5, 5, 5, 5, 5, 5, 6)
  expect_identical(percentiles(x),
                   c(1, 8, 17, 25, 33, 42, 42, 42, 42, 42, 42, 42, 100))
  expect_identical(deciles(x),
                   c(1, 1,  2,  2,  3,  5,  5,  5,  5,  5,  5,  5,  10))
})
