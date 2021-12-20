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

test_that("nelson rules work", {
  x <- rnorm(10000)
  expect_gt(length(nelson_rule1(x)), 1)
  expect_gt(length(nelson_rule2(x)), 1)
  expect_gt(length(nelson_rule3(x)), 1)
  expect_gt(length(nelson_rule4(x)), 1)
  expect_gt(length(nelson_rule5(x)), 1)
  expect_gt(length(nelson_rule6(x)), 1)
  expect_gt(length(nelson_rule7(x)), 1)
  expect_gt(length(nelson_rule8(x)), 1)
  expect_true(is.character(nelson_text(1, 4)))
  expect_true(is.character(nelson_text(2, 4)))
  expect_true(is.character(nelson_text(3, 4)))
  expect_true(is.character(nelson_text(4, 4)))
  expect_true(is.character(nelson_text(5, 4)))
  expect_true(is.character(nelson_text(6, 4)))
  expect_true(is.character(nelson_text(7, 4)))
  expect_true(is.character(nelson_text(8, 4)))
})
