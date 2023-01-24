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

test_that("regression works", {
  expect_s3_class(regression(runif(10)), "certestats_reg")
  expect_s3_class(regression(1:10, runif(10)), "certestats_reg")
  expect_s3_class(regression(runif(10)) |> plot(), "check_model")
  expect_s3_class(regression(runif(10)) |> autoplot(), "gg")
  expect_s3_class(data.frame(x = 1:50, y = runif(50)) |> regression(x, y), "certestats_reg")
})
