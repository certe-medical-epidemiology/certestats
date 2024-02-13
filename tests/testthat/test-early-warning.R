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

test_that("early warning works", {
  
  cases <- data.frame(date = sample(seq(as.Date("2018-01-01"),
                                        as.Date("2022-12-31"),
                                        "1 day"),
                                    size = 300),
                      patient = sample(LETTERS, size = 300, replace = TRUE))
  
  out1 <- cases |> early_warning_cluster()
  out2 <- cases |> early_warning_cluster(minimum_case_days = 99)
  
  expect_true(is.data.frame(format(out1)))
  expect_true(is.data.frame(format(out2)))
  expect_true(has_clusters(out2, -1))
  expect_type(has_ongoing_cluster(out2, "2021-01-01"), "logical")
  expect_type(has_cluster_before(out2, "2021-01-01"), "logical")
  expect_type(has_cluster_after(out2, "2021-01-01"), "logical")
  expect_identical(n_clusters(out2), n_distinct(out2$clusters$cluster))
  expect_message(print(out1))
  expect_message(print(out2))
})
