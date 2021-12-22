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

test_that("QC rules work", {
  x <- c(-3.12, -0.04,  1.59, -2.70, -1.11,  1.40,  0.46,  0.63, -0.32,
         -0.75, -0.34, -2.11,  0.25, -0.76,  1.02, -0.49, -0.91,  1.50,
         1.91,  1.10, -1.48,  0.00,  0.43,  0.01,  2.16,  1.80,  0.14,
         0.89,  1.03,  0.43, -0.68, -0.69, -1.71, -0.71,  0.02, -0.95,
         -1.12, -0.11,  0.14,  1.27, -0.40, -0.27,  1.43,  0.66,  0.13,
         0.09, -0.15, -1.47, -1.42,  2.33,  0.23, -0.28, -0.09, -0.02,
         -0.12,  0.66, -1.31, -2.06,  0.84,  1.25, -0.23,  1.03,  0.67,
         0.70,  1.10, -0.74,  1.06, -1.22,  0.71, -0.71,  0.47, -0.14,
         1.03, -0.84, -0.20, -1.31, -0.90, -2.92,  0.22,  0.62, -0.75,
         0.59, -0.36, -0.19, -2.65,  0.73, -2.24, -0.40,  0.11, -0.49,
         2.14, -0.37, -1.23,  0.64, -0.22,  1.07,  1.63,  0.81,  0.05,
         -0.58, -0.87, -0.30, -0.02,  0.55,  1.11,  1.18,  1.13, -1.09,
         1.85,  1.69,  0.36,  1.85,  0.34, -1.23, -0.15, -0.69, -1.90,
         -0.26,  0.44, -1.31,  0.58,  1.92, -0.96,  0.38,  0.21,  0.02,
         -0.23,  0.73, -0.82, -0.88,  0.75, -0.21, -0.85,  0.62,  0.87,
         -0.85,  0.83, -0.68, -0.04, -0.82,  0.01,  0.79,  1.47, -0.80,
         1.30, -1.10,  1.74,  1.38, -1.21, -1.75, -2.01,  1.73, -1.45,
         2.25,  1.17, -1.78,  0.24, -1.12, -0.46, -0.33,  1.52,  1.65,
         -0.51, -0.52,  2.18, -0.14, -0.92, -0.71,  3.00,  3.00,  3.00,
         3.00,  3.00,  3.00,  3.00,  3.00,  3.00,  3.00,  5.00)
  expect_gte(length(qc_rule1(x)), 1)
  expect_gte(length(qc_rule2(x)), 1)
  expect_gte(length(qc_rule3(x)), 1)
  expect_gte(length(qc_rule4(x)), 1)
  expect_gte(length(qc_rule5(x)), 1)
  expect_gte(length(qc_rule6(x)), 1)
  expect_gte(length(qc_rule7(x)), 1)
  expect_gte(length(qc_rule8(x)), 1)
  expect_true(is.character(qc_rule_text(1, 4)))
  expect_true(is.character(qc_rule_text(2, 4)))
  expect_true(is.character(qc_rule_text(3, 4)))
  expect_true(is.character(qc_rule_text(4, 4)))
  expect_true(is.character(qc_rule_text(5, 4)))
  expect_true(is.character(qc_rule_text(6, 4)))
  expect_true(is.character(qc_rule_text(7, 4)))
  expect_true(is.character(qc_rule_text(8, 4)))
  expect_s3_class(qc_test(x, guideline = "Nelson"), "qc_test")
  expect_s3_class(qc_test(x, guideline = "Westgard"), "qc_test")
  expect_s3_class(qc_test(x, guideline = "AIAG"), "qc_test")
  expect_s3_class(qc_test(x, guideline = "Montgomery"), "qc_test")
  expect_s3_class(qc_test(x, guideline = "Healthcare"), "qc_test")
  expect_output(print(qc_test(x)))
  expect_s3_class(as.data.frame(qc_test(x)), "data.frame")
})
