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

test_that("math functions work", {
  x <- rnorm(1000)
  expect_identical(se(x, na.rm = FALSE),
                   sd(x, na.rm = FALSE) /
                     sqrt(length(x)))
  expect_identical(cv(x, na.rm = FALSE),
                   sd(x, na.rm = FALSE) /
                     abs(mean(x, na.rm = FALSE)))
  expect_identical(cqv(x, na.rm = FALSE),
                   unname((quantile(x, 0.75, na.rm = FALSE) -
                             quantile(x, 0.25, na.rm = FALSE)) /
                            (quantile(x, 0.75, na.rm = FALSE) +
                               quantile(x, 0.25, na.rm = FALSE))))
  expect_identical(z_score(x, na.rm = FALSE),
                   (x - mean(x, na.rm = FALSE)) /
                     sd(x, na.rm = FALSE))
  expect_identical(midhinge(x, na.rm = FALSE),
                   unname((quantile(x, 0.25, na.rm = FALSE) +
                             quantile(x, 0.75, na.rm = FALSE)) / 2))
  expect_identical(sum_of_squares(x, correct_mean = FALSE),
                   sum(x ^ 2, na.rm = FALSE))
  
  expect_lt(cv(rr_ewma(x, 0.99)), cv(x))
  
  expect_identical(mean_harmonic(x),
                   1 / mean(1 / x))
  expect_identical(mean_geometric(x),
                   exp(mean(log(abs(x)))))
  
  x <- c(10:20, NA)
  old <- options()$na.rm
  options(na.rm = TRUE)
  expect_identical(any(x), base::any(x, na.rm = TRUE))
  expect_identical(all(x), base::all(x, na.rm = TRUE))
  expect_identical(mean(x), base::mean(x, na.rm = TRUE))
  expect_identical(sum(x), base::sum(x, na.rm = TRUE))
  expect_identical(prod(x), base::prod(x, na.rm = TRUE))
  expect_identical(min(x), base::min(x, na.rm = TRUE))
  expect_identical(max(x), base::max(x, na.rm = TRUE))
  expect_identical(pmin(x), base::pmin(x, na.rm = TRUE))
  expect_identical(pmax(x), base::pmax(x, na.rm = TRUE))
  expect_identical(range(x), base::range(x, na.rm = TRUE))
  expect_identical(sd(x), stats::sd(x, na.rm = TRUE))
  expect_identical(var(x), stats::var(x, na.rm = TRUE))
  expect_identical(median(x), stats::median(x, na.rm = TRUE))
  expect_identical(fivenum(x), stats::fivenum(x, na.rm = TRUE))
  expect_identical(quantile(x), stats::quantile(x, na.rm = TRUE))
  expect_identical(IQR(x), stats::IQR(x, na.rm = TRUE))
  options(na.rm = old)
  
  df <- data.frame(n = c(1000, 2000, 4000),
                   ref = c(50, 100, 200))
  expect_identical(normalise(df$n, df$ref), c(20000, 20000, 20000))
  expect_identical(normalize(df$n, df$ref, 10), c(200, 200, 200))
  
  x <- rnorm(100, 250, 50)
  expect_gt(ci(x)[1], mean(x) - sd(x))
  expect_lt(ci(x, na.rm = TRUE)[2], mean(x) + sd(x))
  
  expect_equal(sd(scale_sd(x)), 1)
  expect_equal(round(mean(centre_mean(x)), 5), 0)
})
