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

#' Normality Analysis
#' 
#' Check normality of a vector of values.
#' @param x vector of values
#' @importFrom broom tidy
#' @importFrom dplyr bind_rows
#' @inheritSection math_functions Default values of `na.rm`
#' @export
#' @examples 
#' x <- runif(1000)
#' normality(x)
#' 
#' x <- rnorm(1000)
#' normality(x)
#' 
#' x <- rexp(1000, rate = 3)
#' normality(x)
normality <- function(x, na.rm = getOption("na.rm", FALSE), ...) {
   x <- as.double(x)
   y <- seq_len(length(x))
   
   shapiro_wilk <- tidy(stats::shapiro.test(x))
   shapiro_wilk$interpretation <- ifelse(shapiro_wilk$p.value < 0.05,
                                         "Not normally distributed",
                                         "Normally distributed")
   shapiro_wilk$suggest <- suggest(stats::shapiro.test, x)
   
   kolmogorov_smirnov <- tidy(stats::ks.test(x, y = "pnorm"))
   kolmogorov_smirnov$interpretation <- ifelse(kolmogorov_smirnov$p.value < 0.05,
                                               "Not normally distributed",
                                               "Normally distributed")
   kolmogorov_smirnov$suggest <- suggest(stats::ks.test, x, y = "pnorm")
   kolmogorov_smirnov <- kolmogorov_smirnov[, colnames(kolmogorov_smirnov)[colnames(kolmogorov_smirnov) != "alternative"]]
   
   skew <- data.frame(statistic = skewness(x), method = "Skewness")
   skew$interpretation <- "0 in normal distribution"
   kurt <- data.frame(statistic = kurtosis(x), method = "Kurtosis")
   kurt$interpretation <- "3 in normal distribution"
   
   # plots
   par_bak <- par()$mfrow
   par(mfrow = c(1, 2)) 
   hist(x, col = "steelblue")
   stats::qqnorm(x)
   stats::qqline(x)
   par(mfrow = par_bak)
   
   out <- bind_rows(shapiro_wilk, kolmogorov_smirnov, skew, kurt)
   out$method <- gsub(" (normality )?test", "", out$method)
   out
}

#' @importFrom broom tidy
#' @importFrom dplyr case_when
suggest <- function(fn, x, ...) {
  p <- function(x) tidy(fn(x, ...))$p.value
  case_when(
    p(x) >= 0.05 ~ "(as is)",
    p(log(x)) >= 0.05 ~ "log(x)",
    p(sqrt(x)) >= 0.05 ~ "sqrt(x)",
    p(1 / x) >= 0.05 ~ "1 / x",
    p(x ^ (1 / 3)) >= 0.05 ~ "x ^ (1/3)",
    TRUE ~ NA_character_)
}

skewness <- function (x, na.rm = FALSE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  (sum((x - mean(x)) ^ 3) / n) / (sum((x - mean(x)) ^ 2) / n) ^ (3 / 2)
}

kurtosis <- function (x, na.rm = FALSE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  n * sum((x - mean(x)) ^ 4) / (sum((x - mean(x)) ^ 2) ^ 2)
}

