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

#' Distribution Metrics
#' 
#' These are simple distribution metric functions.
#' @param x values
#' @param na.rm a [logical] to indicate whether empty must be removed from `x`
#' @details These are the explanations of the functions:
#' * [se()] (standard error): sd / square root of length
#' @inheritSection math_functions Default values of `na.rm`
#' @rdname distribution_metrics
#' @name distribution_metrics
#' @export
se <- function(x, na.rm = getOption("na.rm", FALSE)) {
  if (na.rm == TRUE) {
    n <- length(stats::na.omit(x))
  } else {
    n <- length(x)
  }
  sd(x, na.rm = na.rm) / sqrt(n)
}

#' @rdname distribution_metrics
#' @param correct_mean with `TRUE` (the default) correct for the mean will be applied, by summing each square of `x` after the mean of `x` has been subtracted, so that this says something about `x`. With `FALSE`, all `x^2` are simply added together, so this says something about `x`'s location in the data.
#' @details * [sum_of_squares()]: sum of (x - mean(x)) ^ 2
#' @seealso For the sum of squares: <https://www.thoughtco.com/sum-of-squares-formula-shortcut-3126266>
#' @export
sum_of_squares <- function(x, correct_mean = TRUE, na.rm = getOption("na.rm", FALSE)) {
  if (isTRUE(correct_mean)) {
    sum((x - mean(x, na.rm = na.rm)) ^ 2, na.rm = na.rm)
  } else {
    sum(x ^ 2, na.rm = na.rm)
  }
}

#' @rdname distribution_metrics
#' @details * [cv()] (coefficient of variation): standard deviation / mean
#' @export
cv <- function(x, na.rm = getOption("na.rm", FALSE)) {
  sd(x, na.rm = na.rm) / abs(mean(x, na.rm = na.rm))
}

#' @rdname distribution_metrics
#' @details * [cqv()] (coefficient of quartile variation): (Q3 - Q1) / (Q3 + Q1)
#' @export
cqv <- function(x, na.rm = getOption("na.rm", FALSE)) {
  cqv.x <-
    (quantile(x, 0.75, na.rm = na.rm) - quantile(x, 0.25, na.rm = na.rm)) /
    (quantile(x, 0.75, na.rm = na.rm) + quantile(x, 0.25, na.rm = na.rm))
  unname(cqv.x)
}

#' @rdname distribution_metrics
#' @details * [z_score()] (number of standard deviations from the mean): (x - mean) / sd
#' @export
z_score <- function(x, na.rm = getOption("na.rm", FALSE)) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
}

#' @rdname distribution_metrics
#' @details * [midhinge()] (mean of interquartile range): (Q1 + Q3) / 2
#' @export
midhinge <- function(x, na.rm = getOption("na.rm", FALSE)) {
  unname((quantile(x, 0.25, na.rm = na.rm) + quantile(x, 0.75, na.rm = na.rm)) / 2)
}

#' @rdname distribution_metrics
#' @details * [ewma()] (exponentially weighted moving average)
#' @param lambda smoothing parameter, must be less than one. Under that condition, instead of equal weights, each squared return is weighted by a multiplier.
#' @export
ewma <- function(x, lambda, na.rm = getOption("na.rm", FALSE)) {
  x <- as.double(x)
  res <- mean(x)
  res.s <- vapply(X = x,
                  FUN = function(xs) res <<- res * lambda + xs * (1 - lambda),
                  FUN.VALUE = 0)
  res.s
}


#' @rdname distribution_metrics
#' @details * [rr_ewma()] (reversed-recombined exponentially weighted moving average)
#' @export
rr_ewma <- function(x, lambda, na.rm = getOption("na.rm", FALSE)) {
  # Reversed-Recombined EWMA
  # http://connor-johnson.com/2014/02/01/smoothing-with-exponentially-weighted-moving-averages/
  mapply(FUN = base::sum,
         # from 1 to end:
         ewma(x = x, lambda = lambda, na.rm = na.rm),
         # from end to 1:
         ewma(x = rev(x), lambda = lambda, na.rm = na.rm),
         na.rm = na.rm) / 2
}
