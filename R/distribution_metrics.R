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
#' * [se()] calculates the standard error: sd / square root of length
#' @inheritSection math_functions Default values of `na.rm`
#' @rdname distribution_metrics
#' @name distribution_metrics
#' @export
se <- function(x, na.rm = getOption("na.rm", FALSE)) {
  if (isTRUE(na.rm)) {
    n <- length(stats::na.omit(x))
  } else {
    n <- length(x)
  }
  sd(x, na.rm = na.rm) / sqrt(n)
}

#' @rdname distribution_metrics
#' @param level alpha level, defaults to 95%
#' @details * [ci()] calculates the confidence intervals for a mean (defaults at 95%), which returns length 2 
#' @export
ci <- function(x, level = 0.95, na.rm = getOption("na.rm", FALSE)) {
  if (isTRUE(na.rm)) {
    n <- length(stats::na.omit(x))
  } else {
    n <- length(x)
  }
  out <- stats::qt(p = 1 - (1 - 0.95) / 2, df = n - 1) * se(x, na.rm = na.rm)
  mean(x, na.rm = na.rm) + c(- out, out)
}

#' @rdname distribution_metrics
#' @param correct_mean with `TRUE` (the default) correct for the mean will be applied, by summing each square of `x` after the mean of `x` has been subtracted, so that this says something about `x`. With `FALSE`, all `x^2` are simply added together, so this says something about `x`'s location in the data.
#' @details * [sum_of_squares()] calculates the sum of (x - mean(x)) ^ 2
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
#' @details * [cv()] calculates the coefficient of variation: standard deviation / mean
#' @export
cv <- function(x, na.rm = getOption("na.rm", FALSE)) {
  sd(x, na.rm = na.rm) / abs(mean(x, na.rm = na.rm))
}

#' @rdname distribution_metrics
#' @details * [cqv()] calculates the coefficient of quartile variation: (Q3 - Q1) / (Q3 + Q1)
#' @export
cqv <- function(x, na.rm = getOption("na.rm", FALSE)) {
  cqv.x <-
    (quantile(x, 0.75, na.rm = na.rm) - quantile(x, 0.25, na.rm = na.rm)) /
    (quantile(x, 0.75, na.rm = na.rm) + quantile(x, 0.25, na.rm = na.rm))
  unname(cqv.x)
}

#' @rdname distribution_metrics
#' @details * [z_score()] calculates the number of standard deviations from the mean: (x - mean) / sd
#' @export
z_score <- function(x, na.rm = getOption("na.rm", FALSE)) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
}

#' @rdname distribution_metrics
#' @details * [midhinge()] calculates the mean of interquartile range: (Q1 + Q3) / 2
#' @export
midhinge <- function(x, na.rm = getOption("na.rm", FALSE)) {
  unname((quantile(x, 0.25, na.rm = na.rm) + quantile(x, 0.75, na.rm = na.rm)) / 2)
}

#' @rdname distribution_metrics
#' @details * [ewma()] calculates the EWMA (exponentially weighted moving average)
#' @param lambda smoothing parameter, a value between 0 and 1. A value of 0 is equal to `x`, a value of 1 equal to the *mean* of `x`. The EWMA looks back and has a delay - the rrEWMA takes the mean of a 'forward' and 'backward' EWMA.
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
#' @details * [rr_ewma()] calculates the rrEWMA (reversed-recombined exponentially weighted moving average)
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

#' @rdname distribution_metrics
#' @param n number to be normalised
#' @param n_ref reference to use for normalisation
#' @param per normalisation factor
#' @details * [normalise()] normalises the data based on a reference: (n / reference) * unit
#' @export
normalise <- function(n, n_ref, per = 1000) {
  (n / n_ref) * per
}

#' @rdname distribution_metrics
#' @export
normalize <- normalise

#' @rdname distribution_metrics
#' @details * [scale_sd()] normalises the data to have a standard deviation of 1, while retaining the mean
#' @export
scale_sd <- function(x) {
  z_score(x, na.rm = TRUE) + mean(x, na.rm = TRUE)
}

#' @rdname distribution_metrics
#' @details * [centre_mean()] normalises the data to have a mean of 0, while retaining the standard deviation
#' @export
centre_mean <- function(x) {
  x - mean(x, na.rm = TRUE)
}
