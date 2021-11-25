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
#' @param x value
#' @param na.rm a [logical] to indicate whether empty must be removed from `x`
#' @details These are the functions:
#' * [se()] (standard error): sd / square root of length
#' @rdname distribution_metrics
#' @name distribution_metrics
#' @export
se <- function(x, na.rm = TRUE) {
  if (na.rm == TRUE) {
    n <- length(na.omit(x))
  } else {
    n <- length(x)
  }
  sd(x, na.rm = na.rm) / sqrt(n)
}

#' @rdname distribution_metrics
#' @details * [cv()] (coefficient of variation): standard deviation / mean
#' @export
cv <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm) / abs(mean(x, na.rm = na.rm))
}

#' @rdname distribution_metrics
#' @details * [cqv()] (coefficient of quartile variation): (Q3 - Q1) / (Q3 + Q1)
#' @export
cqv <- function(x, na.rm = TRUE) {
  cqv.x <-
    (quantile(x, 0.75, info = FALSE, na.rm = na.rm) - quantile(x, 0.25, info = FALSE, na.rm = na.rm)) /
    (quantile(x, 0.75, info = FALSE, na.rm = na.rm) + quantile(x, 0.25, info = FALSE, na.rm = na.rm))
  unname(cqv.x)
}

#' @rdname distribution_metrics
#' @details * [z_score()]: (x - mean) / sd
#' @export
z_score <- function(x, na.rm = TRUE) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
}

#' @rdname distribution_metrics
#' @details * [midhinge()]: (Q1 + Q3) / 2
#' @export
midhinge <- function(x, na.rm = TRUE) {
  unname((quantile(x, 0.25, info = FALSE, na.rm = na.rm) + quantile(x, 0.75, info = FALSE, na.rm = na.rm)) / 2)
}
