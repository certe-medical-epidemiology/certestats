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

#' Weighted Mean
#'
#' Functions to calculate a weighted mean or any other metric.
#' @param x vector of values
#' @param w weights, length 1 or length of `x`
#' @param fun function to apply
#' @param na.rm a [logical] to indicate whether empty must be removed from `x`
#' @param ... arguments passed on to `fun`
#' @inheritSection math_functions Default values of `na.rm`
#' @rdname weighted_mean
#' @export
#' @examples
#' x <- c(1:10)
#' y <- c(1:10)
#' 
#' mean(x)
#' weighted_mean(x, y)
#' 
#' # essentially equal to:
#' mean(rep(x, y))
#' 
#' x <- c(0:1000)
#' y <- c(0:1000)
#' mean(x)
#' weighted_mean(x, y)
#' weighted_median(x, y)
#' weighted_Q1(x, y)
#' weighted_Q3(x, y)
#' weighted_fn(x, y, quantile, c(0.01, 0.99))
weighted_mean <- function(x, w, na.rm = getOption("na.rm", FALSE)) {
  weighted_fn(x = x, w = w, fun = mean, na.rm = na.rm)
}

#' @rdname weighted_mean
#' @export
weighted_median <- function(x, w, na.rm = getOption("na.rm", FALSE)) {
  weighted_fn(x = x, w = w, fun = median, na.rm = na.rm)
}


#' @rdname weighted_mean
#' @export
weighted_Q1 <- function(x, w, na.rm = getOption("na.rm", FALSE)) {
  unname(weighted_fn(x = x, w = w, fun = quantile, na.rm = na.rm, 0.25))
}

#' @rdname weighted_mean
#' @export
weighted_Q3 <- function(x, w, na.rm = getOption("na.rm", FALSE)) {
  unname(weighted_fn(x = x, w = w, fun = quantile, na.rm = na.rm, 0.75))
}

#' @rdname weighted_mean
#' @export
weighted_fn <- function(x, w, fun, ...) {
  stopifnot(is.numeric(x), is.numeric(w), is.function(fun))
  if (length(w) == 1) {
    w <- rep(w, length(x))
  }
  if (length(w) != length(x)) {
    stop("'w' must have the length as 'x'.")
  }
  fun(rep(x, w), ...)
}
