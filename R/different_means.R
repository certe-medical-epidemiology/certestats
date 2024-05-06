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

#' Different Means
#' 
#' Functions to determine harmonic and geometric means.
#' @param x numeric vector
#' @param ... arguments passed on to [base::mean()]
#' @param na.rm ignore empty values
#' @details The harmonic mean can be expressed as the reciprocal of the arithmetic mean of the reciprocals.
#' @rdname different_means
#' @name different_means
#' @export
mean_harmonic <- function(x, ..., na.rm = getOption("na.rm", FALSE)) {
  1 / mean(1 / x, ..., na.rm = na.rm)
}

#' @rdname different_means
#' @details The geometric mean is defined as the nth root of the product of n numbers.
#' @export
mean_geometric <- function(x, ..., na.rm = getOption("na.rm", FALSE)) {
  exp(mean(log(abs(x)), ..., na.rm = na.rm))
  # or prod(x) ^ (1 / length(x))
}
