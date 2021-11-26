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
  exp(mean(log(x), ..., na.rm = na.rm))
  # or prod(x) ^ (1 / length(x))
}

#' @export
any <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::any(..., na.rm = na.rm)
}

#' @export
all <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::all(..., na.rm = na.rm)
}

#' @export
mean <- function(x, ..., na.rm = getOption("na.rm", FALSE)) {
  base::mean(x, ..., na.rm = na.rm)
}

#' @export
sum <- function(..., na.rm = getOption("na.rm", FALSE)) {
  x <- base::sum(..., na.rm = na.rm)
}

#' @export
prod <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::prod(..., na.rm = na.rm)
}

#' @export
min <- function(..., na.rm = getOption("na.rm", FALSE)) {
  x <- base::min(..., na.rm = na.rm)
}

#' @export
max <- function(..., na.rm = getOption("na.rm", FALSE)) {
  x <- base::max(..., na.rm = na.rm)
}

#' @export
pmin <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::pmin(..., na.rm = na.rm)
}

#' @export
pmax <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::pmax(..., na.rm = na.rm)
}

#' @export
range <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::range(..., na.rm = na.rm)
}

#' @export
sd <- function(x, na.rm = getOption("na.rm", FALSE)) {
  stats::sd(x, na.rm = na.rm)
}

#' @export
var <- function(x, ..., na.rm = getOption("na.rm", FALSE)) {
  stats::var(x, na.rm = na.rm, ...)
}

#' @export
median <- function(x, na.rm = getOption("na.rm", FALSE), ...) {
  stats::median(x, na.rm = na.rm, ...)
}

#' @export
fivenum <- function(x, na.rm = getOption("na.rm", FALSE)) {
  stats::fivenum(x, na.rm = na.rm)
}

#' @export
quantile <- function(x, ..., na.rm = getOption("na.rm", FALSE), type = getOption("quantile.type", 7)) {
  stats::quantile(x, ..., na.rm = na.rm, type = type)
}

#' @export
IQR <- function(x, ..., na.rm = getOption("na.rm", FALSE), type = getOption("quantile.type", 7)) {
  stats::IQR(x, ..., na.rm = na.rm, type = type)
}
