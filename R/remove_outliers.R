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

#' Remove Outliers
#' 
#' This simple function uses [boxplot.stats()] to determine outliers and removes them from the vector.
#' @param x a vector of values
#' @param coef a multiple of the IQR that is allowed at maximum to keep values within the accepted range
#' @export
#' @examples
#' remove_outliers(c(1,2,1,2,1,2,8))
#' 
#' remove_outliers(c(1,2,1,2,1,2))
remove_outliers <- function(x, coef = 1.5) {
  out <- grDevices::boxplot.stats(x)$out
  message(length(out), " outlier", ifelse(length(out) != 1, "s", ""), " removed")
  x[!x %in% out]
}

