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

#' Transform Vector to Percentiles
#' 
#' These functions take a numeric vector as input, and return the lowest [percentiles](https://en.wikipedia.org/wiki/Percentile) or [deciles](https://en.wikipedia.org/wiki/Decile) for each value.
#' @param x numeric vector
#' @rdname percentiles
#' @export
#' @examples 
#' x <- c(0, 1, 2, 3, 4, 5, 5, 5, 5, 5, 5, 5, 6)
#' percentiles(x)
#' deciles(x)
#' 
#' percentiles(rnorm(10))
#' 
#' library(dplyr, warn.conflicts = FALSE)
#' tib <- as_tibble(matrix(as.integer(runif(40, min = 1, max = 7)), ncol = 4),
#'                  .name_repair = function(...) LETTERS[1:4])
#' tib
#' 
#' # percentiles per column
#' tib |> mutate_all(percentiles)
percentiles <- function(x) {
  ntiles(x, resolution = 0.01)
}

#' @rdname percentiles
#' @export
deciles <- function(x) {
  ntiles(x, resolution = 0.1)
}

ntiles <- function(x, resolution) {
  # get quantile range, remove first entry (0th pct)
  q <- quantile(x, seq(0, 1, resolution))[-1]
  # get index of nearest ntile for each value
  vapply(FUN.VALUE = double(1), x, function(y) which.min(abs(q - y)))
}
