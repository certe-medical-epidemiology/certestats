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

#' Moving Average
#'
#' Functions to calculate a moving average. These are useful to get rid of (strong) peakiness.
#' @param x vector of values
#' @param w window length; total number of observations to include. This should preferably be an odd number, so that the same number of values to the left and right of \code{x} are included.
#' @param fun function to apply
#' @param side default is `"centre"`, can also be `"left"` or `"right"`. This can be used to take a moving average (or sum, or ...) of e.g. the last 7 days.
#' @param na.rm a [logical] to indicate whether empty must be removed from `x`
#' @param ... arguments passed on to `fun`
#' @details Each function can be used over a moving period with [moving_fn()]. For example, for a moving median: `moving_fn(x, 7, fun = median)`. Or a moving maximum: `moving_fn(x, 5, fun = max)`.
#' 
#' The moving average is determined by averaging `floor(w / 2)` values before and after each element of `x` and all elements in between.
#' @inheritSection math_functions Default values of `na.rm`
#' @rdname moving_average
#' @export
moving_average <- function(x, w, side = "centre", na.rm = getOption("na.rm", FALSE)) {
  moving_fn(x = x, w = w, fun = mean, side = side, na.rm = na.rm)
}

#' @rdname moving_average
#' @export
moving_sum <- function(x, w, side = "centre", na.rm = getOption("na.rm", FALSE)) {
  moving_fn(x = x, w = w, fun = sum, side = side, na.rm = na.rm)
}

#' @rdname moving_average
#' @export
moving_Q1 <- function(x, w, side = "centre", na.rm = getOption("na.rm", FALSE)) {
  unname(moving_fn(x = x, w = w, fun = quantile, side = side, na.rm = na.rm, 0.25))
}

#' @rdname moving_average
#' @export
moving_Q3 <- function(x, w, side = "centre", na.rm = getOption("na.rm", FALSE)) {
  unname(moving_fn(x = x, w = w, fun = quantile, side = side, na.rm = na.rm, 0.75))
}

#' @rdname moving_average
#' @export
moving_fn <- function(x, w, fun, side = "centre", ...) {
  stopifnot(is.numeric(x), is.numeric(w), is.function(fun))
  if (length(w) != 1 || ceiling(w != floor(w)) || w <= 1) {
    stop("'w' must be a single integer greater 1.")
  }
  if (w >= length(x)) {
    stop("'x' must be greater in length than the value of 'w'.")
  }
  
  y <- numeric(length(x))
  for (i in seq_len(length(x))) {
    if (side %in% c("c", "centre", "center")) {
      ind <- c((i - floor(w / 2)):(i + floor(w / 2)))
    } else if (side %in% c("l", "left")) {
      ind <- c((i - floor(w) + 1):i)
    } else if (side %in% c("r", "right")) {
      ind <- c(i:(i + floor(w) - 1))
    } else {
      stop("'side' must be one of 'centre', 'left', 'right'", call. = FALSE)
    }
    ind <- ind[ind %in% seq_len(length(x))]
    y[i] <- fun(x[ind], ...)
  }
  y
}
