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

#' Statistical Tests
#' 
#' These tests are easy-to-understand wrappers around existing \R functions. Moreover, they are wrapped in [broom::tidy()] to return a [data.frame].
#' @param x,y a vector
#' @param .data a [data.frame]
#' @param ... other arguments to pass on to functions
#' @details [mann_whitney_u()] is equal to [`wilcox.test(x, y, paired = FALSE)`][stats::wilcox.test].
#' @importFrom broom tidy
#' @name statistical_tests
#' @rdname statistical_tests
NULL

#' @importFrom dplyr select
mann_whitney_u <- function(...) {
  params <- list(...)
  print(params)
  if (!all(c("x", "y") %in% names(params))) {
    stop("'x' and 'y' must be named")
  }
  if (is.data.frame(params[[1]])) {
    params[["x"]] <- params[[1]] |> select({{ params[["x"]] }})
    params[["y"]] <- params[[1]] |> select({{ params[["y"]] }})
    params <- params[-1]
  }
  params[["paired"]] <- FALSE
  stats::wilcox.test |> 
    do.call(args = params) |> 
    tidy()
}

#' @rdname statistical_tests
#' @method mann_whitney_u default
#' @export
mann_whitney_u.default <- function(x, y, ...) {
  print(1)
  mann_whitney_u(x = x, y = y, ...)
}
#' @rdname statistical_tests
#' @method mann_whitney_u data.frame
#' @export
mann_whitney_u.data.frame <- function(.data, x, y, ...) {
  print(2)
  mann_whitney_u(.data = .data, x = x, y = y, ...)
}

#' @rdname statistical_tests
#' @export
wilcoxon_rank_sum <- mann_whitney_u


