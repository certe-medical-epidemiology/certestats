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

#' Mathematical Functions With Global `na.rm`
#' 
#' These functions call their original base \R namesake, but with a global settable `na.rm` argument.
#' @section Default values of `na.rm`:
#' This 'certestats' package supports a global default setting for `na.rm` in many mathematical functions. This can be set with `options(na.rm = TRUE)` or `options(na.rm = FALSE)`.
#' 
#' For [normality()], [quantile()] and [IQR()], this also applies to the `type` argument. The default, `type = 7`, is the default of base \R. Use `type = 6` to comply with SPSS.
#' @rdname math_functions
#' @name math_functions
#' @inheritParams base::any
#' @source [base::any()]
#' @export
any <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::any(..., na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams base::all
#' @source [base::all()]
#' @export
all <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::all(..., na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams base::mean
#' @source [base::mean()]
#' @export
mean <- function(x, ..., na.rm = getOption("na.rm", FALSE)) {
  base::mean(x, ..., na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams base::sum
#' @source [base::sum()]
#' @export
sum <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::sum(..., na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams base::prod
#' @source [base::prod()]
#' @export
prod <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::prod(..., na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams base::min
#' @source [base::min()]
#' @export
min <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::min(..., na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams base::max
#' @source [base::max()]
#' @export
max <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::max(..., na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams base::pmin
#' @source [base::pmin()]
#' @export
pmin <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::pmin(..., na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams base::pmax
#' @source [base::pmax()]
#' @export
pmax <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::pmax(..., na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams base::range
#' @source [base::range()]
#' @export
range <- function(..., na.rm = getOption("na.rm", FALSE)) {
  base::range(..., na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams stats::sd
#' @source [stats::sd()]
#' @export
sd <- function(x, na.rm = getOption("na.rm", FALSE)) {
  stats::sd(x, na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams stats::var
#' @source [stats::var()]
#' @export
var <- function(x, ..., na.rm = getOption("na.rm", FALSE)) {
  stats::var(x, na.rm = na.rm, ...)
}

#' @rdname math_functions
#' @inheritParams stats::median
#' @source [stats::median()]
#' @export
median <- function(x, na.rm = getOption("na.rm", FALSE), ...) {
  stats::median(x, na.rm = na.rm, ...)
}

#' @rdname math_functions
#' @inheritParams stats::fivenum
#' @source [stats::fivenum()]
#' @export
fivenum <- function(x, na.rm = getOption("na.rm", FALSE)) {
  stats::fivenum(x, na.rm = na.rm)
}

#' @rdname math_functions
#' @inheritParams stats::quantile
#' @source [stats::quantile()]
#' @export
quantile <- function(x, ..., na.rm = getOption("na.rm", FALSE), type = getOption("quantile.type", 7)) {
  stats::quantile(x, ..., na.rm = na.rm, type = type)
}

#' @rdname math_functions
#' @inheritParams stats::IQR
#' @source [stats::IQR()]
#' @export
IQR <- function(x, ..., na.rm = getOption("na.rm", FALSE), type = getOption("quantile.type", 7)) {
  stats::IQR(x, ..., na.rm = na.rm, type = type)
}

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

#' Apply Function per Row
#' 
#' This can be used to e.g. add a maximum of certain rows.
#' @param fn function to apply, such as [max()]
#' @param ... tidyverse selector helpers, passed on to [`select()`][dplyr::select()]
#' @param data data set, will be determined with [`pick()`][dplyr::pick()] if left blank
#' @importFrom dplyr select pick everything
#' @export
#' @examples
#' if (require("dplyr")) {
#'   iris |> 
#'     mutate(max = row_function(max, where(is.numeric)),
#'            sepal_mean = row_function(mean, starts_with("Sepal"))) |> 
#'     head()
#' }
row_function <- function(fn, ..., data = NULL) {
  if (is.null(data)) {
    data <- pick(everything())
  } else if (!is.data.frame(data)) {
    stop("'data' must be a data.frame", call. = FALSE)
  }
  if (tryCatch(length(list(...)) > 0, error = function(e) TRUE)) {
    data <- data |> select(...)
  } 
  apply(data, 1, fn)
}
