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

#' Work with Binary Columns
#' 
#' Transform [logical]s and [numeric]s to a new class 'binary'. The function [try_binary()] only coerces to `binary` if the input is numeric and consists of only zeroes and ones.
#' @param x value(s) to convert to `binary`
#' @rdname binary
#' @export
#' @examples
#' as.binary(c(TRUE, FALSE))
#' 
#' as.binary(c(1, 0))
#' as.binary(c(1, 0)) + 3 # not binary anymore
#' 
#' try_binary(c(0, 1))
#' try_binary(c(2, 3))
#' try_binary(c("a", "b"))
#' 
as.binary <- function(x) {
  structure(as.integer(x), class = "binary")
}

#' @rdname binary
#' @export
is.binary <- function(x) {
  inherits(x, "binary")
}

#' @rdname binary
#' @export
try_binary <- function(x) {
  if (is.logical(x) || (is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE))) {
    as.binary(x)
  } else {
    x
  }
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.binary <- function (x, ...) {
  "bin"
}
#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.binary <- function (x, ...) {
  "binary"
}

#' @export
print.binary <- function(x, ...) {
  cat("<binary>\n")
  print(as.integer(x), ...)
}

#' @export
Math.binary <- function(x, ...) {
  x <- as.double(x)
  .Class <- class(x)
  NextMethod(.Generic)
}

#' @export
Ops.binary <- function(e1, e2) {
  e1 <- as.double(e1)
  if (!missing(e2)) {
    # when .Generic is `!`, e2 is missing
    e2 <- as.double(e2)
  }
  .Class <- class(e1)
  NextMethod(.Generic)
}

#' @export
Summary.binary <- function(..., na.rm = FALSE) {
  # NextMethod() cannot be called from an anonymous function (`...`), so we get() the generic directly:
  fn <- get(.Generic, envir = .GenericCallEnv)
  fn(as.double(c(...)),
     na.rm = na.rm)
}
