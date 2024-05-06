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
