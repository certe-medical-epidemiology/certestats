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

#' @importFrom dplyr `%>%`
#' @export
dplyr::`%>%`

#' @importFrom caret confusionMatrix
#' @export
caret::confusionMatrix

#' @importFrom yardstick metrics
#' @export
yardstick::metrics

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

globalVariables(c(".",
                  ".level",
                  ".pred_class",
                  "model",
                  "name",
                  "neg_pred_value",
                  "pos_pred_value",
                  "predicted",
                  "sensitivity",
                  "specificity",
                  "t_diff",
                  "value",
                  "where"))
