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

#' @importFrom dplyr everything
#' @export
dplyr::everything

#' @importFrom caret confusionMatrix
#' @export
caret::confusionMatrix

#' @importFrom yardstick metrics
#' @export
yardstick::metrics

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

check_is_installed <- function(pkgs) {
  to_install <- pkgs[which(!pkgs %in% rownames(utils::installed.packages(.libPaths())))]
  if (length(to_install) > 0) {
    if (interactive()) {
      # ask to install
      choice <- utils::askYesNo(paste0("Package(s) required but not installed: ",
                                       paste0("'", to_install, "'", collapse = ", "), ". ",
                                       "Install now?"))
    } else {
      choice <- FALSE
    }
    if (isTRUE(choice)) {
      utils::install.packages(to_install)
      # try again:
      check_is_installed(pkgs)
    } else {
      stop("Required package(s) ",
           paste0("'", to_install, "'", collapse = ", "), 
           " not installed", call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}

globalVariables(c(".",
                  ".id",
                  ".estimator",
                  ".level",
                  ".metric",
                  ".pred_class",
                  "certainty",
                  "model",
                  "name",
                  "neg_pred_value",
                  "other",
                  "outcome",
                  "pos_pred_value",
                  "predicted",
                  "sensitivity",
                  "specificity",
                  "std_err",
                  "t_diff",
                  "truth",
                  "value",
                  "where",
                  "y"))

#' @importFrom dplyr mutate starts_with
#' @importFrom yardstick roc_auc
get_auc <- function(df, look_for) {
  df |>
    mutate(predicted = factor(ifelse(predicted == look_for, look_for, "other"),
                              levels = c(look_for, "other")),
           truth = factor(ifelse(truth == look_for, look_for, "other"),
                          levels = c(look_for, "other")),
           other = row_function(sum, starts_with(".pred")) - df[, paste0(".pred_", look_for), drop = TRUE]) |>
    roc_auc(truth, c(paste0(".pred_", look_for), other),
            estimator = "hand_till")
}
