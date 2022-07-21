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

#' Survival Analysis and Censored Regression
#'
#' Perform survival analysis using tidymodels
survival_exec <- function(.data,
                          days,
                          status,
                          training_fraction = 0.1,
                          engine,
                          FUN,
                          ...) {
  check_is_installed("survival")
  
  df <- .data |>
    transmute(days = {{ days }},
              status = {{ status }})
  rows_train <- sort(sample())
  df_train <- .data[rows_train, , drop = FALSE]
  
  ph_spec <- FUN(...) |>
    set_engine(engine) |>
    set_mode("censored regression")
  
  ph_spec |>
    fit(survival::Surv(days, status) ~ ., data = df_train)
}

