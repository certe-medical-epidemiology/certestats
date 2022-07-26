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

# JULY 2022 - THIS IS WORK IN PROGRESS

#' Survival Analysis and Censored Regression
#'
#' Perform survival analysis using tidymodels
#' @importFrom dplyr transmute
#' @importFrom parsnip set_engine set_mode
survival_exec <- function(.data,
                          days,
                          status,
                          predictors = everything(),
                          training_fraction = 0.9,
                          time = c("6 months", "2 years", "3 years"),
                          engine,
                          FUN,
                          ...) {
  check_is_installed(c("censored", "survival"))
  # support mode 'censored regression' for different engines:
  loadNamespace("censored")
  
  df <- .data |>
    select(days = {{ days }},
           status = {{ status }},
           {{ predictors }})
  
  if (training_fraction > 1) {
    # per the documentation, this is the number of rows for training, so:
    training_fraction <- training_fraction / nrow(df)
  }
  if (training_fraction >= 1) {
    # in case of a higher training_fraction than nrow(df), or if training_fraction = 1
    warning("Training size set to ", round(training_fraction * nrow(df)),
            ", while data size is ", nrow(df),
            " - training size has been set to ", nrow(df) - 1, call. = FALSE)
    training_fraction <- (nrow(df) - 1) / nrow(df)
  }
  
  rows_train <- sort(sample(seq_len(nrow(df)),
                            size = round(training_fraction * nrow(df)),
                            replace = FALSE))
  df_train <- df[rows_train, , drop = FALSE]
  df_test <- df[-rows_train, , drop = FALSE]
  
  mdl_recipe <- FUN(...) |>
    set_engine(engine) |>
    set_mode("censored regression")
  
  fit <- mdl_recipe |>
    fit(survival::Surv(days, status) ~ ., data = df_train)
  
  pred_survival <- fit |> 
    stats::predict(df_test, 
                   type = "survival",
                   time = text_in_days(time))
  pred_time <- fit |> 
    stats::predict(df_test,
                   type = "time")
  
  structure(fit,
            class = c("certestats_survival", class(fit)),
            data_training = df_train,
            data_testing = df_test,
            recipe = mdl_recipe,
            predicton_survival = pred_survival,
            prediction_time = pred_time)
}

text_in_days <- function(x) {
  out <- rep(NA_real_, times = length(x))
  # remove spaces
  x <- gsub(" +", "", x)
  # convert times
  out[x %like% "year"] <- as.double(gsub("years?", "", x[x %like% "year"])) * 365
  out[x %like% "month"] <- round((as.double(gsub("months?", "", x[x %like% "month"])) / 12) * 365)
  out[x %like% "week"] <- as.double(gsub("weeks?", "", x[x %like% "week"])) * 7
  out[x %like% "day"] <- as.double(gsub("days?", "", x[x %like% "day"]))
  out[is.na(out)] <- as.double(x[is.na(out)])
  out
}
