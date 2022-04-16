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

test_that("ML works", {
  library(dplyr)
  
  model_decision_trees <- iris %>% ml_decision_trees(Species, where(is.double))
  model_linear_regression <- iris %>% ml_linear_regression(Sepal.Length, where(is.double))
  model_logistic_regression <- suppressWarnings(iris %>% ml_logistic_regression(Species, where(is.double)))
  model_neural_network <- iris %>% ml_neural_network(Species, where(is.double))
  model_nearest_neighbour <- iris %>% ml_nearest_neighbour(Species, where(is.double))
  model_random_forest <- iris %>% ml_random_forest(Species, where(is.double))
  
  expect_s3_class(model_decision_trees, "certestats_ml")
  expect_s3_class(model_linear_regression, "certestats_ml")
  expect_s3_class(model_logistic_regression, "certestats_ml")
  expect_s3_class(model_neural_network, "certestats_ml")
  expect_s3_class(model_nearest_neighbour, "certestats_ml")
  expect_s3_class(model_random_forest, "certestats_ml")
  
  expect_output(print(model_decision_trees))
  
  expect_s3_class(autoplot(model_decision_trees, plot_type = "roc"), "gg")
  expect_s3_class(autoplot(model_decision_trees, plot_type = "gain"), "gg")
  expect_s3_class(autoplot(model_decision_trees, plot_type = "lift"), "gg")
  expect_s3_class(autoplot(model_decision_trees, plot_type = "pr"), "gg")
  expect_s3_class(confusionMatrix(model_decision_trees), "confusionMatrix")
  expect_true(is.data.frame(metrics(model_decision_trees)))
  expect_true(all(c("predicted", ".pred_setosa", ".pred_versicolor", ".pred_virginica") %in% colnames(apply_model_to(model_decision_trees, iris))))
  expect_true(is.factor(apply_model_to(model_decision_trees, iris, only_prediction = TRUE)))
  expect_warning(iris %>% ml_decision_trees(Species, where(is.double), training_fraction = 10000))
  expect_error(iris %>% ml_decision_trees(as.character(Species), where(is.double)))
  
  # tuning parameters
  tuned <- model_neural_network %>% tune_parameters(levels = 1, v = 2)
  tuned2 <- model_neural_network %>% tune_parameters(epochs = dials::epochs(), levels = 1, v = 2)
  expect_true(all(c(".metric", ".estimator", "mean", "n", "std_err") %in% colnames(tuned)))
  expect_error(model_neural_network %>% tune_parameters(dials::epochs()))
  # try to run on any of our ML functions
  expect_true(model_decision_trees %>% tune_parameters(levels = 1, v = 2) %>% is.data.frame())
  expect_true(model_linear_regression %>% tune_parameters(levels = 1, v = 2) %>% is.data.frame())
  # expect_true(model_logistic_regression %>% tune_parameters(levels = 1, v = 2) %>% is.data.frame())
  expect_true(model_neural_network %>% tune_parameters(levels = 1, v = 2) %>% is.data.frame())
  expect_true(model_nearest_neighbour %>% tune_parameters(levels = 1, v = 2) %>% is.data.frame())
  expect_true(model_random_forest %>% tune_parameters(levels = 1, v = 2) %>% is.data.frame())
})
