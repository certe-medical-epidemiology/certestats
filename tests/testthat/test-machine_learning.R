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
  
  model_decision_trees <- iris |> ml_decision_trees(Species, where(is.double))
  model_linear_regression <- iris |> ml_linear_regression(Sepal.Length, where(is.double))
  model_logistic_regression <- suppressWarnings(iris |> ml_logistic_regression(Species, where(is.double)))
  model_neural_network <- iris |> ml_neural_network(Species, where(is.double))
  model_nearest_neighbour <- iris |> ml_nearest_neighbour(Species, where(is.double))
  model_random_forest <- iris |> ml_random_forest(Species, where(is.double))
  
  expect_s3_class(model_decision_trees, "certestats_ml")
  expect_s3_class(model_linear_regression, "certestats_ml")
  expect_s3_class(model_logistic_regression, "certestats_ml")
  expect_s3_class(model_neural_network, "certestats_ml")
  expect_s3_class(model_nearest_neighbour, "certestats_ml")
  expect_s3_class(model_random_forest, "certestats_ml")
  
  expect_output(print(model_decision_trees))
  expect_identical(get_recipe(model_decision_trees),
                   attributes(model_decision_trees)$recipe)
  
  expect_s3_class(autoplot(model_decision_trees, plot_type = "roc"), "gg")
  expect_s3_class(autoplot(model_decision_trees, plot_type = "gain"), "gg")
  expect_s3_class(autoplot(model_decision_trees, plot_type = "lift"), "gg")
  expect_s3_class(autoplot(model_decision_trees, plot_type = "pr"), "gg")
  
  expect_s3_class(confusion_matrix(model_decision_trees), "certestats_confusion_matrix")
  
  # model variables
  expect_identical(get_model_variables(model_decision_trees),
                   iris[0, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
  
  expect_true(is.data.frame(get_metrics(model_decision_trees)))
  expect_true(all(c("predicted", ".pred_setosa", ".pred_versicolor", ".pred_virginica") %in% colnames(apply_model_to(model_decision_trees, iris))))
  expect_true(model_decision_trees |> apply_model_to(iris, only_prediction = TRUE) |> is.factor())
  expect_warning(iris |> ml_decision_trees(Species, where(is.double), training_fraction = 10000))
  expect_error(iris |> ml_decision_trees(as.character(Species), where(is.double)))
  
  # missing values
  mdl <- iris |> ml_random_forest(Species)
  new <- iris |>
    select(matches("[.]")) |>
    slice(1:3) |>
    select(-Petal.Width) |> 
    mutate(Sepal.Width = as.integer(Sepal.Width))
  expect_message(mdl |> apply_model_to(new))
  
  # get weights
  weights <- model_random_forest |> get_variable_weights()
  expect_true(is.numeric(unlist(weights)))
  expect_equal(length(weights), # should be same as trained data without the dependent variable
               ncol(attributes(model_random_forest)$data_original) - 1)
  
  # stratified sampling using strata
  stratified <- esbl_tests |> ml_random_forest(esbl, where(is.double), strata = genus)
  expect_true(is.table(attributes(stratified)$properties$strata))
  # should return warnings for NAs and strata of size 1
  e <- esbl_tests
  e[1, "genus"] <- NA_character_
  expect_warning(e |> ml_random_forest(esbl, where(is.double), strata = genus))
  rows_to_remove <- which(esbl_tests$genus == esbl_tests$genus[1])
  rows_to_remove <- rows_to_remove[c(2:length(rows_to_remove))]
  expect_warning(esbl_tests |> slice(-rows_to_remove) |> ml_random_forest(esbl, where(is.double), strata = genus))
  
  # tuning parameters
  expect_error(tune_parameters("test"))
  tuned_decision_trees <- model_decision_trees |> tune_parameters(levels = 1, v = 2)
  tuned_linear_regression <- model_linear_regression |> tune_parameters(levels = 1, v = 2)
  tuned_logistic_regression <- model_logistic_regression |> tune_parameters(levels = 1, v = 2)
  tuned_neural_network <- model_neural_network |> tune_parameters(levels = 1, v = 2)
  tuned_nearest_neighbour <- model_nearest_neighbour |> tune_parameters(levels = 1, v = 2)
  tuned_random_forest <- model_random_forest |> tune_parameters(levels = 1, v = 2)
  
  tuned2 <- model_neural_network |> tune_parameters(epochs = dials::epochs(), levels = 1, v = 2)
  expect_error(model_neural_network |> tune_parameters(dials::epochs()))
  # try to run on any of our ML functions
  expect_true(tuned_decision_trees |> is.data.frame())
  expect_null(tuned_linear_regression)
  expect_null(tuned_logistic_regression)
  expect_true(tuned_neural_network |> is.data.frame())
  expect_true(tuned_nearest_neighbour |> is.data.frame())
  expect_true(tuned_random_forest |> is.data.frame())
  expect_s3_class(autoplot(tuned_decision_trees), "gg")
  
  # imputation
  model1 <- esbl_tests |> ml_random_forest(esbl, where(is.double))
  esbl_tests2 <- esbl_tests
  esbl_tests2[c(1, 3, 5), "AMC"] <- NA
  expect_message(model1 |> apply_model_to(esbl_tests2))
  expect_message(model1 |> apply_model_to(esbl_tests2, impute_algorithm = "single"))
  expect_error(model1 |> apply_model_to(esbl_tests2, impute_algorithm = FALSE))
  
  # internals
  var_info <- get_recipe(model1)$var_info
  expect_true(all(c("variable", "role") %in% colnames(var_info)))
  expect_true(all(var_info$variable[var_info$variable != "outcome"] %in% colnames(attributes(model1)$data_original)))
})
