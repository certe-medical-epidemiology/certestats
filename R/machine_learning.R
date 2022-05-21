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

#' Create a Traditional Machine Learning (ML) Model
#'
#' Create a traditional machine learning model based on different 'engines'. These function internally use the `tidymodels` packages by RStudio, which is the `tidyverse` variant for predictive modelling.
#' @param .data Data set to train
#' @param outcome Outcome variable to be used (the variable that must be predicted). The value will be evaluated in [`select()`][dplyr::select()] and thus supports the `tidyselect` language. In case of classification prediction, this variable will be coerced to a [factor].
#' @param predictors Variables to use as predictors - these will be transformed using [as.double()] ([factor]s will be transformed to [character]s first). This value defaults to [`everything()`][tidyselect::everything()] and supports the `tidyselect` language.
#' @param training_fraction Fraction of rows to be used for *training*, defaults to 75%. The rest will be used for *testing*. If given a number over 1, the number will be considered to be the required number of rows for *training*.
#' @param strata Groups to consider in the model (i.e., variables to stratify by)
#' @param correlation_filter A [logical] to indicate whether the `predictors` should be removed that have to much correlation with each other, using [recipes::step_corr()]
#' @param centre A [logical] to indicate whether the `predictors` should be transformed so that their mean will be `0`, using [recipes::step_center()]
#' @param scale A [logical] to indicate whether the `predictors` should be transformed so that their standard deviation will be `1`, using [recipes::step_scale()]
#' @param max_na_fraction Maximum fraction of `NA` values (defaults to `0.01`) of the `predictors` before they are removed from the model
#' @param mode Type of predicted value - defaults to `"classification"`, but can also be `"unknown"` or `"regression"`
#' @param engine \R package or function name to be used for the model, will be passed on to [parsnip::set_engine()]
#' @param ... Arguments to be passed on to the `parsnip` functions, see *Model Functions*. For the [tune_parameters()] function, these must be `dials` package calls, such as `dials::trees()` (see Examples).
#' @inheritParams parsnip::decision_tree
#' @inheritParams parsnip::linear_reg
#' @inheritParams parsnip::logistic_reg
#' @inheritParams parsnip::mlp
#' @inheritParams parsnip::nearest_neighbor
#' @inheritParams parsnip::rand_forest
#' @details
#' To predict **regression** (numeric values), the function [ml_logistic_regression()] cannot be used.
#'
#' To predict **classifications** (character values), the function [ml_linear_regression()] cannot be used.
#' 
#' The workflow of the `ml_*()` functions is basically like this (thus saving a lot of `tidymodels` functions to type):
#' 
#' \preformatted{
#'                        .data
#'                          |
#'                rsample::initial_split()
#'                      /        \
#'      rsample::training() rsample::testing()
#'              |                |
#'        recipe::recipe()       |
#'              |                |
#'       recipe::step_corr()     |
#'              |                |
#'      recipe::step_center()    |
#'              |                |
#'       recipe::step_scale()    |
#'              |                |
#'         recipe::prep()        |
#'          /           \        |
#' recipes::bake()       recipes::bake()
#'        |                      |
#' generics::fit()      yardstick::metrics()
#'        |                      |
#'     output            attributes(output)
#' }
#' @return A machine learning model of class `certestats_ml` / `_rpart` / `model_fit`.
#' 
#' ## Attributes
#' 
#' The `ml_*()` functions return the following [attributes][base::attributes()]:
#' 
#' * `properties`: a [list] with model properties: the ML function, engine package, training size, testing size, strata size, mode, and the different ML function-specific properties (such as `tree_depth` in [ml_decision_trees()])
#' * `recipe`: a [recipe][recipes::recipe()] as generated with [recipes::prep()], to be used for training and testing
#' * `data_structure`: a [data.frame] containing the original data structure with zero rows
#' * `data_training`: a [data.frame] containing the training data
#' * `data_testing`: a [data.frame] containing the testing data
#' * `rows_training`: an [integer] vector of rows used for training
#' * `rows_testing`: an [integer] vector of rows used for training
#' * `predictions`: a [data.frame] containing predicted values based on the testing data
#' * `metrics`: a [data.frame] with model metrics as returned by [yardstick::metrics()]
#' * `correlation_filter`: a [logical] indicating whether [recipes::step_corr()] has been applied
#' * `centre`: a [logical] indicating whether [recipes::step_center()] has been applied
#' * `scale`: a [logical] indicating whether [recipes::step_scale()] has been applied
#' 
#' @section Model Functions:
#' These are the called functions from the `parsnip` package. Arguments set in `...` will be passed on to these `parsnip` functions:
#' 
#'  * `ml_decision_trees`: [parsnip::decision_tree()]
#'  * `ml_linear_regression`: [parsnip::linear_reg()]
#'  * `ml_logistic_regression`: [parsnip::logistic_reg()]
#'  * `ml_neural_network`: [parsnip::mlp()]
#'  * `ml_nearest_neighbour`: [parsnip::nearest_neighbor()]
#'  * `ml_random_forest`: [parsnip::rand_forest()]
#' @name machine_learning
#' @rdname machine_learning
#' @export
#' @examples
#' # 'esbl_tests' is an included data set, see ?esbl_tests
#' print(esbl_tests, n = 5)
#' 
#' # predict ESBL test outcome based on MICs
#' model1 <- esbl_tests |> ml_random_forest(esbl, where(is.double))
#' model2 <- esbl_tests |> ml_decision_trees(esbl, where(is.double))
#' 
#' model1 |> metrics()
#' model2 |> metrics()
#' 
#' model1 |> confusionMatrix()
#' 
#' 
#' ## Applying A Model ##
#'  
#' # do NOT apply a model using 'just' stats::predict():
#' model1 |> predict(esbl_tests, type = "prob")
#' 
#' # the recipe was not considered, and you must bake first:
#  recipe <- get_recipe(model1)
#  recipe
#' model1 |> predict(recipes::bake(recipe, new_data = esbl_tests), type = "prob")
#'
#' # as a shortcut, apply a model using apply_model_to() to get predictions and certainties
#' model1 |> apply_model_to(esbl_tests)
#' # apply_model_to() can also correct for missing variables:
#' model1 |> apply_model_to(esbl_tests[, 1:17])
#' 
#' 
#' ## Tuning A Model ##
#'  
#' # tune the parameters of a model (will take some time)
#' tuning <- model1 |> 
#'   tune_parameters(v = 5, levels = 3)
#' autoplot(tuning)
#' 
#' # tuning analysis by specifying (some) parameters
#' iris |> 
#'   ml_random_forest(Species) |> 
#'   tune_parameters(mtry = dials::mtry(range = c(1, 3)),
#'                   trees = dials::trees())
#' 
#' 
#' ## Practical Example 1 ##
#' 
#' # this is what iris data set looks like:
#' head(iris)
#' # create a model to predict the species:
#' iris_model <- iris |> ml_random_forest(Species)
#' # is it a bit reliable?
#' metrics(iris_model)
#' 
#' # now try to predict species from an arbitrary data set:
#' to_predict <- data.frame(Sepal.Length = 5,
#'                          Sepal.Width = 3,
#'                          Petal.Length = 1.5,
#'                          Petal.Width = 0.5)
#' to_predict
#' 
#' # should be 'setosa' in the 'predicted' column:
#' iris_model |> apply_model_to(to_predict)
#' 
#' # how would the model do without the most important 'Sepal.Length' column?
#' to_predict <- to_predict[, c("Sepal.Width", "Petal.Length", "Petal.Width")]
#' to_predict
#' iris_model |> apply_model_to(to_predict)
#' 
#' 
#' ## Practical Example 2 ##
#' 
#' # this example shows plotting methods for a model
#' 
#' # train model to predict genus based on MICs:
#' genus <- esbl_tests |> ml_neural_network(genus, everything())
#' genus |> metrics()
#' genus |> autoplot()
#' genus |> autoplot(plot_type = "gain") 
ml_decision_trees <- function(.data,
                              outcome,
                              predictors = everything(),
                              training_fraction = 3/4,
                              strata = NULL,
                              max_na_fraction = 0.01,
                              correlation_filter = TRUE,
                              centre = TRUE,
                              scale = TRUE,
                              engine = "rpart",
                              mode = c("classification", "regression", "unknown"),
                              tree_depth = 10,
                              ...) {
  ml_exec(FUN = parsnip::decision_tree,
          .data = .data,
          outcome = {{outcome}},
          predictors = {{predictors}},
          training_fraction = training_fraction,
          strata = {{strata}},
          max_na_fraction = max_na_fraction,
          correlation_filter = correlation_filter,
          centre = centre,
          scale = scale,
          engine = engine,
          mode = mode[1L],
          tree_depth = tree_depth,
          ...)
}

#' @rdname machine_learning
#' @export
ml_linear_regression <- function(.data,
                                 outcome,
                                 predictors = everything(),
                                 training_fraction = 3/4,
                                 strata = NULL,
                                 max_na_fraction = 0.01,
                                 correlation_filter = TRUE,
                                 centre = TRUE,
                                 scale = TRUE,
                                 engine = "lm",
                                 mode = "regression",
                                 ...) {
  ml_exec(FUN = parsnip::linear_reg,
          .data = .data,
          outcome = {{outcome}},
          predictors = {{predictors}},
          training_fraction = training_fraction,
          strata = {{strata}},
          max_na_fraction = max_na_fraction,
          correlation_filter = correlation_filter,
          centre = centre,
          scale = scale,
          engine = engine,
          mode = mode,
          ...)
}

#' @rdname machine_learning
#' @export
ml_logistic_regression <- function(.data,
                                   outcome,
                                   predictors = everything(),
                                   training_fraction = 3/4,
                                   strata = NULL,
                                   max_na_fraction = 0.01,
                                   correlation_filter = TRUE,
                                   centre = TRUE,
                                   scale = TRUE,
                                   engine = "glm",
                                   mode = "classification",
                                   penalty = 0.1,
                                   ...) {
  ml_exec(FUN = parsnip::logistic_reg,
          .data = .data,
          outcome = {{outcome}},
          predictors = {{predictors}},
          training_fraction = training_fraction,
          strata = {{strata}},
          max_na_fraction = max_na_fraction,
          correlation_filter = correlation_filter,
          centre = centre,
          scale = scale,
          engine = engine,
          mode = mode,
          penalty = penalty,
          ...)
}

#' @rdname machine_learning
#' @export
ml_neural_network <- function(.data,
                              outcome,
                              predictors = everything(),
                              training_fraction = 3/4,
                              strata = NULL,
                              max_na_fraction = 0.01,
                              correlation_filter = TRUE,
                              centre = TRUE,
                              scale = TRUE,
                              engine = "nnet",
                              mode = c("classification", "regression", "unknown"),
                              penalty = 0,
                              epochs = 100,
                              ...) {
  ml_exec(FUN = parsnip::mlp,
          .data = .data,
          outcome = {{outcome}},
          predictors = {{predictors}},
          training_fraction = training_fraction,
          strata = {{strata}},
          max_na_fraction = max_na_fraction,
          correlation_filter = correlation_filter,
          centre = centre,
          scale = scale,
          engine = engine,
          mode = mode[1L],
          penalty = penalty,
          epochs = epochs,
          ...)
}

#' @rdname machine_learning
#' @export
ml_nearest_neighbour <- function(.data,
                                 outcome,
                                 predictors = everything(),
                                 training_fraction = 3/4,
                                 strata = NULL,
                                 max_na_fraction = 0.01,
                                 correlation_filter = TRUE,
                                 centre = TRUE,
                                 scale = TRUE,
                                 engine = "kknn",
                                 mode = c("classification", "regression", "unknown"),
                                 neighbors = 5,
                                 weight_func = "triangular",
                                 ...) {
  ml_exec(FUN = parsnip::nearest_neighbor,
          .data = .data,
          outcome = {{outcome}},
          predictors = {{predictors}},
          training_fraction = training_fraction,
          strata = {{strata}},
          max_na_fraction = max_na_fraction,
          correlation_filter = correlation_filter,
          centre = centre,
          scale = scale,
          engine = engine,
          mode = mode[1L],
          neighbors = neighbors,
          weight_func = weight_func,
          ...)
}

#' @rdname machine_learning
#' @export
ml_random_forest <- function(.data,
                             outcome,
                             predictors = everything(),
                             training_fraction = 3/4,
                             strata = NULL,
                             max_na_fraction = 0.01,
                             correlation_filter = TRUE,
                             centre = TRUE,
                             scale = TRUE,
                             engine = "ranger",
                             mode = c("classification", "regression", "unknown"),
                             trees = 2000,
                             ...) {
  ml_exec(FUN = parsnip::rand_forest,
          .data = .data,
          outcome = {{outcome}},
          predictors = {{predictors}},
          training_fraction = training_fraction,
          strata = {{strata}},
          max_na_fraction = max_na_fraction,
          correlation_filter = correlation_filter,
          centre = centre,
          scale = scale,
          engine = engine,
          mode = mode[1L],
          trees = trees,
          ...)
}

#' @importFrom dplyr mutate select across filter_all bind_cols all_of cur_column slice
#' @importFrom yardstick metrics
#' @importFrom parsnip set_engine
#' @importFrom recipes recipe step_corr step_center step_scale all_predictors all_outcomes prep bake
#' @importFrom rsample initial_split training testing
#' @importFrom generics fit
ml_exec <- function(FUN,
                    .data,
                    outcome,
                    predictors,
                    training_fraction,
                    strata,
                    max_na_fraction,
                    correlation_filter,
                    centre,
                    scale,
                    engine,
                    ...) {
  
  n_pred <- tryCatch(ncol(select(.data, {{predictors}})), error = function(e) NULL)
  if (is.null(n_pred) || n_pred == 0) {
    stop("no columns found for argument 'predictors' (is argument 'predictors' missing?)", call. = FALSE)
  }
  
  # select only required data
  df <- .data |>
    select(outcome = {{ outcome }}, {{predictors}}, strata = {{strata}})
  
  # this will allow `predictors = everything()`, without selecting the outcome var with it
  predictors <- df |> 
    select(-c(outcome, strata)) |> 
    colnames()
  
  # save structure of original input data
  df_structure <- df |> 
    slice(0)
  
  # format data to work with it
  df <- df |>
    # force all predictors as double
    mutate(across(all_of(predictors),
                  function(values) {
                    if (is.character(values)) {
                      message("Transformed column '", cur_column(),
                              "' from <character> to <factor> and then to <double> to use as predictor")
                      values <- as.factor(values)
                    }
                    as.double(values)
                  })) |>
    # remove columns that do not comply to max_na_fraction
    select(where(function(x) sum(is.na(x)) / length(x) <= max_na_fraction)) |>
    # remove rows that have NA in outcome or predictors
    filter_all(function(x) !is.na(x))
  
  # the outcome variable must be factor in case of regression prediction
  if (list(...)$mode == "classification" && !is.factor(df$outcome)) {
    if (is.logical(df$outcome)) {
      df$outcome <- factor(df$outcome, levels = c(TRUE, FALSE))
    } else {
      if (is.numeric(df$outcome)) {
        warning("The outcome variable is numeric, should the mode not be 'regression' instead of 'classification'?", call. = FALSE)
      }
      df$outcome <- factor(df$outcome)
    }
  }
  
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
  
  suppressWarnings(
    properties <- c(list(ml_function = deparse(substitute(FUN)),
                         engine_package = engine,
                         data_size = nrow(df),
                         training_size = paste0(round(training_fraction * nrow(df)),
                                                " (fraction: ", round(training_fraction, 3), ")"),
                         testing_size = paste0(round((1 - training_fraction) * nrow(df)),
                                               " (fraction: ", round(1 - training_fraction, 3), ")"),
                         strata = paste(length(unique(df$strata)), "groups")),
                    list(...))
  )
  
  if (nrow(df) == 0) {
    stop("No more rows left for analysis (max_na_fraction = ", max_na_fraction, "). Check column values.", call. = FALSE)
  }
  
  if ("strata" %in% colnames(df)) {
    strata_var <- "strata"
  } else {
    strata_var <- NULL
  }
  
  df_split <- initial_split(df, strata = strata_var, prop = training_fraction)
  
  df_recipe <- df_split |>
    training() |>
    recipe(outcome ~ .)
  
  if (isTRUE(correlation_filter)) {
    df_recipe <- df_recipe |> step_corr(all_predictors(), -strata_var)
  }
  if (isTRUE(centre)) {
    df_recipe <- df_recipe |> step_center(all_predictors(), -all_outcomes(), -strata_var)
  }
  if (isTRUE(scale)) {
    df_recipe <- df_recipe |> step_scale(all_predictors(), -all_outcomes(), -strata_var)
  }
  df_recipe <- df_recipe |>
    prep()
  
  # train
  df_training <- df_recipe |>
    bake(new_data = NULL)
  
  # test
  df_testing <- df_recipe |>
    bake(testing(df_split))
  
  # create actual model
  mdl <- FUN(...) |>
    set_engine(engine) |>
    fit(outcome ~ ., data = df_training)
  
  # keep only training columns of original input data
  df_structure <- df_structure |> 
    select(all_of(colnames(df_training)), -outcome)
  
  metrics <- mdl |>
    stats::predict(df_testing) |>
    bind_cols(df_testing)
  metrics <- metrics |>
    metrics(truth = outcome, estimate = colnames(metrics)[1])
  
  if (properties$mode == "classification") {
    prediction <- stats::predict(mdl, df_testing, type = "prob")
    prediction <- prediction |>
      mutate(max = row_function(max, data = prediction))
  } else {
    prediction <- stats::predict(mdl, df_testing, type = "numeric")
  }
  pred_outcome <- stats::predict(mdl, df_testing)
  colnames(pred_outcome) <- "predicted"
  prediction <- bind_cols(prediction,
                          pred_outcome,
                          data.frame(truth = df_testing$outcome, stringsAsFactors = FALSE))
  
  structure(mdl,
            class = c("certestats_ml", class(mdl)),
            properties = properties,
            recipe = df_recipe,
            data_structure = df_structure,
            data_training = df_training,
            data_testing = df_testing,
            rows_training = sort(df_split$in_id),
            rows_testing = seq_len(nrow(df))[!seq_len(nrow(df)) %in% df_split$in_id],
            predictions = prediction,
            metrics = metrics,
            correlation_filter = correlation_filter,
            centre = centre,
            scale = scale)
}

#' @method print certestats_ml
#' @noRd
#' @export
print.certestats_ml <- function(x, ...) {
  model_prop <- attributes(x)
  cat("'certestats' Machine Learning Model\n\n",
      paste0(format(names(model_prop$properties)), " : ", model_prop$properties, "\n"),
      sep = "")
  if (model_prop$properties$mode == "classification") {
    cat("\nClassification certainty (based on testing data):\n")
    intervals <- c(0, 0.5, 0.68, 0.95, 0.98, 1)
    q <- quantile(model_prop$predictions$max, intervals)
    names(q) <- paste0("p", intervals * 100)
    print(q)
  }
  cat("\nMetrics:\n")
  print(metrics(x))
  cat(strrep("-", options()$width -2), "\n", sep = "")
  print(model_prop$recipe)
  cat(strrep("-", options()$width -2), "\n", sep = "")
  # print the rest like it used to be
  cat("Model Object:\n\n")
  class(x) <- class(x)[class(x) != "certestats_ml"]
  print(x)
}

#' @rdname machine_learning
#' @param object,data outcome of machine learning model
#' @param new_data new input data that requires prediction, must have all columns present in the training data
#' @param only_prediction a [logical] to indicate whether predictions must be returned as [vector], otherwise returns a [data.frame] with certainties of the predictions
#' @details The [apply_model_to()] function can be used to fit a model on a new data set, using [`predict()`][parsnip::predict.model_fit()]. If the new data misses variables that were in the training data, these variables will be generated with `NA`s if the model allows this (such as in [ml_decision_trees()]) and with the [mean][mean()] of the variable in the training data otherwise.
#' @importFrom dplyr select all_of mutate across everything pull type_sum cur_column
#' @importFrom recipes bake
#' @export
apply_model_to <- function(object, new_data, only_prediction = FALSE) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  # test - transform data according to recipe
  training_structure <- attributes(object)$data_structure
  training_data <- attributes(object)$data_training
  training_cols <- colnames(training_data)
  training_cols <- training_cols[training_cols != "outcome"]
  # correct for missing variables
  misses <- training_cols[!training_cols %in% colnames(new_data)]
  if (length(misses) > 0) {
    miss_df <- training_structure[, sort(misses), drop = FALSE]
    warning("These variables were used for training but are missing from the input data: ",
            paste0(colnames(miss_df), " <", miss_df |> vapply(FUN.VALUE = character(1), type_sum), ">", collapse = ", "),
            call. = FALSE)
  }
  for (col in misses) {
    # add as NA in the class of training_data:
    new_data[, col] <- training_structure[0, col, drop = TRUE][1]
  }
  # sort according to training data
  new_data <- new_data |> select(all_of(training_cols))
  # check classes of each model variable
  old_classes <- training_structure |> vapply(FUN.VALUE = character(1), type_sum)
  new_classes <- new_data |> vapply(FUN.VALUE = character(1), type_sum)
  class_diff <- which(old_classes != new_classes)
  if (length(class_diff) > 0) {
    warning("These variables are of different types than the training data of the model: ",
            paste0(names(old_classes[class_diff]), " (<",
                   old_classes[class_diff], "> in model, <",
                   new_classes[class_diff], "> in input)",
                   collapse = ", "),
            call. = FALSE)
  }
  # transform in the same way the model data was transformed
  new_data <- new_data |>
    mutate(across(everything(),
                  function(values) {
                    if (is.character(values)) {
                      values <- as.factor(values)
                    }
                    as.double(values)
                  }))
  test_data <- bake(get_recipe(object), new_data = new_data)
  
  predicted <- tryCatch(stats::predict(object = object, new_data = test_data, type = NULL), error = function(e) NULL)
  if (is.null(predicted)) {
    # replace NAs with mean of the training data, since this type of model apparently does not support NAs
    test_data <- test_data |> 
      mutate(across(everything(),
                    function(values) {
                      if (all(is.na(values))) {
                        values <- training_data |> pull(cur_column()) |> mean()
                      }
                      values
                    }))
    # rerun prediction with new mean-fills
    predicted <- stats::predict(object = object, new_data = test_data, type = NULL)
  }
  
  if (isTRUE(only_prediction)) {
    out <- predicted[[1]]
    if (all(out %in% c("TRUE", "FALSE", NA))) {
      out <- as.logical(out)
    }
  } else {
    preds <- stats::predict(object, test_data, type = "prob")
    out <- bind_cols(stats::setNames(predicted, "predicted"),
                     preds) |> 
        mutate(certainty = row_function(max, data = preds), .after = 1)
    if (all(out$predicted %in% c("TRUE", "FALSE", NA))) {
      out$predicted <- as.logical(out$predicted)
    }
  }
  out
}

#' @importFrom caret confusionMatrix
#' @method confusionMatrix certestats_ml
#' @rdname machine_learning
#' @export
confusionMatrix.certestats_ml <- function(data, ...) {
  conf_mtrx <- as.matrix(table(attributes(data)$predictions$predicted,
                               attributes(data)$predictions$truth))
  # not more columns than rows
  conf_mtrx <- conf_mtrx[, seq_len(NROW(conf_mtrx))]
  confusionMatrix(conf_mtrx)
}

#' @importFrom yardstick metrics
#' @method metrics certestats_ml
#' @rdname machine_learning
#' @export
metrics.certestats_ml <- function(data, ...) {
  as.data.frame(attributes(data)$metrics, stringsAsFactors = FALSE)
}

#' @method autoplot certestats_ml
#' @rdname machine_learning
#' @param plot_type the plot type, can be `"roc"` (default), `"gain"`, `"lift"` or `"pr"`. These functions rely on [yardstick::roc_curve()], [yardstick::gain_curve()], [yardstick::lift_curve()] and [yardstick::pr_curve()] to construct the curves.
#' @details Use [autoplot()] on a model to plot the receiver operating characteristic (ROC) curve, showing the relation between sensitivity and specificity. This plotting function uses [yardstick::roc_curve()] to construct the curve. The (overall) area under the curve (AUC) will be printed as subtitle.
#' @importFrom ggplot2 autoplot ggplot aes geom_path geom_abline coord_equal scale_x_continuous scale_y_continuous labs element_line
#' @importFrom dplyr bind_cols starts_with
#' @importFrom yardstick roc_curve roc_auc gain_curve lift_curve pr_curve
#' @export
autoplot.certestats_ml <- function(object, plot_type = "roc", ...) {
  model_prop <- attributes(object)
  plot_type <- tolower(plot_type)[1L]
  
  if (plot_type == "roc") {
    curve_fn <- roc_curve
  } else if (plot_type == "gain") {
    curve_fn <- gain_curve
  } else if (plot_type == "lift") {
    curve_fn <- lift_curve
  } else if (plot_type == "pr") {
    curve_fn <- pr_curve
  } else {
    stop("invalid plot_type", call. = FALSE)
  }
  
  if (all(c(".pred_TRUE", ".pred_FALSE") %in% colnames(model_prop$predictions))) {
    curve <- curve_fn(model_prop$predictions,
                      truth = truth,
                      1)
    if (plot_type == "roc") {
      roc_auc <- roc_auc(model_prop$predictions,
                         truth = truth,
                         1)
    }
  } else {
    curve <- curve_fn(model_prop$predictions,
                      truth = truth,
                      starts_with(".pred"))
    if (plot_type == "roc") {
      roc_auc <- roc_auc(model_prop$predictions,
                         truth = truth,
                         starts_with(".pred"))
    }
  }
  
  if (plot_type == "roc") {
    if (".level" %in% colnames(curve) &&
        !all(c(".pred_TRUE", ".pred_FALSE") %in% colnames(model_prop$predictions))) {
      for (val in unique(curve$.level)) {
        auc <- get_auc(model_prop$predictions, val)
        curve$.level[curve$.level == val] <- paste0(curve$.level[curve$.level == val],
                                                    " (AUC: ", round(auc$.estimate, digits = 3), ")")
      }
      multiple_outcomes <- TRUE
      p <- ggplot(curve, aes(x = 1 - specificity, y = sensitivity, colour = .level))
    } else {
      multiple_outcomes <- FALSE
      p <- ggplot(curve, aes(x = 1 - specificity, y = sensitivity))
    }
    p <- p +
      geom_path(size = 0.75) +
      geom_abline(lty = 3) +
      coord_equal() +
      scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(x * 100, "%")) +
      scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x * 100, "%")) +
      labs(title = "Receiver Operating Characteristic (ROC) Curve",
           subtitle = paste0(ifelse(isTRUE(multiple_outcomes), "Overall ", ""),
                             "Area Under the Curve (AUC): ",
                             round(roc_auc$.estimate, digits = 3)),
           colour = "Outcome Variable")
    
  } else if (plot_type == "pr") {
    suppressMessages(
      p <- curve |>
        # thse is defined in the yardstick package:
        autoplot() +
        scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(x * 100, "%")) +
        scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x * 100, "%")) +
        labs(title = "Precision Recall (PR) Curve")
    )
    
  } else {
    p <- curve |>
      # these ones are defined in the yardstick package:
      autoplot() +
      scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(x, "%")) +
      scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x, "%")) +
      labs(title = paste(tools::toTitleCase(plot_type), "Curve"))
    
  }
  
  if ("certeplot2" %in% rownames(utils::installed.packages())) {
    p <- p +
      certeplot2::theme_minimal2() +
      certeplot2::scale_colour_certe_d()
  }
  
  p
}

#' @rdname machine_learning
#' @export
get_recipe <- function(object) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  attributes(object)$recipe
}

#' @rdname machine_learning
#' @param only_params_in_model a [logical] to indicate whether only parameters in the model should be tuned
#' @inheritParams dials::grid_regular
#' @inheritParams rsample::vfold_cv
#' @details Use the [tune_parameters()] function to analyse tune parameters of any `ml_*()` function. Without any parameters manually defined, it will try to tune all parameters of the underlying ML model. The tuning will be based on a [V-fold cross-validation][rsample::vfold_cv()], of which the number of partitions can be set with `v`. The number of `levels` will be used to split the range of the parameters. For example, a range of 1-10 with `levels = 2` will lead to `[1, 10]`, while `levels = 5` will lead to `[1, 3, 5, 7, 9]`. The resulting [data.frame] will be sorted from best to worst. These results can also be plotted using [autoplot()].
#' @importFrom parsnip set_engine set_mode
#' @importFrom dials grid_regular
#' @importFrom workflows workflow add_model add_formula
#' @importFrom rsample vfold_cv
#' @importFrom tune tune_grid collect_metrics
#' @importFrom hardhat tune
#' @importFrom dplyr arrange desc across starts_with rename_with everything
#' @importFrom tidyr pivot_wider
#' @export
tune_parameters <- function(object, ..., only_params_in_model = FALSE, levels = 5, v = 10) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  
  model_prop <- attributes(object)
  dots <- list(...)
  
  # get the parsnip function and its parameters
  FUN <- eval(parse(text = model_prop$properties$ml_function))
  params <- names(formals(FUN))
  params <- params[!params %in% c("mode", "engine")]
  if (isTRUE(only_params_in_model)) {
    params <- params[params %in% names(model_prop$properties)]
  }
  params <- lapply(stats::setNames(as.list(params), params), function(x) tune())
  
  # create the grid
  if (length(dots) > 0) {
    # parameters were specified manually in ...
    if (is.null(names(dots)) || any(names(dots) == "")) {
      stop("All tune parameters must be named if they are specified manually, e.g. `trees = dials::trees()`", call. = FALSE)
    }
    params <- params[names(params) %in% names(dots)]
    dials_fns <- dots
    names(dials_fns) <- NULL
  } else {
    message("Assuming tuning analysis for the ", length(params), " parameters ", paste0("'", names(params), "'", collapse = ", "),
            ".\nUse e.g. `", names(params)[1], " = dials::", names(params)[1], "()` to specify tuning for less parameters.")
    dials_fns <- lapply(names(params), function(p) {
      dials_fn <- eval(parse(text = paste0("dials::", p)))
      if (!is.null(dials_fn()$range$lower) && !is.numeric(dials_fn()$range$lower)) {
        stop("Unknown lower range in parameter '", p, "'. Specify dials::", p, "() manually.")
      }
      if (!is.null(dials_fn()$range$upper) && !is.numeric(dials_fn()$range$upper)) {
        if (p == "mtry") {
          new_upper <- ncol(model_prop$data_training) - 1
          # it's a random guess from the predictors, so take that length as upper range
          message("Assuming upper range of ", new_upper, " for `dials::", p, "()` because of number of available predictors.")
          return(dials_fn(range = c(dials_fn()$range$lower, new_upper)))
        } else {
          stop("Unknown upper range in parameter '", p, "'. Specify dials::", p, "() manually.")
        }
      }
      dials_fn()
    })
  }
  tree_grid <- do.call(grid_regular,
                       args = c(dials_fns, list(levels = levels)))
  
  # (re)create the model specification
  model_spec <- do.call(FUN, args = params) |> 
    set_engine(model_prop$properties$engine_package) |> 
    set_mode(model_prop$properties$mode)
  
  # create the worflow, using the tuning specification
  tree_wf <- workflow() |>
    add_model(model_spec) |>
    add_formula(outcome ~ .)

  # create the V-fold cross-validation (also known as k-fold cross-validation)
  vfold <- vfold_cv(model_prop$data_training, v = v)
  
  # run the tuning
  if (interactive()) {
    cat("\n")
    ans <- utils::askYesNo(paste0("This will run a tuning analysis using a ",
                                  v, "-fold cross-validation for ",
                                  nrow(tree_grid), " combinations. Continue?"))
    if (!isTRUE(ans)) {
      return(invisible(NULL))
    } else {
      message("[", Sys.time(), "] Running tuning workflow...")
    }
  } else {
    message("[", Sys.time(), "] Running tuning analysis using a ", v, "-fold cross-validation for ", nrow(tree_grid), " combinations...")
  }
  suppressWarnings(
    tree_res <- tree_wf |>
      tune_grid(resamples = vfold,
                grid = tree_grid)
  )
  out <- tree_res |>
    collect_metrics() 
  message("[", Sys.time(), "] Done.")
  
  # return the results, arranging according the best metrics on average
  structure(out |> 
              pivot_wider(-.estimator, names_from = .metric, values_from = c(mean, std_err)) |>
              arrange(desc(across(starts_with("mean_"))),
                      across(everything())) |>
              rename_with(function(x) gsub("^mean_", "", x)) |>
              rename_with(function(x) gsub("^std_err_(.*)", "\\1_se", x)),
            class = c("certestats_tuning", class(out)),
            result = tree_res)
}

#' @method autoplot certestats_tuning
#' @inheritParams tune::autoplot.tune_results
#' @importFrom ggplot2 scale_y_continuous labs
#' @rdname machine_learning
#' @export
autoplot.certestats_tuning <- function(object, type = c("marginals", "parameters", "performance"), ...) {
  p <- autoplot(attributes(object)$result, type = type[1]) +
    scale_y_continuous(expand = c(0.05, 0),
                       limits = c(NA, 1),
                       labels = function(x) paste0(format(x * 100), "%")) +
    labs(title = "Tuning Parameters")
  
  if ("certeplot2" %in% rownames(utils::installed.packages())) {
    p <- p +
      certeplot2::theme_minimal2() +
      certeplot2::scale_colour_certe_d()
  }
  p
}
