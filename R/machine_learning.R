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

#' Create a Machine Learning (ML) Model
#'
#' These functions can be used to create a machine learning model based on different 'engines' and to generalise predicting outcomes based on such models. These functions are wrappers around `tidymodels` packages (especially [`parsnip`](https://parsnip.tidymodels.org), [`recipes`](https://recipes.tidymodels.org), [`rsample`](https://rsample.tidymodels.org), [`tune`](https://tune.tidymodels.org), and [`yardstick`](https://yardstick.tidymodels.org)) created by RStudio.
#' @param .data Data set to train
#' @param outcome Outcome variable, also called the *response variable* or the *dependent variable*; the variable that must be predicted. The value will be evaluated in [`select()`][dplyr::select()] and thus supports the `tidyselect` language. In case of classification prediction, this variable will be coerced to a [factor].
#' @param predictors Explanatory variables, also called the *predictors* or the *independent variables*; the variables that are used to predict `outcome`. These variables will be transformed using [as.double()] ([factor]s will be transformed to [character]s first). This value defaults to [`everything()`][tidyselect::everything()] and supports the `tidyselect` language.
#' @param training_fraction Fraction of rows to be used for *training*, defaults to 75%. The rest will be used for *testing*. If given a number over 1, the number will be considered to be the required number of rows for *training*.
#' @param correlation_threshold A value (default 0.9) to indicate the correlation threshold. Predictors with a correlation higher than this value with be removed from the model, using [recipes::step_corr()]
# @param na_filter A [logical] to remove rows where the `predictors` contain `NA`, using [recipes::step_naomit()]
#' @param centre A [logical] to indicate whether the `predictors` should be transformed so that their mean will be `0`, using [recipes::step_center()]. Binary columns will be skipped.
#' @param scale A [logical] to indicate whether the `predictors` should be transformed so that their standard deviation will be `1`, using [recipes::step_scale()]. Binary columns will be skipped.
#' @param na_threshold Maximum fraction of `NA` values (defaults to `0.01`) of the `predictors` before they are removed from the model, using [recipes::step_rm()]
#' @param mode Type of predicted value - defaults to `"classification"`, but can also be `"unknown"` or `"regression"`
#' @param engine \R package or function name to be used for the model, will be passed on to [parsnip::set_engine()]
#' @param ... Arguments to be passed on to the `parsnip` functions, see *Model Functions*.
#' 
#' For the [tune_parameters()] function, these must be `dials` package calls, such as `dials::trees()` (see Examples).
#' 
#' For [`predict()`][parsnip::predict.model_fit()], these must be arguments passed on to [parsnip::predict.model_fit()]
#' @inheritParams parsnip::decision_tree
#' @inheritParams parsnip::linear_reg
#' @inheritParams parsnip::logistic_reg
#' @inheritParams parsnip::mlp
#' @inheritParams parsnip::nearest_neighbor
#' @inheritParams parsnip::rand_forest
#' @inheritParams parsnip::boost_tree
#' @inheritParams rsample::initial_split
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
#' @return A machine learning model of class `certestats_ml` / ... / `model_fit`.
#' 
#' @section Attributes:
#' The `ml_*()` functions return the following [attributes][base::attributes()]:
#' 
#' * `properties`: a [list] with model properties: the ML function, engine package, training size, testing size, strata size, mode, and the different ML function-specific properties (such as `tree_depth` in [ml_decision_trees()])
#' * `recipe`: a [recipe][recipes::recipe()] as generated with [recipes::prep()], to be used for training and testing
#' * `data_original`: a [data.frame] containing the original data, possibly without invalid strata
#' * `data_structure`: a [data.frame] containing the original data structure (only trained variables) with zero rows
#' * `data_means`: a [data.frame] containing the means of the original data (only trained variables)
#' * `data_training`: a [data.frame] containing the training data of `data_original`
#' * `data_testing`: a [data.frame] containing the testing data of `data_original`
#' * `rows_training`: an [integer] vector of rows used for training in `data_original`
#' * `rows_testing`: an [integer] vector of rows used for training in `data_original`
#' * `predictions`: a [data.frame] containing predicted values based on the testing data
#' * `metrics`: a [data.frame] with model metrics as returned by [yardstick::metrics()]
#' * `correlation_threshold`: a [logical] indicating whether [recipes::step_corr()] has been applied
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
#'  * `ml_xg_boost`: [parsnip::xgb_train()]
#' @name machine_learning
#' @rdname machine_learning
#' @export
#' @examples
#' # 'esbl_tests' is an included data set, see ?esbl_tests
#' print(esbl_tests, n = 5)
#' 
#' esbl_tests |> correlation_plot(add_values = FALSE) # red will be removed from model
#' 
#' # predict ESBL test outcome based on MICs using 2 different models
#' model1 <- esbl_tests |> ml_xg_boost(esbl, where(is.double))
#' model2 <- esbl_tests |> ml_decision_trees(esbl, where(is.double))
#' 
#' 
#' # Assessing A Model ----------------------------------------------------
#' 
#' model1 |> get_metrics()
#' model2 |> get_metrics()
#' 
#' model1 |> confusion_matrix()
#' 
#' # a correlation plot of a model shows the training data
#' model1 |> correlation_plot(add_values = FALSE)
#' 
#' model1 |> feature_importances()
#' model1 |> feature_importances() |> autoplot()
#' model2 |> feature_importance_plot()
#' 
#' # decision trees can also have a tree plot
#' model2 |> tree_plot()
#' 
#' 
#' # Applying A Model -----------------------------------------------------
#'  
#' # simply use base R `predict()` to apply a model:
#' model1 |> predict(esbl_tests)
#' 
#' # but apply_model_to() contains more info and can apply corrections:
#' model1 |> apply_model_to(esbl_tests)
#' # and to format the result, e.g. for an API:
#' model1 |> apply_model_to(esbl_tests) |> dplyr::slice(1:10) |> format()
#' 
#' # put in only parts of new data:
#' model1 |> apply_model_to(esbl_tests[, 1:15])
#' esbl_tests2 <- esbl_tests
#' esbl_tests2[2, "CIP"] <- NA
#' esbl_tests2[5, "AMC"] <- NA
#' # with XGBoost, nothing will be changed (it can correct for missings):
#' model1 |> apply_model_to(esbl_tests2)
#' # with random forest (or others), missings will be imputed:
#' model2 |> apply_model_to(esbl_tests2)
#' 
#' 
#' # Tuning A Model -------------------------------------------------------
#'  
#' # tune the parameters of a model (will take some time)
#' tuning <- model2 |> 
#'   tune_parameters(k = 5, levels = 3)
#' autoplot(tuning)
#' 
#' # tuning analysis by specifying (some) parameters
#' iris |> 
#'   ml_xg_boost(Species) |> 
#'   tune_parameters(mtry = dials::mtry(range = c(1, 3)),
#'                   trees = dials::trees())
#' 
#' 
#' # Practical Example #1 --------------------------------------------------
#' 
#' # this is what iris data set looks like:
#' head(iris)
#' # create a model to predict the species:
#' iris_model <- iris |> ml_xg_boost(Species)
#' iris_model_rf <- iris |> ml_random_forest(Species)
#' # is it a bit reliable?
#' get_metrics(iris_model)
#' 
#' # now try to predict species from an arbitrary data set:
#' to_predict <- data.frame(Sepal.Length = 5,
#'                          Sepal.Width = 3,
#'                          Petal.Length = 1.5,
#'                          Petal.Width = 0.5)
#' to_predict
#' 
#' # should be 'setosa' in the 'predicted' column with huge certainty:
#' iris_model |> apply_model_to(to_predict)
#' 
#' # API formatting:
#' iris_model |> apply_model_to(to_predict) |> format()
#' iris_model |> apply_model_to(to_predict, only_prediction = TRUE)
#' iris_model |> apply_model_to(to_predict, only_certainty = TRUE)
#' 
#' # which variables are generally important (only trained variables)?
#' iris_model |> feature_importances()
#' 
#' # how would the model do without the 'Sepal.Length' column?
#' to_predict <- to_predict[, c("Sepal.Width", "Petal.Width", "Petal.Length")]
#' to_predict
#' iris_model |> apply_model_to(to_predict)
#' 
#' # now compare that with a random forest model that requires imputation:
#' iris_model_rf |> apply_model_to(to_predict)
#' 
#' # the certainly is very different.
#' 
#' 
#' # Practical Example #2 -------------------------------------------------
#' 
#' # this example shows plotting methods for a model
#' 
#' # train model to predict genus based on MICs:
#' genus <- esbl_tests |> ml_xg_boost(genus, everything())
#' genus |> get_metrics()
#' genus |> feature_importance_plot()
#' genus |> autoplot()
#' genus |> autoplot(plot_type = "gain")
#' genus |> autoplot(plot_type = "pr")
ml_xg_boost <- function(.data,
                        outcome,
                        predictors = everything(),
                        training_fraction = 0.75,
                        strata = NULL,
                        na_threshold = 0.01,
                        correlation_threshold = 0.9,
                        centre = TRUE,
                        scale = TRUE,
                        engine = "xgboost",
                        mode = c("classification", "regression", "unknown"),
                        trees = 15,
                        ...) {
  ml_exec(FUN = parsnip::boost_tree,
          .data = .data,
          outcome = {{ outcome }},
          predictors = {{ predictors }},
          training_fraction = training_fraction,
          strata = {{ strata }},
          na_threshold = na_threshold,
          correlation_threshold = correlation_threshold,
          centre = centre,
          scale = scale,
          engine = engine,
          mode = mode[1L],
          trees = trees,
          ...)
}

#' @rdname machine_learning
#' @export
ml_decision_trees <- function(.data,
                              outcome,
                              predictors = everything(),
                              training_fraction = 0.75,
                              strata = NULL,
                              na_threshold = 0.01,
                              correlation_threshold = 0.9,
                              centre = TRUE,
                              scale = TRUE,
                              engine = "rpart",
                              mode = c("classification", "regression", "unknown"),
                              tree_depth = 30,
                              ...) {
  ml_exec(FUN = parsnip::decision_tree,
          .data = .data,
          outcome = {{ outcome }},
          predictors = {{ predictors }},
          training_fraction = training_fraction,
          strata = {{ strata }},
          na_threshold = na_threshold,
          correlation_threshold = correlation_threshold,
          centre = centre,
          scale = scale,
          engine = engine,
          mode = mode[1L],
          tree_depth = tree_depth,
          ...)
}

#' @rdname machine_learning
#' @export
ml_random_forest <- function(.data,
                             outcome,
                             predictors = everything(),
                             training_fraction = 0.75,
                             strata = NULL,
                             na_threshold = 0.01,
                             correlation_threshold = 0.9,
                             centre = TRUE,
                             scale = TRUE,
                             engine = "ranger",
                             mode = c("classification", "regression", "unknown"),
                             trees = 500,
                             ...) {
  ml_exec(FUN = parsnip::rand_forest,
          .data = .data,
          outcome = {{ outcome }},
          predictors = {{ predictors }},
          training_fraction = training_fraction,
          strata = {{ strata }},
          na_threshold = na_threshold,
          correlation_threshold = correlation_threshold,
          centre = centre,
          scale = scale,
          engine = engine,
          mode = mode[1L],
          trees = trees,
          ...)
}

#' @rdname machine_learning
#' @export
ml_neural_network <- function(.data,
                              outcome,
                              predictors = everything(),
                              training_fraction = 0.75,
                              strata = NULL,
                              na_threshold = 0.01,
                              correlation_threshold = 0.9,
                              centre = TRUE,
                              scale = TRUE,
                              engine = "nnet",
                              mode = c("classification", "regression", "unknown"),
                              penalty = 0,
                              epochs = 100,
                              ...) {
  ml_exec(FUN = parsnip::mlp,
          .data = .data,
          outcome = {{ outcome }},
          predictors = {{ predictors }},
          training_fraction = training_fraction,
          strata = {{ strata }},
          na_threshold = na_threshold,
          correlation_threshold = correlation_threshold,
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
                                 training_fraction = 0.75,
                                 strata = NULL,
                                 na_threshold = 0.01,
                                 correlation_threshold = 0.9,
                                 centre = TRUE,
                                 scale = TRUE,
                                 engine = "kknn",
                                 mode = c("classification", "regression", "unknown"),
                                 neighbors = 5,
                                 weight_func = "triangular",
                                 ...) {
  ml_exec(FUN = parsnip::nearest_neighbor,
          .data = .data,
          outcome = {{ outcome }},
          predictors = {{ predictors }},
          training_fraction = training_fraction,
          strata = {{ strata }},
          na_threshold = na_threshold,
          correlation_threshold = correlation_threshold,
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
ml_linear_regression <- function(.data,
                                 outcome,
                                 predictors = everything(),
                                 training_fraction = 0.75,
                                 strata = NULL,
                                 na_threshold = 0.01,
                                 correlation_threshold = 0.9,
                                 centre = TRUE,
                                 scale = TRUE,
                                 engine = "lm",
                                 mode = "regression",
                                 ...) {
  ml_exec(FUN = parsnip::linear_reg,
          .data = .data,
          outcome = {{ outcome }},
          predictors = {{ predictors }},
          training_fraction = training_fraction,
          strata = {{ strata }},
          na_threshold = na_threshold,
          correlation_threshold = correlation_threshold,
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
                                   training_fraction = 0.75,
                                   strata = NULL,
                                   na_threshold = 0.01,
                                   correlation_threshold = 0.9,
                                   centre = TRUE,
                                   scale = TRUE,
                                   engine = "glm",
                                   mode = "classification",
                                   penalty = 0.1,
                                   ...) {
  ml_exec(FUN = parsnip::logistic_reg,
          .data = .data,
          outcome = {{ outcome }},
          predictors = {{ predictors }},
          training_fraction = training_fraction,
          strata = {{ strata }},
          na_threshold = na_threshold,
          correlation_threshold = correlation_threshold,
          centre = centre,
          scale = scale,
          engine = engine,
          mode = mode,
          penalty = penalty,
          ...)
}

#' @importFrom dplyr mutate select across filter bind_cols all_of slice summarise mutate_all where
#' @importFrom yardstick metrics
#' @importFrom parsnip set_engine
#' @importFrom recipes recipe step_corr step_center step_scale step_rm step_naomit all_predictors all_outcomes prep bake step_mutate_at step_dummy all_nominal_predictors all_numeric_predictors
#' @importFrom rsample initial_split training testing
#' @importFrom certestyle format2
#' @importFrom generics fit
ml_exec <- function(FUN,
                    .data,
                    outcome,
                    predictors,
                    training_fraction,
                    strata,
                    na_threshold,
                    correlation_threshold,
                    centre,
                    scale,
                    engine,
                    ...) {
  
  start_the_clock <- Sys.time()
  
  if (!engine %in% c("lm", "glm")) {
    # this will ask to install packages like ranger or rpart
    check_is_installed(engine)
  }
  
  # show which arguments are useful to set in the function:
  args_function <- formals(FUN)
  args_given <- list(...)
  # update function arguments with given arguments
  args_to_note <- utils::modifyList(args_function, args_given)
  if (length(args_to_note) > 0 && interactive()) {
    args_msg <- paste("- ", names(args_to_note), "=", sapply(args_to_note, function(x) if (!is.null(x)) deparse(x) else "NULL"), collapse = "\n")
    message("Arguments currently set for `", deparse(substitute(FUN)), "()`:\n", args_msg)
  }
  
  err_msg <- ""
  n_pred <- tryCatch(ncol(select(.data, {{ predictors }})), 
                     error = function(e) {
                       err_msg <<- e$message
                       NULL
                     })
  if (is.null(n_pred) || n_pred == 0) {
    if (err_msg == "") {
      err_msg <- "is argument 'predictors' missing?"
    }
    stop("no columns found for argument 'predictors': ", err_msg, call. = FALSE)
  }
  
  # select only required data
  df <- .data |>
    as.data.frame(stringsAsFactors = FALSE) |> 
    mutate(.strata = {{ strata }})
  if (".strata" %in% colnames(df)) {
    .strata <- ".strata"
    # check NAs
    if (any(is.na(df$`.strata`))) {
      warning("Some strata were NA, these rows were removed before training (n = ", sum(is.na(df$`.strata`)), ")", call. = FALSE)
      df <- df |>
        filter(!is.na(.strata))
    }
    # check strata with size 1
    strata_tbl <- table(df$`.strata`)
    if (any(as.double(strata_tbl) == 1)) {
      strata_to_remove <- names(strata_tbl)[strata_tbl == 1]
      warning(length(strata_to_remove), " strata only contained one row, these rows were removed before training (n = ",
              df |>
                filter(.strata %in% strata_to_remove) |>
                nrow(),
              ")", call. = FALSE)
      df <- df |>
        filter(!.strata %in% strata_to_remove)
    }
  } else {
    .strata <- NULL
  }
  # save this one for later
  df.bak <- df |> select(-all_of(.strata))
  # select only columns to keep in model for now
  df <- df |> select(outcome = {{ outcome }}, {{ predictors }}, all_of(.strata))
  
  # the outcome variable must be factor in case of classification prediction
  if (list(...)$mode == "classification" && !inherits(df$outcome, c("factor", "character"))) {
    if (is.logical(df$outcome)) {
      df$outcome <- factor(df$outcome, levels = c(TRUE, FALSE))
    } else {
      if (is.numeric(df$outcome)) {
        message("NOTE: The outcome variable is numeric, should the mode not be 'regression' instead of 'classification'?")
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
    message("NOTE: Training size set to ", round(training_fraction * nrow(df)),
            ", while data size is ", nrow(df),
            " - training size has been set to ", nrow(df) - 1)
    training_fraction <- (nrow(df) - 1) / nrow(df)
  }
  
  suppressWarnings(
    properties <- c(list(ml_function = deparse(substitute(FUN)),
                         engine_package = engine,
                         data_size = nrow(df),
                         training_fraction = training_fraction,
                         training_size = round(training_fraction * nrow(df)),
                         testing_size = round((1 - training_fraction) * nrow(df)),
                         strata = if (is.null(df$`.strata`)) NULL else table(df$`.strata`)),
                    list(...))
  )
  
  df_split <- initial_split(df, strata = all_of(.strata), prop = training_fraction)
  df_split_train <- df_split |> training() |> select(-all_of(.strata))
  df_split_test <- df_split |> testing() |> select(-all_of(.strata))
  
  ## Create recipe ----
  mdl_recipe <- df_split_train |> recipe(outcome ~ .)
  
  # make dummies for all characters/factors
  mdl_recipe <- mdl_recipe |> step_dummy(all_nominal_predictors())
  # flag all binary columns
  mdl_recipe <- mdl_recipe |> step_mutate_at(all_predictors(), fn = try_binary)
  # unselect columns with too many NAs
  mdl_recipe <- mdl_recipe |> step_rm(where(function(x) sum(is.na(x)) / length(x) > !!na_threshold))
  # unselect columns that correlate too much
  mdl_recipe <- mdl_recipe |> step_corr(all_predictors(), -where(is.binary), threshold = correlation_threshold)
  # filter rows with NA
  mdl_recipe <- mdl_recipe |> step_naomit(all_predictors())
  # centre out numeric features
  if (isTRUE(centre)) {
    mdl_recipe <- mdl_recipe |> step_center(all_predictors(), -all_outcomes(), -where(is.binary))
  }
  # scale out numeric features
  if (isTRUE(scale)) {
    mdl_recipe <- mdl_recipe |> step_scale(all_predictors(), -all_outcomes(), -where(is.binary))
  }
  mdl_recipe <- mdl_recipe |> prep()
  
  ## Fit model ----
  
  # train
  df_training <- mdl_recipe |> bake(new_data = NULL)
  
  # test
  df_testing <- mdl_recipe |> bake(df_split_test)
  
  # create actual model
  mdl <- FUN(...) |> set_engine(engine)
  if (interactive()) {
    message("\n[", format2(Sys.time(), "HH:MM:SS"), "] Fitting model...", appendLF = FALSE)
  }
  mdl <- mdl |> fit(outcome ~ ., data = df_training)
  if (interactive()) {
    message("Done.")
  }
  
  ## Get performance metrics ----
  
  # save structure of original input data
  df_structure <- df.bak |> 
    select(all_of(colnames(df_training)[colnames(df_training) %in% colnames(df.bak)])) |> 
    slice(0)
  df_means <- df.bak |> 
    summarise(across(where(function(col) mode(col) == "numeric"),
                     function(x) suppressWarnings(as.double(mean(x, na.rm = TRUE)))))
  
  metrics <- mdl |>
    stats::predict(df_testing) |>
    bind_cols(df_testing)
  metrics <- metrics |>
    metrics(truth = outcome, estimate = colnames(metrics)[1])
  
  if (properties$mode == "classification") {
    prediction <- stats::predict(mdl, df_testing, type = "prob")
    prediction <- prediction |>
      mutate(certainty = row_function(max, data = prediction), .before = 1)
  } else {
    prediction <- stats::predict(mdl, df_testing, type = "numeric")
  }
  pred_outcome <- stats::predict(mdl, df_testing)
  colnames(pred_outcome) <- "predicted"
  predictions <- bind_cols(data.frame(truth = df_testing$outcome, stringsAsFactors = FALSE),
                           pred_outcome,
                           prediction)
  
  run_time <- Sys.time() - start_the_clock
  
  if (interactive()) {
    message("\nCreated ML model with these metrics:\n",
            paste("-", metrics$.metric, "=", round(metrics$.estimate, 3), collapse = "\n"))
    message("\nModel trained in ~", format(round(run_time)), ".\n")
  }
  
  ## Return model with properties ----
  
  structure(mdl,
            class = c("certestats_ml", class(mdl)),
            properties = properties,
            recipe = mdl_recipe,
            data_original = df.bak,
            data_structure = df_structure,
            data_means = df_means,
            data_training = df_training,
            data_testing = df_testing,
            rows_training = sort(df_split$in_id),
            rows_testing = seq_len(nrow(df))[!seq_len(nrow(df)) %in% df_split$in_id],
            predictions = predictions,
            metrics = metrics,
            correlation_threshold = correlation_threshold,
            centre = centre,
            scale = scale,
            run_time = run_time)
}

is_xgboost <- function(object) {
  identical(attributes(object)$properties$engine_package, "xgboost")
}
is_decisiontree <- function(object) {
  identical(attributes(object)$properties$engine_package, "rpart")
}

#' @method print certestats_ml
#' @importFrom cli cli_h1 cli_h2
#' @noRd
#' @export
print.certestats_ml <- function(x, ...) {
  model_prop <- attributes(x)
  cli_h1("Train Arguments")
  cat(paste0(format(names(model_prop$properties)), " : ", model_prop$properties, "\n"),
      sep = "")
  cat(paste0("\nTrained in ~", format(round(model_prop$run_time)), ".\n"))
  if (model_prop$properties$mode == "classification") {
    cli_h1("Classification Certainty")
    intervals <- c(0, 0.5, 0.68, 0.95, 0.98, 1)
    q <- quantile(model_prop$predictions$certainty, intervals, na.rm = TRUE)
    names(q) <- paste0("p", intervals * 100)
    print(q)
    cat("\nBased on testing data.\n")
  }
  cli_h1("Metrics")
  print(metrics(x))
  print(model_prop$recipe)
  # print the rest like it used to be
  cli_h1("Model Object")
  class(x) <- class(x)[class(x) != "certestats_ml"]
  print(x)
}

#' @method confusion_matrix certestats_ml
#' @rdname machine_learning
#' @export
confusion_matrix.certestats_ml <- function(data, ...) {
  attributes(data)$predictions |>
  confusion_matrix(truth:predicted)
}

#' @rdname machine_learning
#' @param object,data outcome of machine learning model
#' @inheritParams parsnip::predict.model_fit
#' @details The [`predict()`][parsnip::predict.model_fit()] function can be used to fit a model on a new data set. Its wrapper [apply_model_to()] works in the same way, but can also detects and fixes missing variables, missing data points, and data type differences between the trained data and the input data.
#' @importFrom dplyr as_tibble
#' @export
predict.certestats_ml <- function(object,
                                  new_data,
                                  type = NULL,
                                  ...) {
  out <- apply_model_to(object = object,
                        new_data = new_data,
                        type = type,
                        only_prediction = FALSE,
                        correct_mistakes = FALSE,
                        ...)
  as_tibble(out[, 1])
}

#' @rdname machine_learning
#' @param only_prediction a [logical] to indicate whether predictions must be returned as [vector], otherwise returns a [data.frame]
#' @param only_certainty a [logical] to indicate whether certainties must be returned as [vector], otherwise returns a [data.frame]
#' @param correct_mistakes a [logical] to indicate whether missing variables and missing values should be added to `new_data`
#' @param impute_algorithm the algorithm to use in [impute()] if `correct_mistakes = TRUE`. Can be `"mice"` (default) for the [Multivariate Imputations by Chained Equations (MICE) algorithm][mice::mice], or `"single-point"` for a trained median.
#' @importFrom recipes bake remove_role
#' @importFrom dplyr bind_cols mutate filter pull as_tibble select any_of
#' @export
apply_model_to <- function(object,
                           new_data,
                           only_prediction = FALSE,
                           only_certainty = FALSE,
                           correct_mistakes = TRUE,
                           impute_algorithm = "mice",
                           ...) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }

  # remove class so there will not be an infinite loop
  class(object) <- setdiff(class(object), "certestats_ml")
  
  cols_required <- get_recipe(object)$var_info |>
    filter(role == "predictor") |>
    pull(variable)
  # remove redundant columns
  new_data <- new_data |> 
    select(any_of(cols_required))
  
  
  # check (1): new_data must not have missing columns ----
  
  cols_missing <- setdiff(cols_required, colnames(new_data))
  if (length(cols_missing) > 0) {
    if (!isTRUE(correct_mistakes) && is_xgboost(object)) {
      stop("Missing variables in `new_data`: ", toString(cols_missing), ".")
    }
    if (!is_xgboost(object)) {
      for (col in cols_missing) {
        # add as NA in the class of the original data:
        new_val <- attributes(object)$data_original[[col]]
        if (is.numeric(new_val)) {
          # take median
          message("Adding missing variable as median: ", col)
          new_val <- median(new_val, na.rm = TRUE)
          new_data[, col] <- new_val
        } else {
          # take mode, value that occurs most often
          message("Adding missing variable as mode value: ", col)
          new_val <- new_val |> table()
          new_val <- names(new_val[order(new_val, decreasing = TRUE)[1]])
          new_data[, col] <- new_val
        }
      }
    } else {
      # is xgboost, make the missings NA
      message("Missing variables in the data: ", toString(cols_missing), ", though XGBoost can function nonetheless")
      for (i in cols_missing) {
        new_data[, i] <- attributes(object)$data_original[1, i][2]
      }
    }
  }
  
  # check (2): new_data must not have missing values ----
  
  if (anyNA(new_data) && !is_xgboost(object)) {
    missings <- new_data |> vapply(FUN.VALUE = logical(1), anyNA)
    missings <- names(missings)[missings]
    if (!isTRUE(correct_mistakes)) {
      stop("Missing ", sum(is.na(new_data)), " data points in `new_data` variables: ", toString(missings), ".")
    }
    new_data <- new_data |> 
      impute(vars = all_of(missings),
             algorithm = impute_algorithm,
             m = 5,
             info = TRUE)
  }
  
  
  # check (3): new_data must not have different structure ----
  
  data_types_old <- vapply(attributes(object)$data_original |> select(any_of(colnames(new_data))), FUN.VALUE = character(1), mode)
  data_types_new <- vapply(new_data, FUN.VALUE = character(1), mode)
  data_type_diff <- names(which(data_types_old != data_types_new))
  if (length(data_type_diff) > 0) {
    if (!isTRUE(correct_mistakes)) {
      stop("Data structure difference between original data and `new_data`: ", toString(data_type_diff), ".")
    }
    for (col in data_type_diff) {
      old_mode <- mode(attributes(object)$data_original[[col]])
      FUN <- get(paste0("as.", old_mode))
      message("Transforming variable ", col, " from '", mode(new_data[[col]]), "' to '", old_mode, "'")
      new_data[, col] <- FUN(new_data[[col]])
    }
  }
  
  
  # bake recipe and get predictions ----
  model_recipe <- object |> get_recipe()
  if (is_xgboost(object) && length(cols_missing) > 0) {
    model_recipe <- model_recipe |>
      remove_role(any_of(cols_missing), old_role = "predictor")
  }
  if ("outcome" %in% model_recipe$var_info$variable && !"outcome" %in% colnames(new_data)) {
    # this will otherwise give an error because of applying step_rm() in ml_exec()
    new_data$outcome <- model_recipe$ptype$outcome[1]
  }
  # the actual baking
  new_data <- bake(model_recipe, new_data = new_data)
  out <- stats::predict(object, new_data, ...) # this includes `type` if coming from predict.certestats_ml()
  # return results ----
  if (isFALSE(only_prediction)) {
    preds <- stats::predict(object, new_data, type = "prob")
    out <- bind_cols(stats::setNames(out, "predicted"),
                     preds) |> 
      mutate(certainty = row_function(max, data = preds), .after = 1)
    if (all(out$predicted %in% c("TRUE", "FALSE", NA))) {
      out$predicted <- as.logical(out$predicted)
    }
  }
  names(out)[1] <- "predicted"
  
  if (isTRUE(only_prediction)) {
    out$predicted
  } else if (isTRUE(only_certainty)) {
    out$certainty
  } else {
    structure(as_tibble(out),
              class = c("certestats_pred_outcome", class(out)))
  }
}

#' @method format certestats_pred_outcome
#' @noRd
#' @export
format.certestats_pred_outcome <- function(x, ...) {
  out <- rep("", NROW(x))
  for(i in seq_len(NROW(out))) {
    if (!is.character(x$predicted) && !is.factor(x$predicted)) {
      out[i] <- paste0(x$predicted[i], " (", round(x$certainty[i] * 100, 1), "%)")
    } else {
      # list all prediction certainties
      all_predictions <- round(unlist(x[i,-c(1,2)]) * 100, 1)
      all_predictions <- sort(all_predictions, decreasing = TRUE)[-1] # remove first
      names(all_predictions) <- gsub("^[.]pred_", "", names(all_predictions))
      all_predictions <- paste0(names(all_predictions), " = ", all_predictions, "%")
      all_predictions <- paste0(all_predictions, collapse = ", ")
      out[i] <- paste0(x$predicted[i], " (", round(x$certainty[i] * 100, 1), "%; ", all_predictions, ")")
    }
  }
  out
}

#' @method metrics certestats_ml
#' @noRd
#' @export
metrics.certestats_ml <- function(data, ...) {
  get_metrics(data)
}

#' @rdname machine_learning
#' @importFrom dplyr select mutate as_tibble tibble arrange desc
#' @details
#' Use [feature_importances()] to get the importance of all features/variables. Use [autoplot()] afterwards to plot the results. These two functions are combined in [feature_importance_plot()].
#' @export
feature_importances <- function(object, ...) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  if (is_xgboost(object)) {
    out <- xgboost::xgb.importance(model = object$fit) |> 
      as_tibble() |>
      select(feature = Feature, gain = Gain, cover = Cover, frequency = Frequency)
    out$importance <- 0.6 * out$gain + 0.2 * out$cover + 0.2 * out$frequency
    out <- out |> 
      arrange(desc(importance))
    
  } else if (is_decisiontree(object)) {
    out <- object$fit$variable.importance
    out <- tibble(feature = names(out), importance = out / sum(out, na.rm = TRUE))
    
  } else {
    stop("Currently only supported for XGBoost and decision tree models.")
  }
  structure(out,
            class = c("certestats_feature_importances", class(out)))
}

#' @rdname machine_learning
#' @export
feature_importance_plot <- function(object, ...) {
  autoplot(feature_importances(object, ...))
}

#' @rdname machine_learning
#' @export
roc_plot <- function(object, ...) {
  autoplot(object, plot_type = "roc", ...)
}

#' @rdname machine_learning
#' @export
gain_plot <- function(object, ...) {
  autoplot(object, plot_type = "gain", ...)
}

#' @rdname machine_learning
#' @export
tree_plot <- function(object, ...) {
  if (!is_decisiontree(object)) {
    stop("Tree plots only work for decision tree models.")
  }
  rpart.plot::rpart.plot(object$fit, roundint = FALSE, ...)
}

#' @rdname machine_learning
#' @param add_values a [logical] to indicate whether values must be printed in the tiles
#' @param cols columns to use for correlation plot, defaults to [`everything()`][dplyr::everything()]
#' @details
#' Use [correlation_plot()] to plot the correlation between all variables, even characters. If the input is a `certestats` ML model, the training data of the model will be plotted.
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate_if select_if rename select filter mutate
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 geom_text scale_colour_manual
#' @importFrom certestyle colourpicker
#' @export
correlation_plot <- function(data, add_values = TRUE, cols = everything(), correlation_threshold = 0.9) {
  if (inherits(data, "certestats_ml")) {
    if (missing(correlation_threshold)) {
      correlation_threshold <- attributes(data)$correlation_threshold
    }
    data <- attributes(data)$data_training
  }
  
  if (!is.data.frame(data)) {
    stop('`data` must be a data.frame')
  }
  
  data <- data |> select({{ cols }})
  
  corr <- data |>
    mutate_if(is.character, as.factor) |>
    mutate_if(is.factor, as.integer) |>
    mutate_if(is.logical, as.integer) |>
    select_if(is.numeric) |>
    stats::cor() |>
    as.data.frame(stringsAsFactors = FALSE) |> 
    rownames_to_column(var = "rowname") |> 
    pivot_longer(-rowname, names_to = "y", values_to = "Correlation") |> 
    rename(x = rowname) |>
    filter(x != y) |>
    mutate(x = factor(x, levels = colnames(data), ordered = TRUE),
           y = factor(y, levels = rev(colnames(data)), ordered = TRUE))
  
  p <- ggplot(corr) +
    geom_tile(aes(x, y, fill = Correlation)) +
    geom_tile(data = corr |> filter(abs(Correlation) >= correlation_threshold & Correlation != 1),
              aes(x, y, colour = paste0("\u2265 ", correlation_threshold,
                                        "\n\u2264 -", correlation_threshold)),
              fill = NA,
              linewidth = 1) +
    scale_colour_manual(values = "red", labels = function(x) x) +
    labs(title = "Correlation Plot",
         y = "",
         x = "",
         colour = "Threshold",
         caption = paste("Min-max:", paste(round(range(corr$Correlation, na.rm = TRUE), 2), collapse = "-")))
  
  if ("package:certeplot2" %in% search()) {
    p <- p +
      plot2::theme_minimal2() +
      scale_fill_gradient2(low = colourpicker("certeroze0"),
                           mid = "white",
                           high = colourpicker("certeblauw0"),
                           limits = c(-1, 1))
  } else {
    p <- p +
      scale_fill_gradient2(limits = c(-1, 1))
  }
  
  if (isTRUE(add_values)) {
    p <- p +
      geom_text(aes(x, y, label = round(Correlation, 2)),
                colour = ifelse(abs(corr$Correlation) > 0.55, "white", "black"),
                size = 3)
  }
  
  p
  
}

#' @rdname machine_learning
#' @importFrom yardstick metrics
#' @export
get_metrics <- function(object) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  as.data.frame(attributes(object)$metrics, stringsAsFactors = FALSE)
}

#' @rdname machine_learning
#' @export
get_accuracy <- function(object) {
  m <- get_metrics(object)
  m[which(m$.metric == "accuracy"), ".estimate", drop = TRUE]
}

#' @rdname machine_learning
#' @export
get_kappa <- function(object) {
  m <- get_metrics(object)
  m[which(m$.metric == "kap"), ".estimate", drop = TRUE]
}

#' @rdname machine_learning
#' @export
get_recipe <- function(object) {
  attributes(object)$recipe
}

#' @rdname machine_learning
#' @export
get_specification <- function(object) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  object$spec
}

#' @rdname machine_learning
#' @export
get_rows_testing <- function(object) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  attributes(object)$rows_testing
}

#' @rdname machine_learning
#' @export
get_rows_training <- function(object) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  attributes(object)$rows_training
}

#' @rdname machine_learning
#' @export
get_original_data <- function(object) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  attributes(object)$data_original
}

#' @rdname machine_learning
#' @importFrom yardstick roc_curve
#' @export
get_roc_data <- function(object) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  model_prop <- attributes(object)
  if (all(c(".pred_TRUE", ".pred_FALSE") %in% colnames(model_prop$predictions))) {
    curve <- roc_curve(model_prop$predictions,
                       truth = truth,
                       ".pred_TRUE")
  } else {
    curve <- roc_curve(model_prop$predictions,
                       truth = truth,
                       starts_with(".pred"))
  }
  curve
}

#' @rdname machine_learning
#' @export
get_coefficients <- function(object) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  out <- object$fit$coefficients
  if (is.null(out)) {
    message("A model created with ", attributes(object)$properties$ml_function, "() does not have coefficients.")
  }
  out
}

#' @rdname machine_learning
#' @details Use the [get_model_variables()] function to return a zero-row [data.frame] with the variables that were used for training, even before the recipe steps.
#' @export
get_model_variables <- function(object) {
  vars <- get_recipe(object)$var_info$variable
  vars <- vars[vars != "outcome"]
  attributes(object)$data_original[0, vars, drop = FALSE]
}

#' @rdname machine_learning
#' @details Use the [get_variable_weights()] function to determine the (rough) estimated weights of each variable in the model. This is not as reliable as retrieving coefficients, but it does work for any model. The weights are determined by running the model over all the highest and lowest values of each variable in the trained data. The function returns a data set with 1 row, of which the values sum up to 1.
#' @importFrom dplyr select slice pull mutate across everything as_tibble
#' @export
get_variable_weights <- function(object) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  
  if (!is.null(object$fit$coefficients)) {
    message("This model contains coefficients. Use get_coefficients() to retrieve them.")
  }
  
  training_data <- attributes(object)$data_training
  # run prediction on NAs to get 'clean' certainty
  suppressWarnings(
    suppressMessages(
      empty_prediction <- object |> 
        apply_model_to(training_data,
                       # impossible to use MICE for imputation, it would say:
                       # `mice` detected constant and/or collinear variables. No predictors were left after their removal.
                       impute_algorithm = "single-point") |> 
        pull(certainty)
    )
  )
  
  # get min and max from trained variables (since they might be scaled and centred)
  ranges <- training_data |>
    select(-outcome) |>
    lapply(function(col) range(col, na.rm = TRUE)) |> 
    as.data.frame()
  # create empty data.frame to populate and predict
  ranges_new <- ranges |> 
    lapply(function(x) rep(NA_real_, ncol(ranges) * 2)) |>
    as.data.frame()
  rows <- 1
  for (i in seq_len(ncol(ranges))) {
    ranges_new[c(rows, rows + 1), i] <- ranges[, i, drop = TRUE]
    rows <- rows + 2
  }
  # run prediction using only each min and max for each variable
  suppressWarnings(
    suppressMessages(
      preds <- object |>
        # impossible to use MICE for imputation, it would say:
        # `mice` detected constant and/or collinear variables. No predictors were left after their removal.
        apply_model_to(ranges_new, impute_algorithm = "single-point") |>
        pull(certainty)
    )
  )
  
  # store differences with 'clean' prediction
  ind <- 1
  differences <- double(length = ncol(ranges))
  for (i in seq_len(ncol(ranges))) {
    differences[i] <- max(abs(preds[c(ind, ind + 1)] - empty_prediction), na.rm = TRUE)
    ind <- ind + 2
  }
  # transform differences as fraction of total sum
  differences <- differences / sum(differences, na.rm = TRUE)
  # give variable names
  names(differences) <- colnames(ranges)
  out <- differences |> as.list() |> as_tibble()
  
  model_vars <- colnames(get_model_variables(object))
  for (col in model_vars[!model_vars %in% colnames(out)]) {
    if (!is_xgboost(object)) {
      message("Note: Variable '", col, "' was in the recipe, but was excluded for training")
    }
    out[, col] <- 0
  }
  out <- out[, model_vars, drop = FALSE]
  out
}

#' @rdname machine_learning
#' @param only_params_in_model a [logical] to indicate whether only parameters in the model should be tuned
#' @inheritParams dials::grid_regular
#' @inheritParams rsample::vfold_cv
#' @param k The number of partitions of the data set
#' @details Use the [tune_parameters()] function to analyse tune parameters of any `ml_*()` function. Without any parameters manually defined, it will try to tune all parameters of the underlying ML model. The tuning will be based on a [K-fold cross-validation][rsample::vfold_cv()], of which the number of partitions can be set with `k`. The number of `levels` will be used to split the range of the parameters. For example, a range of 1-10 with `levels = 2` will lead to `[1, 10]`, while `levels = 5` will lead to `[1, 3, 5, 7, 9]`. The resulting [data.frame] will be sorted from best to worst. These results can also be plotted using [autoplot()].
#' @importFrom parsnip set_engine set_mode
#' @importFrom dials grid_regular
#' @importFrom workflows workflow add_model add_formula
#' @importFrom rsample vfold_cv
#' @importFrom tune tune_grid collect_metrics
#' @importFrom hardhat tune
#' @importFrom dplyr arrange desc across starts_with rename_with everything
#' @importFrom tidyr pivot_wider
#' @export
tune_parameters <- function(object, ..., only_params_in_model = FALSE, levels = 5, k = 10) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  
  model_prop <- attributes(object)
  dots <- list(...)
  
  # get the parsnip function and its parameters
  FUN <- eval(parse(text = model_prop$properties$ml_function))
  params <- names(formals(FUN))
  params <- params[!params %in% c("mode", "engine",
                                  # neural network models
                                  "activation", "dropout", "learn_rate",
                                  # extreme gradient boost trees
                                  "sample_size",
                                  # linear and logistic regression models
                                  "penalty", "mixture")]
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
    if (length(params) == 0) {
      message("No parameters to tune in ", model_prop$properties$ml_function, "().")
      return(invisible(NULL))
    }
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
  
  # create the K-fold cross-validation (also known as v-fold cross-validation)
  k_fold <- vfold_cv(model_prop$data_training, v = k)
  
  # show a message the prints the overview of all tuning values
  vals <- vapply(FUN.VALUE = character(1), tree_grid, function(x) paste0(trimws(format(unique(x), scientific = FALSE)), collapse = ", "))
  message("\nThese parameters will be tuned with these values:\n",
          paste0("  - ", names(vals), ": ", vals, collapse = "\n"))
  
  # run the tuning
  if (interactive()) {
    cat("\n")
    ans <- utils::menu(title = paste0("This will run a tuning analysis using a ",
                                      k, "-fold cross-validation for ", levels, "^", length(dials_fns), " = ",
                                      format(nrow(tree_grid), big.mark = ","), " combinations. Continue?"),
                       choices = c("Continue",
                                   "Return tree grid",
                                   "Return intended workflow",
                                   "Return K-fold cross-validation object",
                                   "Cancel"),
                       graphics = FALSE)
    if (ans == 1) {
      message("[", round(Sys.time()), "] Running tuning workflow...")
    } else if (ans == 2) {
      return(tree_grid)
    } else if (ans == 3) {
      return(tree_wf)
    } else if (ans == 4) {
      return(k_fold)
    } else {
      return(invisible(NULL))
    }
  } else {
    message("[", round(Sys.time()), "] Running tuning analysis using a ", k, "-fold cross-validation for ", nrow(tree_grid), " combinations...")
  }
  suppressWarnings(
    tree_res <- tree_wf |>
      tune_grid(resamples = k_fold,
                grid = tree_grid)
  )
  out <- tree_res |>
    collect_metrics() 
  message("[", round(Sys.time()), "] Done.")
  
  out <- out |> 
    pivot_wider(id_cols = -.estimator, names_from = .metric, values_from = c(mean, std_err))
  if (all(c("mean_accuracy", "mean_roc_auc") %in% colnames(out))) {
    out <- out |> 
      arrange(desc(mean_accuracy), desc(mean_roc_auc))
  } else {
    # arrange according the best metrics on average
    out <- out |> 
      arrange(desc(across(starts_with("mean_"))),
              across(everything()))
  }
  
  out <- out |> 
    rename_with(function(x) gsub("^mean_", "", x)) |>
    rename_with(function(x) gsub("^std_err_(.*)", "\\1_se", x))
  
  # return the results, 
  structure(out,
            class = c("certestats_tuning", class(out)),
            result = tree_res)
}

#' @rdname machine_learning
#' @details The [check_testing_predictions()] function combines the data used for testing from the original data with its predictions, so the original data can be reviewed per prediction.
#' @importFrom dplyr as_tibble slice select bind_cols
#' @export
check_testing_predictions <- function(object) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  out <- bind_cols(attributes(object)$predictions,
                   get_original_data(object) |>
                     slice(get_rows_testing(object))) |> 
    as_tibble()
  if (all(out$truth %in% c("TRUE", "FALSE", NA))) {
    out$truth <- as.logical(out$truth)
    out$predicted <- as.logical(out$predicted)
  }
  out
}

#' @method autoplot certestats_ml
#' @rdname machine_learning
#' @param plot_type the plot type, can be `"roc"` (default), `"gain"`, `"lift"` or `"pr"`. These functions rely on [yardstick::roc_curve()], [yardstick::gain_curve()], [yardstick::lift_curve()] and [yardstick::pr_curve()] to construct the curves.
#' @details Use [autoplot()] on a model to plot the receiver operating characteristic (ROC) curve, the gain curve, the lift curve, or the precision-recall (PR) curve. For the ROC curve, the (overall) area under the curve (AUC) will be printed as subtitle.
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
                      ".pred_TRUE")
    if (plot_type == "roc") {
      roc_auc <- roc_auc(model_prop$predictions,
                         truth = truth,
                         ".pred_TRUE")
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
      geom_path(linewidth = 0.75) +
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
        # autoplot is defined in the yardstick package:
        autoplot() +
        scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(x * 100, "%")) +
        scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x * 100, "%")) +
        labs(title = "Precision Recall (PR) Curve")
    )
    
  } else {
    p <- curve |>
      # autoplot is defined in the yardstick package:
      autoplot() +
      scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(x, "%")) +
      scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x, "%")) +
      labs(title = paste(tools::toTitleCase(plot_type), "Curve"))
    
  }
  
  if ("package:certeplot2" %in% search()) {
    p <- p +
      plot2::theme_minimal2() +
      certeplot2::scale_colour_certe_d()
  }
  
  p
}

#' @method autoplot certestats_feature_importances
#' @importFrom ggplot2 ggplot geom_col coord_flip labs theme element_line element_blank scale_fill_discrete
#' @importFrom tidyr pivot_longer
#' @importFrom certestyle colourpicker
#' @rdname machine_learning
#' @export
autoplot.certestats_feature_importances <- function(object, ...) {
  obj <- as.data.frame(object)
  obj$feature <- factor(obj$feature, levels = rev(obj$feature), ordered = TRUE)
  
  p <- ggplot(obj) +
    geom_col(aes(x = feature, y = importance)) +
    coord_flip() +
    labs(title = "Feature Importances",
         y = "",
         x = "",
         caption = if ("gain" %in% colnames(obj)) "Calculated as: 0.6 * Gain + 0.2 * Cover + 0.2 * Frequency" else NULL)
  
  if ("package:certeplot2" %in% search()) {
    p <- p +
      plot2::theme_minimal2() +
      scale_fill_discrete(type = colourpicker("certe", 3))
  }
  
  p <- p +
    theme(panel.grid.major.x = element_line(linewidth = 0.5),
          panel.grid.minor = element_line(linewidth = 0.25),
          panel.grid.major.y = element_blank())
  
  p
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
  
  if ("package:certeplot2" %in% search()) {
    p <- p +
      plot2::theme_minimal2() +
      certeplot2::scale_colour_certe_d()
  }
  p
}
