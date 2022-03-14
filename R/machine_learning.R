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
#' @param outcome Outcome variable to be used (the variable that must be predicted). In case of classification prediction, this variable will be coerced to a [factor].
#' @param predictors Variables to use as predictors - these will be transformed using [as.double()]
#' @param training_fraction Fraction of rows to be used for *training*, defaults to 75%. The rest will be used for *testing*. If given a number over 1, the number will be considered to be the required number of rows for *training*.
#' @param strata Groups to consider in the model (i.e., variables to stratify by)
#' @param correlation_filter A [logical] to indicate whether the `predictors` should be removed that have to much correlation with each other, using [recipes::step_corr()]
#' @param centre A [logical] to indicate whether the `predictors` should be transformed so that their mean will be `0`, using [recipes::step_center()]
#' @param scale A [logical] to indicate whether the `predictors` should be transformed so that their standard deviation will be `1`, using [recipes::step_scale()]
#' @param max_na_fraction Maximum fraction of `NA` values (defaults to `0.01`) of the `predictors` before they are removed from the model
#' @param mode Type of predicted value - defaults to `"classification"`, but can also be `"unknown"` or `"regression"`
#' @param engine \R package or function name to be used for the model, will be passed on to [parsnip::set_engine()]
#' @param ... Arguments to be passed on to the `parsnip` functions, see *Model Functions*
#' @param times Number of times to run the model(s), defaults to `25`
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
#' ```
#'                        .data
#'                          |
#'                rsample::initial_split()
#'                      /        \
#'      rsample::training()   rsample::testing()
#'               |                |
#'         recipe::recipe()       |
#'              |                 |
#'        recipe::step_corr()     |
#'              |                 |
#'       recipe::step_center()    |
#'              |                 |
#'       recipe::step_scale()     |
#'              |                 |
#'          recipe::prep()        |
#'          /            \        |
#' recipes::bake()        recipes::bake()
#'        |                       |
#' generics::fit()       yardstick::metrics()
#'        |                       |
#'     output            attributes(output)
#' ```
#' 
#' Use [autoplot()] on a model to plot the receiver operating characteristic (ROC) curve, showing the relation between sensitivity and specificity. This plotting function uses [yardstick::roc_curve()] to construct the curve.
#' 
#' @section Attributes:
#' The `ml_*()` functions return the following [attributes][base::attributes()]:
#' 
#' * `properties`: a [list] with model properties: the ML function, engine package, training size, testing size, strata size, mode, and the different ML function-specific properties (such as `tree_depth` in [ml_decision_trees()])
#' * `recipe`: a [recipe][recipes::recipe()] as generated with [recipes::prep()], to be used for training and testing
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
#' model1 <- iris %>% ml_random_forest(Species, where(is.double))
#' model2 <- iris %>% ml_decision_trees(Species, where(is.double))
#' model3 <- iris %>% ml_neural_network(Species, where(is.double))
#'
#' model1 %>% metrics()
#' model2 %>% metrics()
#'
#' model1 %>% apply_model_to(iris)
#' 
#' model3 %>% autoplot()
#' model3 %>% autoplot(plot_type = "gain")
#' 
#' # confusion matrix of the model (trained data)
#' model1 %>% confusionMatrix()
#' 
#' # confusion model of applying to a different data set
#' table(iris$Species,
#'       apply_model_to(model1, iris, TRUE)) %>% 
#'   confusionMatrix()
#'   
#' \dontrun{
#' esbl %>%
#'   ml_random_forest(esbl_interpr,
#'                    betalactams()) %>%
#'   apply_model_to(esbl[1:10, ])
#' }
ml_decision_trees <- function(.data,
                              outcome,
                              predictors,
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
                                 predictors,
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
                                   predictors,
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
                              predictors,
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
                                 predictors,
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
                             predictors,
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

#' @rdname machine_learning
#' @importFrom caret confusionMatrix
#' @importFrom cleaner format_names
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr `%>%` mutate select everything arrange
#' @importFrom progress progress_bar
#' @export
bootstrap_ml <- function(.data,
                         outcome,
                         predictors,
                         times = 25,
                         training_fraction = 3/4,
                         strata = NULL,
                         max_na_fraction = 0.01,
                         correlation_filter = TRUE,
                         centre = TRUE,
                         scale = TRUE,
                         mode = c("classification", "regression")) {
  
  
  funs <- list(structure(ml_decision_trees, nm = "ml_decision_trees"),
               structure(ml_neural_network, nm = "ml_neural_network"),
               structure(ml_nearest_neighbour, nm = "ml_nearest_neighbour"),
               structure(ml_random_forest, nm = "ml_random_forest"))
  if (mode[1L] == "regression") {
    funs <- c(funs,
              structure(ml_linear_regression, nm = "ml_linear_regression"))
  } else if (mode[1L] == "classification") {
    funs <- c(funs,
              structure(ml_logistic_regression, nm = "ml_logistic_regression"))
  }
  
  n <- times * length(funs)
  pb <- progress_bar$new(
    format = ":elapsedfull [:bar] :current/:total :eta",
    total = n, clear = FALSE)
  
  df <- NULL
  
  for (i in seq_len(times)) {
    sapply(funs,
           function(f) {
             pb$tick()
             
             t_start <- as.numeric(Sys.time()) * 1000
             suppressWarnings(suppressMessages(
               m <- confusionMatrix(f(.data = .data,
                                      outcome = {{outcome}},
                                      predictors = {{predictors}},
                                      training_fraction = training_fraction,
                                      strata = {{strata}},
                                      max_na_fraction = max_na_fraction,
                                      correlation_filter = correlation_filter,
                                      centre = centre,
                                      scale = scale,
                                      mode = mode[1L]))
             ))
             t_diff <- (as.numeric(Sys.time()) * 1000) - t_start
             to_merge <- m$byClass %>%
               as.data.frame() %>%
               rownames_to_column("Class") %>%
               mutate(model = attributes(f)$nm,
                      t_diff = t_diff)
             if (is.null(df)) {
               df <<- to_merge
             } else {
               df <<- rbind(df, to_merge)
             }
           })
  }
  
  df <- df %>%
    format_names(snake_case = TRUE) %>%
    select(model, class, everything()) %>%
    arrange(model, class)
  
  structure(df, class = c("certestats_ml_bootstrap", "data.frame"))
}

#' @importFrom dplyr `%>%` mutate select across filter_all bind_cols
#' @importFrom yardstick metrics
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
  
  # format data to work with it
  df <- .data %>%
    mutate(outcome = {{outcome}},
           strata = {{strata}}) %>%
    select(outcome, {{predictors}}, strata) %>%
    # force all predictors as double
    mutate(across({{ predictors }}, as.double)) %>%
    # remove columns that do not comply to max_na_fraction
    select(where(~sum(is.na(.)) / length(.) <= max_na_fraction)) %>%
    # remove rows that have NA in outcome or predictors
    filter_all(~!is.na(.))

  # the outcome variable must be factor in case of regression prediction
  if (list(...)$mode == "classification" && !is.factor(df$outcome)) {
    if (is.logical(df$outcome)) {
      df$outcome <- factor(df$outcome, levels = c(TRUE, FALSE))
    } else {
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
  
  df_split <- rsample::initial_split(df, strata = strata_var, prop = training_fraction)
  
  df_recipe <- df_split %>%
    rsample::training() %>%
    recipes::recipe(outcome ~ .)
  
  if (isTRUE(correlation_filter)) {
    df_recipe <- df_recipe %>% recipes::step_corr(recipes::all_predictors(), -strata_var)
  }
  if (isTRUE(centre)) {
    df_recipe <- df_recipe %>% recipes::step_center(recipes::all_predictors(), -recipes::all_outcomes(), -strata_var)
  }
  if (isTRUE(scale)) {
    df_recipe <- df_recipe %>% recipes::step_scale(recipes::all_predictors(), -recipes::all_outcomes(), -strata_var)
  }
  df_recipe <- df_recipe %>%
    recipes::prep()
  
  # train
  df_training <- df_recipe %>%
    recipes::bake(new_data = NULL)
  
  # test
  df_testing <- df_recipe %>%
    recipes::bake(rsample::testing(df_split))
  
  mdl <- FUN(...) %>%
    parsnip::set_engine(engine) %>%
    generics::fit(outcome ~ ., data = df_training)
  
  metrics <- mdl %>%
    stats::predict(df_testing) %>%
    bind_cols(df_testing) %>% 
    metrics(truth = outcome, estimate = colnames(.)[1])
  
  if (properties$mode == "classification") {
    prediction <- stats::predict(mdl, df_testing, type = "prob") %>%
      mutate(max = row_function(max, data = .))
  } else {
    prediction <- stats::predict(mdl, df_testing, type = "numeric")
  }
  pred_outcome <- stats::predict(mdl, df_testing)
  colnames(pred_outcome) <- "predicted"
  prediction <- bind_cols(prediction, pred_outcome)
  
  structure(mdl,
            class = c("certestats_ml", class(mdl)),
            properties = properties,
            recipe = df_recipe,
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
    cat("\nClassification reliability (based on testing data):\n")
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
#' @param only_prediction a [logical] to indicate whether predictions must be returned as [vector], otherwise returns a [data.frame] with reliabilities of the predictions
#' @importFrom dplyr select all_of mutate across everything
#' @export
apply_model_to <- function(object, new_data, only_prediction = FALSE) {
  if (!inherits(object, "certestats_ml")) {
    stop("Only output from certestats::ml_*() functions can be used.")
  }
  # test - transform data according to recipe
  training_cols <- colnames(attributes(object)$data_training)
  training_cols <- training_cols[training_cols != "outcome"]
  if (!all(training_cols %in% colnames(new_data))) {
    stop("These columns are in the training data, but not in the new data: ",
         paste0(training_cols[!training_cols %in% colnames(new_data)], collapse = ", "))
  }
  new_data <- new_data %>% 
    select(all_of(training_cols)) %>% 
    mutate(across(everything(), as.double))
  test_data <- recipes::bake(attributes(object)$recipe, new_data = new_data)
  
  if (isTRUE(only_prediction)) {
    stats::predict(object, test_data)[[1]]
  } else {
    bind_cols(stats::setNames(stats::predict(object, test_data), "predicted"),
              stats::predict(object, test_data, type = "prob"))
  }
}

#' @importFrom caret confusionMatrix
#' @method confusionMatrix certestats_ml
#' @rdname machine_learning
#' @export
confusionMatrix.certestats_ml <- function(data, ...) {
  # some package already have a confusion matrix, such as 'randomForest':
  conf_mtrx <- attributes(data)$model$fit$confusion
  # create it self otherwise:
  if (is.null(conf_mtrx)) {
    conf_mtrx <- data %>%
      stats::predict(attributes(data)$data_testing) %>%
      bind_cols(outcome = attributes(data)$data_testing$outcome) %>%
      table()
  }
  conf_mtrx <- as.matrix(conf_mtrx)
  # not more columns than rows
  conf_mtrx <- conf_mtrx[, seq_len(NROW(conf_mtrx))]
  caret::confusionMatrix(conf_mtrx)
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
#' @importFrom ggplot2 autoplot ggplot aes geom_path geom_abline coord_equal scale_x_continuous scale_y_continuous labs element_line
#' @importFrom dplyr `%>%` bind_cols starts_with
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
    curve <- curve_fn(model_prop$predictions, truth = predicted, 1)
    if (plot_type == "roc") {
      roc_auc <- roc_auc(model_prop$predictions, truth = predicted, 1)
    }
  } else {
    curve <- curve_fn(model_prop$predictions,
                      truth = predicted,
                      starts_with(".pred"))
    if (plot_type == "roc") {
      roc_auc <- roc_auc(model_prop$predictions,
                         truth = predicted,
                         starts_with(".pred"))
    }
  }
  
  if (plot_type == "roc") {
    if (".level" %in% colnames(curve) &&
        !all(c(".pred_TRUE", ".pred_FALSE") %in% colnames(model_prop$predictions))) {
      p <- ggplot(curve, aes(x = 1 - specificity, y = sensitivity, colour = .level))
    } else {
      p <- ggplot(curve, aes(x = 1 - specificity, y = sensitivity))
    }
    p <- p +
      geom_path(size = 1) +
      geom_abline(lty = 3) +
      coord_equal() +
      scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(x * 100, "%")) +
      scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x * 100, "%")) +
      labs(title = "Receiver Operating Characteristic (ROC) Curve",
           subtitle = paste0("Area Under Curve (AUC): ", round(roc_auc$.estimate, digits = 3)),
           colour = "Outcome")
    
  } else if (plot_type == "pr") {
    suppressMessages(
      p <- curve %>%
        # thse is defined in the yardstick package:
        autoplot() +
        scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(x * 100, "%")) +
        scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x * 100, "%")) +
        labs(title = "Precision Recall (PR) Curve")
    )
    
  } else {
    p <- curve %>%
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

#' @method autoplot certestats_ml_bootstrap
#' @rdname machine_learning
#' @importFrom ggplot2 autoplot
#' @param all_cols Include all columns, not only sensitivity, specificity, pos_pred_value, neg_pred_value.
#' @importFrom dplyr `%>%` select filter mutate across
#' @importFrom tidyr pivot_longer
#' @export
autoplot.certestats_ml_bootstrap <- function(object, all_cols = FALSE, ...) {
  
  if (!isTRUE(all_cols)) {
    object <- object %>%
      select(model, class, t_diff, sensitivity, specificity, pos_pred_value, neg_pred_value)
  }
  
  markup <- function(x) {
    x %>%
      gsub("^Class: ", "", .) %>%
      gsub("^ml_", "", .) %>%
      gsub("_", " ", .) %>%
      tools::toTitleCase()
  }
  
  object %>%
    pivot_longer(-c(model, class, t_diff)) %>%
    filter(!is.na(value)) %>%
    mutate(across(c(model, class, name), markup)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = class,
                                           y = value,
                                           colour = name)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(facets = "model") +
    ggplot2::labs(title = "Model Properties",
                  colour = "Property")
}
