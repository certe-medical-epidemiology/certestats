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
#' Create a machine learning model based on different 'engines'. These function internally use the `tidymodels` packages by RStudio, which is the `tidyverse` variant for predictive modelling.
#' @param .data Data set to train
#' @param outcome Outcome variable to be used (the variable that must be predicted)
#' @param predictors Variables to use as predictors - these will be transformed using [as.double()]
#' @param training_fraction Fraction of rows to be used for *training*, defaults to 75%. The rest will be used for *testing*.
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
#' To predict **regression** (numeric values), any model can be used.
#'
#' To predict **classifications** (character values), the functions [ml_linear_regression()] and [ml_logistic_regression()] cannot be used.
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
#' model1 %>% yardstick::metrics()
#' model2 %>% yardstick::metrics()
#'
#' model1 %>% apply_model_to(iris)
#' model1 %>% ggplot2::autoplot()
#' model1 %>% caret::confusionMatrix()
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
          strata = strata,
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
          strata = strata,
          max_na_fraction = max_na_fraction,
          correlation_filter = correlation_filter,
          centre = centre,
          scale = scale,
          engine = engine,
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
                                   mode = "regression",
                                   penalty = 0.1,
                                   ...) {
  ml_exec(FUN = parsnip::logistic_reg,
          .data = .data,
          outcome = {{outcome}},
          predictors = {{predictors}},
          training_fraction = training_fraction,
          strata = strata,
          max_na_fraction = max_na_fraction,
          correlation_filter = correlation_filter,
          centre = centre,
          scale = scale,
          engine = engine,
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
          strata = strata,
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
          strata = strata,
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
          strata = strata,
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
              structure(ml_linear_regression, nm = "ml_linear_regression"),
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
                                      strata = strata,
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

#' @method autoplot certestats_ml_bootstrap
#' @rdname machine_learning
#' @importFrom ggplot2 autoplot
#' @param object a bootstrap object
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
    # ggplot2::geom_violin() +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(facets = "model") +
    ggplot2::labs(title = "Model Properties",
                  colour = "Property")
}

#' @importFrom dplyr `%>%` mutate select across filter everything bind_cols
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
  
  if (missing(predictors)) {
    stop("'predictors' is missing, use tidyselect code to choose them, such as 'where(is.double)'.")
  }
  
  properties <- c(list(FUN = deparse(substitute(FUN)),
                       engine = engine,
                       training_fraction = training_fraction),
                  list(...))
  
  # data opmaken om te kunnen verwerken
  df <- .data %>%
    mutate(outcome = {{outcome}}) %>%
    select(outcome, {{predictors}}) %>%
    # alle predictors forceren als double
    mutate(across({{ predictors }}, as.double)) %>%
    # kolommen verwijderen die niet aan max_na_fraction voldoen
    select(where(~sum(is.na(.)) / length(.) <= max_na_fraction)) %>%
    # rijen verwijderen die NAs hebben in outcome of predictor
    filter(across(everything(), ~ !is.na(.)))
  
  if (nrow(df) == 0) {
    stop("No more rows left for analysis (max_na_fraction = ", max_na_fraction, "). Check column values.", call. = FALSE)
  }
  
  df_split <- rsample::initial_split(df, strata = strata, prop = training_fraction)
  
  df_recipe <- df_split %>%
    rsample::training() %>%
    recipes::recipe(outcome ~ .)
  
  if (isTRUE(correlation_filter)) {
    df_recipe <- df_recipe %>% recipes::step_corr(recipes::all_predictors())
  }
  if (isTRUE(centre)) {
    df_recipe <- df_recipe %>% recipes::step_center(recipes::all_predictors(), -recipes::all_outcomes())
  }
  if (isTRUE(scale)) {
    df_recipe <- df_recipe %>% recipes::step_scale(recipes::all_predictors(), -recipes::all_outcomes())
  }
  df_recipe <- df_recipe %>%
    recipes::prep()
  
  # trainen
  df_training <- df_recipe %>%
    recipes::bake(new_data = NULL)
  
  # testen
  df_testing <- df_recipe %>%
    recipes::bake(rsample::testing(df_split))
  
  mdl <- FUN(...) %>%
    parsnip::set_engine(engine) %>%
    generics::fit(outcome ~ ., data = df_training)
  
  metrics <- mdl %>%
    stats::predict(df_testing) %>%
    bind_cols(df_testing) %>%
    yardstick::metrics(truth = outcome, estimate = .pred_class)
  
  structure(mdl,
            class = c("certestats_ml", class(mdl)),
            properties = properties,
            recipe = df_recipe,
            data_training = df_training,
            data_testing = df_testing,
            rows_training = sort(df_split$in_id),
            rows_testing = seq_len(nrow(df))[!seq_len(nrow(df)) %in% df_split$in_id],
            metrics = metrics,
            correlation_filter = correlation_filter,
            centre = centre,
            scale = scale)
}

#' @rdname machine_learning
#' @param ml_model Uitkomst van \code{\link{ml_random_forest}}.
#' @param data Nieuwe invoerdata die voorspelling nodig hebben.
#' @param only_prediction Standaard is \code{FALSE}. Alleen voorspelling retourneren, zonder kansen.
#' @export
apply_model_to <- function(ml_model, data, only_prediction = FALSE) {
  if (!inherits(ml_model, "certestats_ml")) {
    stop("Only output from certetools::ml_model() can be used.")
  }
  # data op zelfde manier transformeren als inputdata
  model_prop <- attributes(ml_model)
  
  # testen
  test_data <- recipes::bake(model_prop$recipe, new_data = data)
  
  cat("Machine learning model\n\n",
      paste0("- ", names(model_prop$properties), ": ", model_prop$properties, "\n"),
      "\n", sep = "")
  print(model_prop$recipe)
  cat("\n")
  
  out <- bind_cols(stats::setNames(stats::predict(ml_model, test_data), "predicted"),
                   stats::predict(ml_model, test_data, type = "prob"))
  
  if (isTRUE(only_prediction)) {
    out$predicted
  } else {
    out
  }
}

#' @importFrom caret confusionMatrix
#' @method confusionMatrix certestats_ml
#' @rdname machine_learning
#' @export
confusionMatrix.certestats_ml <- function(ml_model) {
  # sommige packages hebben al een confusion matrix, zoals randomForest:
  conf_mtrx <- attributes(ml_model)$model$fit$confusion
  # maar anders zelf maken:
  if (is.null(conf_mtrx)) {
    conf_mtrx <- ml_model %>%
      stats::predict(attributes(ml_model)$data_testing) %>%
      bind_cols(outcome = attributes(ml_model)$data_testing$outcome) %>%
      table()
  }
  conf_mtrx <- as.matrix(conf_mtrx)
  # niet meer kolommen dan rijen
  conf_mtrx <- conf_mtrx[, seq_len(NROW(conf_mtrx))]
  caret::confusionMatrix(conf_mtrx)
}

#' @importFrom yardstick metrics
#' @method metrics certestats_ml
#' @rdname machine_learning
#' @export
metrics.certestats_ml <- function(ml_model) {
  attributes(ml_model)$metrics
}

#' @method autoplot certestats_ml
#' @rdname machine_learning
#' @importFrom ggplot2 autoplot
#' @importFrom dplyr `%>%` bind_cols
#' @export
autoplot.certestats_ml <- function(ml_model) {
  model_prop <- attributes(ml_model)
  
  out <- ml_model %>%
    stats::predict(model_prop$data_testing, type = "prob") %>%
    bind_cols(model_prop$data_testing)
  
  pred <- colnames(out)[colnames(out) %like% ".pred"]
  if (length(pred) == 2) {
    pred <- pred[1L]
  }
  out %>%
    yardstick::gain_curve("outcome", pred) %>%
    autoplot()
}
