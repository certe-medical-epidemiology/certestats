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

#' Impute: Filling Missing Values
#' 
#' Imputation is the process of replacing missing data with substituted values. This is done because of three main problems that missing data causes: missing data can introduce a substantial amount of bias, make the handling and analysis of the data more arduous, and create reductions in efficiency.
#' @param .data data set with missing values to impute
#' @param vars variables of `.data` that must be imputed, defaults to [`everything()`][tidyselect::everything()] and supports the `tidyselect` language.
#' @param algorithm algorithm to use for imputation, must be `"mice"` or `"single-point"`. For the latter, `FUN` must be given.
#' @param m number of multiple imputations if using MICE, see [mice::mice()]. The mean of all imputations will be used as result.
#' @param method method to use if using MICE, see [mice::mice()]
#' @param FUN function to use for single-point imputation (directly) or for MICE to summarise the results over all `m` iterations
#' @param info print info about imputation
#' @param ... arguments to pass on to [mice::mice()]
#' @details 
#' Imputation can be done using single-point, such as the mean or the median, or using [multivariate imputation by chained equations][mice::mice()] (MICE). Using MICE is a lot more reliable, but also a lot slower, than single-point imputation.
#' 
#' The suggested and default method is MICE. The generated MICE object will be stored as an [attribute][attributes()] with the data, and can be retrieved with [get_mice()], containing all specifics about the imputation. MICE is also known as *fully conditional specification* and *sequential regression multiple imputation*. It was designed for data with randomly missing values, though there is simulation evidence to suggest that with a sufficient number of auxiliary variables it can also work on data that are missing not at random. 
#' 
#' Use [is_imputed()] to get a [data.frame] with `TRUE`s for all values that were imputed.
#' @importFrom dplyr mutate across everything cur_column group_by summarise all_of select as_tibble
#' @importFrom tidyr complete
#' @rdname impute
#' @export
#' @examples 
#' iris2 <- dplyr::as_tibble(iris)
#' iris2[3, 2] <- NA
#' iris2[3, 9] <- NA
#' iris
#' iris2
#' 
#' result <- iris2 |> impute()
#' result
#'   
#' iris2 |> impute(algorithm = "single-point")
#' iris2 |>
#'   impute(vars = starts_with("Sepal"),
#'          algorithm = "single-point")
#' iris2 |>
#'   impute(vars = where(is.double),
#'          algorithm = "single-point",
#'          FUN = median)
#'   
#' result |> is_imputed()
#' result |> get_mice()
impute <- function(.data,
                   vars = everything(),
                   algorithm = "mice",
                   m = 10,
                   method = NULL,
                   FUN = mean,
                   info = TRUE,
                   ...) {
  
  algorithm <- tolower(algorithm[1L])
  
  cols_to_impute <- .data |> 
    select({{ vars }}) |> 
    colnames()
  cols_with_na <- vapply(FUN.VALUE = logical(1),
                         .data,
                         function(x) any(is.na(x)))
  cols_with_na <- names(cols_with_na[cols_with_na])
  cols_to_impute <- intersect(cols_with_na, cols_to_impute)
  if (length(cols_to_impute) == 0) {
    if (isTRUE(info)) {
      message("No missing values to impute, returning original data set.")
    }
    return(.data)
  }
  impute_selection <- as.data.frame(is.na(.data))
  for (col in colnames(impute_selection)) {
    if (!col %in% cols_to_impute) {
      impute_selection[, col] <- FALSE
    }
  }
  
  if (algorithm == "mice") {
    check_is_installed("mice")
    if (m < 1) {
      m <- 1
    }
    if (isTRUE(info)) {
      message("Generating MICE using m = ", m, " multiple imputations... ", appendLF = FALSE)
    }
    imp <- .data |>
      mice::mice(...,
                 where = as.matrix(impute_selection),
                 m = m,
                 method = method,
                 print = FALSE)
    imputed_cols <- vapply(FUN.VALUE = logical(1),
                           as.data.frame(imp$where),
                           function(col) any(col, na.rm = TRUE))
    imputed_cols <- names(imputed_cols[imputed_cols])
    completed <- imp |>
      complete(action = "long") |> 
      group_by(.id) |>
      summarise(across(all_of(imputed_cols),
                       function(col, fn = FUN, print_info = info) {
                         if (is.numeric(col) && !is.factor(col)) {
                           # take the mean of all multiple imputations
                           FUN(col[!is.na(col)])
                         } else {
                           # take the mode
                           names(sort(table(col), decreasing = TRUE)[1])
                         }
                       }), .groups = "drop") |> 
      select(-`.id`)

    if (isTRUE(info)) {
      message("OK.")
    }
    for (col in imputed_cols) {
      if (isTRUE(info)) {
        message("Imputed variable '", col, "' using MICE (method: ", get_mice_method(imp$method[names(imp$method) == col]),
                ") in row ", paste(which(is.na(.data[, col, drop = TRUE])), collapse = ", "))
      }
    }
    for (col in colnames(completed)) {
      .data[, col] <- completed[, col, drop = TRUE]
    }
    structure(.data,
              imputed = impute_selection,
              impute_algorithm = paste0("MICE, run get_mice() for the MICE object"),
              mice = imp,
              class = c("imputed", class(.data)))
    
  } else if (algorithm %like% "single") {
    fn_deparse <- deparse(substitute(FUN))
    
    imp <- .data |> 
      mutate(across(all_of(cols_to_impute),
                    function(col, fn = FUN, print_info = info, fn_txt = fn_deparse) {
                      if (is.numeric(col) && !is.factor(col)) {
                        # calculate the single point arithmetic value
                        outcome <- fn(col[!is.na(col)])
                        if (isTRUE(print_info)) {
                          message("Imputed variable '", cur_column(), "' using single ", fn_txt, 
                                  " of ", round(outcome, 2), " in row ", paste(which(is.na(col)), collapse = ", "))
                        }
                        col[is.na(col)] <- outcome
                      } else {
                        # take the most common value
                        outcome <- names(sort(table(col[is.na(col)]), decreasing = TRUE)[1])
                        if (isTRUE(print_info)) {
                          message("Imputed variable '", cur_column(), "' using the modal value \"", outcome, "\"", 
                                  " in row ", paste(which(is.na(col)), collapse = ", "))
                        }
                        col[is.na(col)] <- outcome
                      }
                      col
                    }))
    structure(imp,
              imputed = impute_selection,
              impute_algorithm = paste0("Single-point (", fn_deparse, ")"),
              class = c("imputed", class(imp)))
    
  } else {
    stop("Invalid algorithm. Must be \"mice\" or \"single-point\".")
  }
}

#' @rdname impute
#' @importFrom dplyr mutate across everything as_tibble
#' @export
is_imputed <- function(.data) {
  imp <- attributes(.data)$imputed
  alg <- attributes(.data)$impute_algorithm
  if (is.null(imp)) {
    message("This data set was not imputed using certestats::impute()")
    .data |>
      mutate(across(everything(), ~FALSE)) |>
      as_tibble()
  } else {
    message("Imputation algorithm: ", alg)
    as_tibble(imp)
  }
}

#' @rdname impute
#' @export
get_mice <- function(.data) {
  attributes(.data)$mice
}

#' @method print imputed
#' @noRd
#' @export
print.imputed <- function(x, ...) {
  total <- suppressMessages(sum(unlist(is_imputed(x), use.names = FALSE)))
  class(x) <- class(x)[class(x) != "imputed"]
  print(x, ...)
  cat(paste0("NOTE: This data set contains ", total, " imputed value", ifelse(total == 1, "", "s"),
             ". Use is_imputed() for details.\n"))
}

get_mice_method <- function(x) {
  mice_methods <- c(
    `pmm` = "Predictive mean matching",
    `midastouch` = "Weighted predictive mean matching",
    `sample` = "Random sample from observed values",
    `cart` = "Classification and regression trees",
    `rf` = "Random forest imputations",
    `mean` = "Unconditional mean imputation",
    `norm` = "Bayesian linear regression",
    `norm.nob` = "Linear regression ignoring model error",
    `norm.boot` = "Linear regression using bootstrap",
    `norm.predict` = "Linear regression, predicted values",
    `lasso.norm` = "Lasso linear regression",
    `lasso.select.norm` = "Lasso select + linear regression",
    `quadratic` = "Imputation of quadratic terms",
    `ri` = "Random indicator for nonignorable data",
    `logreg` = "Logistic regression",
    `logreg.boot` = "Logistic regression with bootstrap",
    `lasso.logreg` = "Lasso logistic regression",
    `lasso.select.logreg` = "Lasso select + logistic regression",
    `polr` = "Proportional odds model",
    `polyreg` = "Polytomous logistic regression",
    `lda` = "Linear discriminant analysis",
    `2l.norm` = "Level-1 normal heteroscedastic",
    `2l.lmer` = "Level-1 normal homoscedastic, lmer",
    `2l.pan` = "Level-1 normal homoscedastic, pan",
    `2l.bin` = "Level-1 logistic, glmer",
    `2lonly.mean` = "Level-2 class mean",
    `2lonly.norm` = "Level-2 class normal",
    `2lonly.pmm` = "Level-2 class predictive mean matching"
  )
  tolower(unname(mice_methods[match(x, names(mice_methods))]))
}
