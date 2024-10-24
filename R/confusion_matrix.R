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

#' Confusion Matrix Metrics
#' 
#' Create a confusion matrix and calculate its metrics. This function is an agnostic `yardstick` wrapper: it applies all `yardstick` functions that are metrics used for confusion matrices without internal hard-coded function names.
#' @inheritParams yardstick::mcc
#' @inheritParams yardstick::f_meas
#' @inheritParams yardstick::kap
#' @param na.rm a [logical] to indicate whether empty must be removed
#' @import yardstick
#' @inheritSection math_functions Default values of `na.rm`
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate select filter
#' @rdname confusion_matrix
#' @export
#' @examples
#' df <- tibble::tibble(name = c("Predict Yes", "Predict No"),
#'                      "Actual Yes" = c(123, 26),
#'                      "Actual No" = c(13, 834))
#' df
#' confusion_matrix(df)
confusion_matrix <- function(data, ...) {
  UseMethod("confusion_matrix")
}

#' @rdname confusion_matrix
#' @export
confusion_matrix.default <- function(data,
                                     truth,
                                     estimate,
                                     na.rm = getOption("na.rm", FALSE),
                                     ...) {
  check_is_installed("progress")
  if (missing(data)) {
    if (missing(truth) || missing(estimate)) {
      stop("If `data` is not provided, `truth` and `estimate` must be provided")
    }
    data <- NULL
  }
  data.bak <- data
  if (NCOL(data) == 3 && is.character(data[[1]])) {
    data <- data |> select(2:3) |> as.data.frame()
    rownames(data) <- colnames(data)
  }
  if (!is.null(data) && NROW(data) == NCOL(data)) {
    data <- data |> as.matrix() |> as.table()
  } else if (is.table(data)) {
    stop("A `table` must have nrow == ncol")
  } else if (is.null(data)) {
    data <- tibble(truth = truth, predicted = estimate)
    truth <- 1
    predicted <- 2
  }
  if (is.table(data)) {
    df <- as.data.frame(data)
    long_df <- df[rep(row.names(df), df$Freq), 1:2]
    rownames(long_df) <- NULL
    data <- long_df
    truth <- 1
    predicted <- 2
    colnames(data)[1] <- "truth"
    colnames(data)[2] <- ".pred_"
  }
  
  # do all metrics from the yardstick package!
  # every metric has a *_vec() variant, so look them up
  vecs <- ls(envir = asNamespace("yardstick"))
  vecs <- vecs[vecs %like% "_vec$"]
  fns <- gsub("_vec$", "", vecs)
  fns <- fns[fns %in% ls(envir = asNamespace("yardstick"))]
  fns_names <- character(length(fns))
  for (i in seq_len(length(fns_names))) {
    fns_names[i] <- get_function_title(fns[i], "yardstick")
  }
  
  # sort on name
  fns <- fns[order(tolower(fns_names))]
  fns_names <- fns_names[order(tolower(fns_names))]
  
  # deduplicate (e.g., spec() == specificity())
  fns <- fns[!duplicated(tolower(fns_names))]
  fns_names <- fns_names[!duplicated(tolower(fns_names))]
  
  out <- tibble()
  
  p <- progress::progress_bar$new(total = length(fns))
  
  for (i in seq_len(length(fns))) {
    p$tick()
    fn <- get(fns[i], envir = asNamespace("yardstick"))
    fn.data.frame <- tryCatch(get(paste0(fns[i], ".data.frame"), envir = asNamespace("yardstick")), error = function(e) NULL)
    fn.formals <- names(formals(ifelse(is.null(fn.data.frame), fn, fn.data.frame)))
    nm <- fns_names[i]
    nm_abbreviation <- gsub("[^A-Z]", "", nm)
    # paste capitals as abbreviation
    nm <- paste0(nm, ifelse(nchar(nm_abbreviation) > 2, paste0(" (", nm_abbreviation, ")"), ""))
    outcome <- NULL
    error <- FALSE
    error_message <- NULL
    tryCatch({
      outcome <- data |>
        fn(truth = truth, colnames(data)[colnames(data) %like% "^[.]pred_"][1])
    }, error = function(e) {
      if (inherits(e, "rlang_error")) {
        e$message <- paste0(rlang::cnd_header(e), rlang::cnd_body(e))
      }
      error_message <<- e$message
      if (paste0(e$message, collapse = "") %unlike% "(no applicable method|should be a|must match the number of)") {
        message("Function yardstick::", fns[i], "() cannot be computed: ", e$message)
      }
    })
    if (!is.null(error_message)) {
      # try again
      if (error_message %like% "should be a numeric") {
        tryCatch({
          outcome <- data |>
            mutate(across(colnames(data)[colnames(data) %like% "^[.]pred_"][1], as.double)) |> 
            fn(truth = truth, colnames(data)[colnames(data) %like% "^[.]pred_"][1], na_rm = na.rm)
        }, error = function(e) NULL)
        if (is.null(outcome)) {
          tryCatch({
            outcome <- data |>
              mutate(across(truth, as.double)) |> 
              fn(truth = truth, colnames(data)[colnames(data) %like% "^[.]pred_"][1], na_rm = na.rm)
          }, error = function(e) NULL)
        }
      } else if (error_message %like% "estimate.*should be a factor") {
        tryCatch({
          outcome <- data |>
            fn(truth = truth, predicted, na_rm = na.rm)
        }, error = function(e) NULL)
        if (is.null(outcome)) {
          tryCatch({
            outcome <- data |>
              mutate(across(truth, as.factor)) |> 
              fn(truth = truth, predicted, na_rm = na.rm)
          }, error = function(e) NULL)
        }
      }
      if (is.null(outcome)) {
        next
      }
    }
    if (NROW(outcome) == 0) {
      message("Function yardstick::", fns[i], "() produced 0 rows, so the ", fns_names[i], " is lacking")
    } else {
      # add it
      outcome <- outcome |> 
        mutate(.metric_name = nm)
      out <- out |> bind_rows(outcome)
    }
  }
  
  if (!is.table(data)) {
    data <- table(data[[1]], data[[2]])
  }
  
  out <- out |> filter(!is.na(.metric))
  
  structure(out,
            data = data,
            class = c("certestats_confusion_matrix", class(out)))
}

#' @export
print.certestats_confusion_matrix <- function(x, ...) {
  cat("Original data:\n\n")
  print(attributes(x)$data)
  cat("\n\nModel metrics:\n\n")
  cat(paste(format(x$.metric_name), format(round(x$.estimate, 3))), sep = "\n")
}

get_function_title <- function(fun, pkg) {
  rd <- utils::help(fun, package = eval(pkg), help_type = "text")
  .getHelpFile <- get(".getHelpFile", envir = asNamespace("utils"))
  txt <- as.character(.getHelpFile(rd))
  title <- paste(txt[seq_len(which(txt == "}")[1])], collapse = "")
  title <- gsub("\\title{", "", title, fixed = TRUE)
  title <- gsub("}", "", title, fixed = TRUE)
  title <- gsub("[\n\t\r]+", " ", title)
  # 'Detection prevalence' should just be 'prevalence'
  title <- trimws(gsub("Detection", "", title, ignore.case = TRUE))
  tools::toTitleCase(title)
}
