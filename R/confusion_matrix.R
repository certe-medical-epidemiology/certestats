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
#' Create a confusion matrix and calculate compatible `yardstick` metrics.
#' @param na.rm A [logical] to indicate whether missing values should be removed.
#' @inheritParams yardstick::f_meas
#' @inheritParams yardstick::mcc
#' @inheritParams yardstick::kap
#' @details
#' This is a function-agnostic `yardstick` wrapper: it discovers metric functions exported by `yardstick`, filters them by metric type compatible with the provided data (class / class-probability / numeric), and computes what is applicable.
#' @import yardstick
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr across all_of select everything mutate select all_of mutate bind_rows filter
#' @importFrom hardhat frequency_weights
#' @rdname confusion_matrix
#' @export
#' @examples
#' df <- tibble(name = c("Yes", "No"),
#'                       "Yes" = c(123, 26),
#'                       "No" = c(13, 834))
#' confusion_matrix(df)
confusion_matrix <- function(data, ...) {
  UseMethod("confusion_matrix")
}

# ---- helpers ---------------------------------------------------------------

.check_is_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package `", pkg, "` is required but not installed.", call. = FALSE)
  }
}

# Convert a "wide" confusion matrix data.frame (with a row-label column) to a table.
# Supports a 3-col format where first col is character and remaining two cols are counts,
# and also supports a typical "row label" + multiple numeric columns format.
.wide_df_to_table <- function(df) {
  if (!is.data.frame(df)) {
    stop("Internal error: expected data.frame.", call. = FALSE)
  }
  if (ncol(df) < 3) {
    stop("A wide confusion matrix data.frame must have >= 3 columns.", call. = FALSE)
  }
  row_lab <- df[[1]]
  if (!is.character(row_lab)) {
    stop("First column of wide confusion matrix must be character row labels.", call. = FALSE)
  }
  mat <- as.matrix(df[, -1, drop = FALSE])
  storage.mode(mat) <- "numeric"
  rn <- row_lab
  cn <- colnames(df)[-1]
  # If row labels contain prefixes like "Predict ", strip (best-effort).
  # rn2 <- trimws(gsub("^Predict\\s+", "", rn, ignore.case = TRUE))
  # cn2 <- trimws(gsub("^Actual\\s+", "", cn, ignore.case = TRUE))
  dimnames(mat) <- list(rn, cn)
  as.table(mat)
}

# Convert a square matrix/data.frame with dimnames to a table.
.square_to_table <- function(x) {
  mat <- as.matrix(x)
  storage.mode(mat) <- "numeric"
  if (is.null(rownames(mat)) || is.null(colnames(mat))) {
    # best-effort: use existing colnames for both if possible
    if (!is.null(colnames(mat))) {
      rownames(mat) <- colnames(mat)
    }
  }
  as.table(mat)
}

# Convert a table (confusion matrix counts) to a compact weighted data frame with
# columns: truth, estimate, .w
.table_to_weighted_df <- function(tab) {
  df <- as.data.frame(tab, stringsAsFactors = FALSE)
  # For a 2D table, as.data.frame gives columns Var1, Var2, Freq (names may vary)
  # We'll use the first two and the frequency column.
  if (ncol(df) < 3) {
    stop("Table input must be 2D (nrow == ncol).", call. = FALSE)
  }
  truth <- df[[2]]
  estimate <- df[[1]]
  freq <- df[[3]]
  
  out <- tibble(
    truth = factor(truth, levels = dimnames(tab)[[2]]),
    estimate = factor(estimate, levels = dimnames(tab)[[1]])
  )
  
  # case weights (preferred; avoids row replication)
  if (requireNamespace("hardhat", quietly = TRUE)) {
    out$.w <- frequency_weights(freq)
  } else {
    # fallback: replicate (can be large)
    out <- out[rep(seq_len(nrow(out)), freq), , drop = FALSE]
  }
  out
}

# Build a confusion matrix table attribute from a normalized df.
# Uses case weights if present.
.df_to_conf_table <- function(df) {
  if (!all(c("truth", "estimate") %in% names(df))) {
    return(NULL)
  }
  if (!is.factor(df$truth) || !is.factor(df$estimate)) {
    # confusion matrix is defined for class labels
    return(NULL)
  }
  
  if (".w" %in% names(df)) {
    # Prefer compact construction when case weights exist.
    # hardhat::frequency_weights() can be summed by coercing to numeric.
    w <- as.numeric(df$.w)
    tab <- stats::xtabs(w ~ truth + estimate, data = df)
    return(tab)
  }
  
  table(df$truth, df$estimate)
}

# Discover exported yardstick metric functions and classify by metric type.
# Returns a named list: name -> function, with an added attribute `metric_type`.
.discover_yardstick_metrics <- function() {
  ns <- asNamespace("yardstick")
  ex <- getNamespaceExports("yardstick")
  objs <- mget(ex, envir = ns, inherits = TRUE)
  fns <- Filter(is.function, objs)
  
  # Keep only metric functions created by yardstick constructors; they carry classes like
  # "class_metric", "class_prob_metric", "numeric_metric", etc.
  is_metric <- vapply(fns, function(fn) any(grepl("_metric$", class(fn))), logical(1))
  fns <- fns[is_metric]
  
  mtype <- vapply(fns, function(fn) {
    cls <- class(fn)
    if (any(cls == "class_prob_metric")) return("class_prob")
    if (any(cls == "class_metric")) return("class")
    if (any(cls == "numeric_metric")) return("numeric")
    if (any(cls == "survival_metric")) return("survival")
    if (any(cls == "dynamic_survival_metric")) return("dynamic_survival")
    if (any(cls == "ordered_prob_metric")) return("ordered_prob")
    if (any(cls == "ordered_metric")) return("ordered")
    "other"
  }, character(1))
  
  attr(fns, "metric_type") <- mtype
  fns
}

# Minimal, cached title lookup (best-effort). Falls back to prettified function name.
.metric_title_cache <- local({
  e <- new.env(parent = emptyenv())
  function(metric_name) {
    if (exists(metric_name, envir = e, inherits = FALSE)) {
      return(get(metric_name, envir = e, inherits = FALSE))
    }
    
    title <- NULL
    # Best-effort: read Rd title (can fail for internal aliases, etc.)
    # This is intentionally guarded and cached.
    try({
      rd <- utils::help(metric_name, package = "yardstick", help_type = "text")
      .getHelpFile <- get(".getHelpFile", envir = asNamespace("utils"))
      txt <- as.character(.getHelpFile(rd))
      end <- which(txt == "}")[1]
      if (!is.na(end)) {
        raw <- paste(txt[seq_len(end)], collapse = "")
        raw <- gsub("\\title{", "", raw, fixed = TRUE)
        raw <- gsub("}", "", raw, fixed = TRUE)
        raw <- gsub("[\n\t\r]+", " ", raw)
        raw <- trimws(raw)
        # "Detection prevalence" -> "Prevalence"
        raw <- trimws(gsub("\\bDetection\\b", "", raw, ignore.case = TRUE))
        title <- tools::toTitleCase(raw)
      }
    }, silent = TRUE)
    
    if (is.null(title) || !nzchar(title)) {
      # Fallback: title-case function name
      title <- tools::toTitleCase(gsub("_", " ", metric_name))
    }
    
    assign(metric_name, title, envir = e)
    title
  }
})

# Create a short abbreviation from a title (capital letters). Only used if informative.
.title_abbrev <- function(title) {
  ab <- gsub("[^A-Z]", "", title)
  if (nchar(ab) >= 3) ab else ""
}

# Safe metric call: only pass arguments that exist in the function formals.
# Supports class, class_prob, and numeric signatures.
.call_metric <- function(fn, nm, df, type, na.rm) {
  fn_df <- paste0(nm, ".data.frame")
  if (fn_df %in% ls(envir = asNamespace("yardstick"))) {
    fn <- get(fn_df, envir = asNamespace("yardstick"))
  }
  fmls <- names(formals(fn))
  
  # Prepare args list
  args <- list()
  args$data <- df
  
  # truth is always a column called "truth" in our normalized data
  if ("truth" %in% fmls) args$truth <- rlang::sym("truth")
  
  # case weights if available and supported
  if (".w" %in% names(df) && "case_weights" %in% fmls) {
    args$case_weights <- rlang::sym(".w")
  }
  
  if ("na_rm" %in% fmls) args$na_rm <- na.rm
  
  
  if (identical(type, "class")) {
    if (!("estimate" %in% names(df))) stop("Internal: missing estimate column.", call. = FALSE)
    if ("estimate" %in% fmls) args$estimate <- rlang::sym("estimate")
  } else if (identical(type, "class_prob")) {
    prob_cols <- names(df)[grepl("^\\.pred_", names(df)) & names(df) != ".pred_class"]
    if (length(prob_cols) == 0) stop("Internal: missing probability columns.", call. = FALSE)
    if ("estimate" %in% fmls) args$estimate <- all_of(prob_cols)
  } else if (identical(type, "numeric")) {
    if (!("estimate" %in% names(df))) stop("Internal: missing estimate column.", call. = FALSE)
    if ("estimate" %in% fmls) args$estimate <- rlang::sym("estimate")
  } else {
    stop("Internal: unsupported metric type.", call. = FALSE)
  }
  
  # Execute
  do.call(fn, args)
}

# Determine which metric types are compatible with the normalized data.
.compatible_metric_types <- function(df) {
  has_truth <- "truth" %in% names(df)
  if (!has_truth) return(character())
  
  truth_is_factor <- is.factor(df$truth)
  truth_is_numeric <- is.numeric(df$truth)
  
  has_estimate <- "estimate" %in% names(df)
  estimate_is_factor <- has_estimate && is.factor(df$estimate)
  estimate_is_numeric <- has_estimate && is.numeric(df$estimate)
  
  prob_cols <- names(df)[grepl("^\\.pred_", names(df)) & names(df) != ".pred_class"]
  has_prob <- length(prob_cols) > 0 && all(vapply(df[prob_cols], is.numeric, logical(1)))
  
  out <- character()
  
  if (truth_is_factor && estimate_is_factor) out <- c(out, "class")
  if (truth_is_factor && has_prob) out <- c(out, "class_prob")
  if (truth_is_numeric && estimate_is_numeric) out <- c(out, "numeric")
  
  unique(out)
}

# ---- methods ---------------------------------------------------------------

#' @rdname confusion_matrix
#' @export
confusion_matrix.default <- function(data,
                                     truth,
                                     estimate,
                                     na.rm = getOption("na.rm", FALSE),
                                     ...) {
  # 1) normalize inputs to a "df" suitable for yardstick, and (optionally) a confusion matrix table
  conf_tab <- NULL
  df <- NULL
  
  truth_quo <- rlang::enquo(truth)
  estimate_quo <- rlang::enquo(estimate)
  
  if (missing(data)) {
    if (rlang::quo_is_missing(truth_quo) || rlang::quo_is_missing(estimate_quo)) {
      stop("If `data` is not provided, both `truth` and `estimate` must be provided.", call. = FALSE)
    }
    # truth and estimate are vectors
    df <- tibble(
      truth = truth,
      estimate = estimate
    )
    # Prefer factor for class labels if possible; otherwise leave as-is.
    if (!is.numeric(df$truth) && !is.factor(df$truth)) df$truth <- as.factor(df$truth)
    if (!is.numeric(df$estimate) && !is.factor(df$estimate)) df$estimate <- as.factor(df$estimate)
    
  } else {
    # data provided: could be (a) table/matrix/square df, (b) wide confusion df, (c) long prediction df
    if (is.table(data)) {
      # validate 2D square
      d <- dim(data)
      if (length(d) != 2L || d[1] != d[2]) {
        stop("A `table` input must be 2D with nrow == ncol.", call. = FALSE)
      }
      conf_tab <- data
      df <- .table_to_weighted_df(conf_tab)
      
    } else if (is.matrix(data)) {
      if (length(dim(data)) != 2L || nrow(data) != ncol(data)) {
        stop("A matrix input must be square (nrow == ncol).", call. = FALSE)
      }
      conf_tab <- .square_to_table(data)
      df <- .table_to_weighted_df(conf_tab)
      
    } else if (is.data.frame(data)) {
      # Detect "wide confusion" format: first col character and remaining numeric-ish
      if (ncol(data) >= 3 && is.character(data[[1]]) &&
          all(vapply(data[-1], function(x) is.numeric(x) || is.integer(x), logical(1)))) {
        conf_tab <- .wide_df_to_table(data)
        # For a confusion table, orient as: rows = truth, cols = estimate.
        # Our wide parser builds rownames from first col and colnames from remaining,
        # which are typically estimate-by-truth. We will keep as-is and then swap when building df:
        # .table_to_weighted_df expects tab with dimnames [[1]] = estimate, [[2]] = truth (Var1/Var2 order).
        # To ensure truth in rows, estimate in cols, we transpose here to match expectation consistently.
        conf_tab <- t(conf_tab)
        df <- .table_to_weighted_df(conf_tab)
        
      } else {
        # Treat as regular predictions data.frame
        if (rlang::quo_is_missing(truth_quo) || rlang::quo_is_missing(estimate_quo)) {
          stop("For prediction data.frame input, `truth` and `estimate` must be provided.", call. = FALSE)
        }
        
        truth_nm <- rlang::as_name(rlang::ensym(truth))
        est_nm <- rlang::as_name(rlang::ensym(estimate))
        
        if (!(truth_nm %in% names(data))) stop("`truth` column not found in `data`.", call. = FALSE)
        if (!(est_nm %in% names(data))) stop("`estimate` column not found in `data`.", call. = FALSE)
        
        df <- as_tibble(data) |>
          select(!!truth_nm, !!est_nm, everything()) |>
          mutate(
            truth = .data[[truth_nm]],
            estimate = .data[[est_nm]]
          ) |>
          select(-all_of(c(truth_nm, est_nm)), truth, estimate)
        
        # Ensure truth/estimate are factors if they look like class labels (common case)
        if (!is.numeric(df$truth) && !is.factor(df$truth)) df$truth <- as.factor(df$truth)
        if (!is.numeric(df$estimate) && !is.factor(df$estimate)) df$estimate <- as.factor(df$estimate)
        
        conf_tab <- .df_to_conf_table(df)
      }
      
    } else {
      stop("Unsupported `data` type. Provide a data.frame, matrix, or table.", call. = FALSE)
    }
  }
  
  # 2) discover yardstick metrics and filter by compatible metric types
  metrics <- .discover_yardstick_metrics()
  mtype <- attr(metrics, "metric_type")
  
  compatible <- .compatible_metric_types(df)
  
  if (length(compatible) == 0) {
    stop(
      "No compatible yardstick metric types found for the provided inputs.\n",
      "Expected:\n",
      "  - class: truth factor + estimate factor\n",
      "  - class_prob: truth factor + .pred_<level> numeric columns\n",
      "  - numeric: truth numeric + estimate numeric",
      call. = FALSE
    )
  }
  
  keep <- mtype %in% compatible
  metrics <- metrics[keep]
  mtype <- mtype[keep]
  
  if (length(metrics) == 0) {
    stop("No exported `yardstick` metrics are compatible with the provided data.", call. = FALSE)
  }
  
  # 3) create pretty metric titles (cached) and deduplicate by title (case-insensitive)
  metric_names <- names(metrics)
  titles <- vapply(metric_names, .metric_title_cache, character(1))
  
  ord <- order(tolower(titles))
  metrics <- metrics[ord]
  mtype <- mtype[ord]
  metric_names <- metric_names[ord]
  titles <- titles[ord]
  
  dedup <- !duplicated(tolower(titles))
  metrics <- metrics[dedup]
  mtype <- mtype[dedup]
  metric_names <- metric_names[dedup]
  titles <- titles[dedup]
  
  # 4) run metrics, safely; do not coerce factor<->numeric to force metrics to run
  out <- tibble()
  use_progress <- requireNamespace("progress", quietly = TRUE)
  p <- NULL
  if (use_progress) {
    p <- progress::progress_bar$new(total = length(metrics))
  }
  
  for (i in seq_along(metrics)) {
    if (use_progress) p$tick()
    
    fn <- metrics[[i]]
    nm <- names(metrics)[i]
    type <- mtype[[i]]

    res <- tryCatch(
      .call_metric(fn, nm, df, type = type, na.rm = na.rm),
      error = function(e) {
        warning(conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    
    if (is.null(res)) {
      # Silence expected incompatibilities; message only for unexpected ones.
      # Expected errors are common for metrics that require particular estimators, event_level, etc.
      next
    }
    
    if (nrow(res) == 0) {
      next
    }
    
    # Attach a human-readable metric title for printing / downstream formatting
    # Include abbreviation only if it looks informative.
    ab <- .title_abbrev(titles[[i]])
    pretty <- if (nzchar(ab)) paste0(titles[[i]], " (", ab, ")") else titles[[i]]
    res <- mutate(res, .metric_name = pretty)
    out <- bind_rows(out, res)
  }
  
  if (NROW(out) > 1) {
    out <- filter(out, !is.na(.metric))
  }
  
  # 5) ensure confusion matrix table attribute exists when possible
  if (is.null(conf_tab)) conf_tab <- .df_to_conf_table(df)
  
  structure(
    out,
    data = conf_tab,
    class = c("certestats_confusion_matrix", class(out))
  )
}

#' @export
#' @importFrom cli cli_h1
print.certestats_confusion_matrix <- function(x, ...) {
  cli_h1("Confusion Matrix")
  tab <- attributes(x)$data
  if (is.null(tab)) {
    cat("(No confusion matrix table available for these inputs.)\n")
  } else {
    dimnames(tab) <- list(
      Actual = dimnames(tab)[[1]],
      Predicted = dimnames(tab)[[2]]
    )
    print(tab)
  }
  cat("\n")
  cli_h1("Model Metrics")
  cat("\n")
  
  if (nrow(x) == 0) {
    cat("(No compatible yardstick metrics could be computed.)\n")
    return(invisible(x))
  }
  
  # Print in a stable order: by .metric_name, then estimator if present
  df <- x
  if (".estimator" %in% names(df)) {
    df <- df[order(tolower(df$.metric_name), tolower(as.character(df$.estimator))), , drop = FALSE]
  } else {
    df <- df[order(tolower(df$.metric_name)), , drop = FALSE]
  }
  
  # round for printing
  est <- df$.estimate
  est_print <- suppressWarnings(ifelse(is.finite(est), round(est, 3), est))
  
  # include estimator column if present and not always identical
  if (".estimator" %in% names(df) && length(unique(as.character(df$.estimator))) > 1) {
    lines <- paste(format(df$.metric_name), format(as.character(df$.estimator)), format(est_print))
  } else {
    lines <- paste(format(df$.metric_name), format(est_print))
  }
  
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
