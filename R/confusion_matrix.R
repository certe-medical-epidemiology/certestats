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

#' Confusion Matrix Metrics and Interpretation
#'
#' Create a confusion matrix and calculate compatible `yardstick` metrics.
#' Supports class, class-probability, and numeric metrics. For multiclass classification
#' problems (`n_classes > 2`), this function computes both overall (macro/multiclass)
#' metrics and one-vs-rest (OvR) metrics per class label. It also add a human-readable
#' model interpretation.
#'
#' @param na.rm A [logical] indicating whether missing values should be removed.
#' @inheritParams yardstick::f_meas
#' @inheritParams yardstick::mcc
#' @inheritParams yardstick::kap
#'
#' @details
#' This is a function-agnostic wrapper around `yardstick`. It automatically discovers metric
#' functions exported by `yardstick`, filters them by compatibility with the input type
#' (class labels, class probabilities, or numeric regression), and applies all applicable metrics.
#'
#' For multiclass classification (i.e., where the truth and estimate are factors with more than
#' two levels), both macro/multiclass metrics and per-class OvR versions are computed.
#' Each per-class column treats the class as the positive label and all others as negative.
#'
#' In classification settings, this includes metrics such as accuracy, precision, recall,
#' specificity, F1, MCC, Kappa, and others, depending on compatibility.
#'
#' For binary classification or regression, output is unchanged and includes applicable metrics only.
#'
#' @import yardstick
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr across all_of select everything mutate select all_of mutate bind_rows filter
#' @importFrom hardhat frequency_weights
#'
#' @rdname confusion_matrix
#' @export
#'
#' @examples
#' # From aggregated counts
#' df <- data.frame(name = c("Yes", "No"),
#'                  Yes = c(123, 26),
#'                  No  = c(13, 834))
#' confusion_matrix(df)
#'
#' # From predictions on known labels
#' iris |>
#'   ml_decision_trees(Species, quiet = TRUE) |>
#'   confusion_matrix()

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
  conf_tab <- NULL
  df <- NULL
  
  truth_quo <- rlang::enquo(truth)
  estimate_quo <- rlang::enquo(estimate)
  
  if (missing(data)) {
    if (rlang::quo_is_missing(truth_quo) || rlang::quo_is_missing(estimate_quo)) {
      stop("If `data` is not provided, both `truth` and `estimate` must be provided.", call. = FALSE)
    }
    df <- tibble(
      truth = truth,
      estimate = estimate
    )
    if (!is.numeric(df$truth) && !is.factor(df$truth)) df$truth <- as.factor(df$truth)
    if (!is.numeric(df$estimate) && !is.factor(df$estimate)) df$estimate <- as.factor(df$estimate)
  } else {
    if (is.table(data)) {
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
      if (ncol(data) >= 3 && is.character(data[[1]]) &&
          all(vapply(data[-1], function(x) is.numeric(x) || is.integer(x), logical(1)))) {
        conf_tab <- .wide_df_to_table(data)
        conf_tab <- t(conf_tab)
        df <- .table_to_weighted_df(conf_tab)
      } else {
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
            truth = data[[truth_nm]],
            estimate = data[[est_nm]]
          ) |>
          select(-all_of(c(truth_nm, est_nm)), truth, estimate)
        if (!is.numeric(df$truth) && !is.factor(df$truth)) df$truth <- as.factor(df$truth)
        if (!is.numeric(df$estimate) && !is.factor(df$estimate)) df$estimate <- as.factor(df$estimate)
        conf_tab <- .df_to_conf_table(df)
      }
    } else {
      stop("Unsupported `data` type. Provide a data.frame, matrix, or table.", call. = FALSE)
    }
  }
  
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
    if (!is.null(res) && nrow(res) > 0) {
      ab <- .title_abbrev(titles[[i]])
      pretty <- if (nzchar(ab)) paste0(titles[[i]], " (", ab, ")") else titles[[i]]
      res <- mutate(res, .metric_name = pretty, .class = "overall")
      out <- bind_rows(out, res)
    }
  }
  
  # Add per-class OvR metrics if multiclass
  if (is.factor(df$truth) && length(levels(df$truth)) > 2) {
    cls <- levels(df$truth)
    for (k in cls) {
      df_k <- df |>
        mutate(
          truth = factor(truth == k, levels = c(FALSE, TRUE)),
          estimate = factor(estimate == k, levels = c(FALSE, TRUE))
        )
      for (i in seq_along(metrics)) {
        fn <- metrics[[i]]
        nm <- names(metrics)[i]
        type <- mtype[[i]]
        if (!identical(type, "class")) next
        res <- tryCatch(
          .call_metric(fn, nm, df_k, type = type, na.rm = na.rm),
          error = function(e) NULL
        )
        if (!is.null(res) && nrow(res) > 0) {
          ab <- .title_abbrev(titles[[i]])
          pretty <- if (nzchar(ab)) paste0(titles[[i]], " (", ab, ")") else titles[[i]]
          res <- mutate(res, .metric_name = pretty, .class = k)
          out <- bind_rows(out, res)
        }
      }
    }
  }
  
  if (NROW(out) > 1) {
    out <- filter(out, !is.na(.metric))
  }
  
  if (is.null(conf_tab)) conf_tab <- .df_to_conf_table(df)
  
  structure(
    out,
    data = conf_tab,
    class = c("certestats_confusion_matrix", class(out))
  )
}

#' @noRd
#' @export
#' @importFrom cli cli_h1
print.certestats_confusion_matrix <- function(x, ...) {
  
  # Confusion Matrix ----
  
  cli_h1("Confusion Matrix")
  tab <- attributes(x)$data
  if (is.null(tab)) {
    cat("(No confusion matrix table available for these inputs.)\n")
  } else {
    cat("\n")
    dimnames(tab) <- list(
      Actual = dimnames(tab)[[1]],
      Predicted = dimnames(tab)[[2]]
    )
    print(tab)
  }
  
  # Model Metrics ----
  
  df <- x
  metric_names <- unique(df$.metric_name)
  class_labels <- unique(df$.class)
  
  if (".estimator" %in% names(df)) {
    df <- df |> select(-.estimator) 
  }
  
  wide <- tidyr::pivot_wider(
    df,
    id_cols = .metric_name,
    names_from = .class,
    values_from = .estimate
  )
  
  wide <- wide[order(tolower(wide$.metric_name)), ]
  is_num <- vapply(wide, is.numeric, logical(1))
  wide[is_num] <- lapply(wide[is_num], function(x) ifelse(is.finite(x), round(x, 3), x))
  
  print_df <- as.data.frame(wide)
  print_df$.metric_name <- format(print_df$.metric_name, justify = "left")
  colnames(print_df)[1] <- " "
  
  cli_h1("Model Metrics")
  
  if (nrow(x) == 0) {
    cat("(No compatible metrics could be computed.)\n")
    return(invisible(x))
  }
  
  if (length(class_labels) > 1) {
    cat("\n")
  } else {
    colnames(print_df)[2] <- "  "
  }
  print(print_df, row.names = FALSE)
  
  # Model Interpretation ----
  
  cli_h1("Model Interpretation")
  cat("\n")
  .print_confusion_matrix_interpretation(x)
  invisible(x)
}



# ---- helpers ---------------------------------------------------------------

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

# Build a confusion matrix table attribute from a normalised df.
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
        # "F Measure" -> "F1 Score"
        raw <- gsub("F Measure", "F1 Score", raw, ignore.case = TRUE)
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
  
  # truth is always a column called "truth" in our normalised data
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

# Determine which metric types are compatible with the normalised data.
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

.print_confusion_matrix_interpretation <- function(x) {
  txt <- .confusion_matrix_interpretation(x)
  if (length(txt) == 0) return(invisible(NULL))
  cat(wrap_print(txt))
  invisible(NULL)
}

wrap_print <- function(x, width = getOption("width")) {
  if (length(x) == 0 || all(is.na(x))) return(character())
  
  x <- paste(x[!is.na(x)], collapse = "\n")
  x <- gsub("\r\n?", "\n", x)
  
  # split into paragraphs on blank lines
  paras <- strsplit(x, "\n[[:space:]]*\n", perl = TRUE)[[1]]
  paras <- trimws(paras)
  paras <- paras[nzchar(paras)]
  
  wrapped <- vapply(paras, FUN.VALUE = character(1), function(p) {
    # normalise whitespace inside paragraph
    p <- gsub("[[:space:]]+", " ", p)
    paste(strwrap(p, width = width), collapse = "\n")
  })
  
  paste(wrapped, collapse = "\n\n")
}

.confusion_matrix_interpretation <- function(x) {
  if (!inherits(x, "certestats_confusion_matrix") || nrow(x) == 0) {
    return(character())
  }
  
  m <- x
  get <- function(pat) {
    i <- grepl(pat, m$.metric_name, ignore.case = TRUE)
    if (!any(i)) return(NA_real_)
    m$.estimate[i][1]
  }
  pct <- function(v) ifelse(is.na(v), NA, round(100 * v, 1))
  
  acc  <- get("^Accuracy$")
  bal  <- get("Balanced Accuracy")
  rec  <- get("Recall|Sensitivity")
  prec <- get("Precision|PPV")
  spec <- get("Specificity")
  npv  <- get("Negative Predictive")
  f1   <- get("F1")
  kappa <- get("Kappa")
  mcc <- get("Matthews")
  
  conf_tab <- attr(x, "data")
  support <- if (!is.null(conf_tab)) rowSums(conf_tab) else NULL
  imbalance <- if (!is.null(support)) max(support) / min(support) else NA_real_
  
  out <- c()
  
  ## Global performance summary
  out <- c(out, .interp_overall_granular(acc, bal, pct))
  
  ## Agreement metrics
  if (!is.na(kappa) && !is.na(mcc)) {
    if (pct(kappa) == pct(mcc)) {
      out <- c(out, sprintf(
        "Agreement between predicted and true classes is strong (Cohen's Kappa and MCC are both %.1f%%). These account for chance agreement and are robust to class imbalance.",
        pct(kappa)
      ))
    } else {
      out <- c(out, sprintf(
        "Agreement between predicted and true classes is strong (Cohen's Kappa = %.1f%%, MCC = %.1f%%). These account for chance agreement and are robust to class imbalance.",
        pct(kappa),
        pct(mcc)
      ))
    }
  }
  
  ## Precision-recall trade-off
  out <- c(out, .interp_precision_recall_granular(prec, rec, pct))
  
  ## F1 Score (harmonic mean)
  if (!is.na(f1)) {
    out <- c(out, sprintf(
      "The macro-averaged F1 score is %.1f%%, indicating balanced harmonic performance across classes.",
      pct(f1)
    ))
  }
  
  ## Specificity / NPV
  out <- c(out, .interp_exclusion_granular(spec, npv, pct))
  
  ## Misclassification structure
  out <- c(out, .interp_confusion_structure(conf_tab))
  
  ## Class imbalance warning
  if (!is.null(imbalance) && imbalance > 1.5) {
    out <- c(out, sprintf(
      "Class imbalance is present (max:minor support ratio = %.2f). While macro-averaging mitigates this, some metrics may still overestimate performance on minority classes.",
      round(imbalance, 2)
    ))
  }
  
  ## Sparse error structure
  if (!is.null(conf_tab)) {
    sparse_errors <- sum(conf_tab != 0) < 0.5 * length(conf_tab)
    if (sparse_errors) {
      out <- c(out, "The confusion matrix is sparsely populated; many class pairs have zero observed errors. Interpret per-class metrics cautiously, as sparse data may inflate estimates.")
    }
  }
  
  ## High certainty warning (optional, when metrics are near-perfect)
  if ((!is.na(acc) && acc == 1) || (!is.na(bal) && bal == 1)) {
    out <- c(out, "Model performance is perfect, which is very suspicious. Consider cross-validation or external validation to avoid overfitting.")
  } else if (!is.na(acc) && acc > 0.975 && !is.na(bal) && bal > 0.975) {
    out <- c(out, "Model performance is near-perfect. Consider cross-validation or external validation to confirm generalisability and avoid overfitting.")
  }
  
  out[!is.na(out) & nzchar(out)]
}


.interp_overall_granular <- function(acc, bal, pct) {
  if (all(is.na(c(acc, bal)))) return(NA_character_)
  
  ref <- if (!is.na(bal)) bal else acc
  label <- cut(
    ref,
    breaks = c(-Inf, 0.65, 0.75, 0.85, 0.95, 0.9999, Inf),
    labels = c("unacceptable", "poor", "limited", "good", "very strong", "perfect, which might indicate overfitting"),
    right = FALSE
  )
  
  sprintf(
    "Overall performance is %s. Accuracy (%.1f%%) and balanced accuracy (%.1f%%) indicate %s separation between classes.",
    label,
    pct(acc),
    pct(bal),
    if (label %in% c("unacceptable", "poor", "limited")) "limited"
    else if (label == "good") "consistent"
    else if (grepl("perfect", label)) "perfect"
    else "highly consistent"
  )
}

.interp_precision_recall_granular <- function(prec, rec, pct) {
  if (any(is.na(c(prec, rec)))) return(NA_character_)
  
  diff <- prec - rec
  
  if (abs(diff) < 0.0001) {
    return(
      sprintf(
        "Precision and recall (both %.1f%%) are perfectly aligned, indicating an ideally balanced trade-off between false positives and missed true cases.",
        pct(prec)
      )
    )
  } else if (abs(diff) < 0.025) {
    return(
      sprintf(
        "Precision (%.1f%%) and recall (%.1f%%) are closely aligned, indicating a balanced trade-off between false positives and missed true cases.",
        pct(prec), pct(rec)
      )
    )
  }
  
  if (diff > 0) {
    sev <- cut(
      diff,
      breaks = c(0, 0.05, 0.15, Inf),
      labels = c("slightly", "moderately", "strongly"),
      right = FALSE
    )
    return(
      sprintf(
        "Precision (%.1f%%) exceeds recall (%.1f%%), meaning the model is %s conservative: predictions are usually correct, but true cases are missed.",
        pct(prec), pct(rec), sev
      )
    )
  }
  
  sev <- cut(
    -diff,
    breaks = c(0, 0.05, 0.15, Inf),
    labels = c("slightly", "moderately", "strongly"),
    right = FALSE
  )
  sprintf(
    "Recall (%.1f%%) exceeds precision (%.1f%%), meaning the model %s prioritises detecting true cases at the cost of more false positives.",
    pct(rec), pct(prec), sev
  )
}

.interp_exclusion_granular <- function(spec, npv, pct) {
  if (all(is.na(c(spec, npv)))) return(NA_character_)
  
  ref <- max(spec, npv, na.rm = TRUE)
  label <- cut(
    ref,
    breaks = c(-Inf, 0.70, 0.85, 0.95, Inf),
    labels = c("poor", "moderate", "strong", "very strong"),
    right = FALSE
  )
  
  sprintf(
    "The model's ability to rule out incorrect classes is %s, with specificity at %.1f%% and negative predictive value at %.1f%%.",
    label, pct(spec), pct(npv)
  )
}

.interp_confusion_structure <- function(tab) {
  if (is.null(tab) || (!is.matrix(tab) && !is.table(tab))) {
    return(NA_character_)
  }
  
  off <- tab
  diag(off) <- 0
  nz <- off[off > 0]
  
  if (length(nz) == 0) {
    return(
      "The confusion matrix shows no misclassifications; all observations were assigned to the correct class."
    )
  }
  
  if (length(nz) <= nrow(tab)) {
    return(
      "Most misclassifications are concentrated between a small number of class pairs, indicating overlap between specific categories rather than random error."
    )
  }
  
  "Misclassifications are distributed across multiple classes, suggesting broader overlap in feature patterns."
}
