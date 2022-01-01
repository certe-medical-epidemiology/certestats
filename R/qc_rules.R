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

#' Quality Control (QC) Rules
#'
#' These rules are used for quality control (QC). Default values are set for **Nelson's QC rules**, but they also support Westgard, AIAG, Montgomery and Healthcare QC rules.
#' @param x vector with values
#' @param m mean
#' @param s standard deviation
#' @param threshold minimal number of sequential values before rule is triggered (defaults to Nelson's)
#' @param direction_mean a logical to indicate whether *n* observations in a row must be tested for alternating in direction of the mean
#' @param rule number of the rule
#' @section Rules list:
#' 
#' | Rule | Rule explanation:                                                      | Nelson | Westgard | AIAG | Montg. | HC |
#' |:----:|:-----------------------------------------------------------------------|:------:|:--------:|:----:|:------:|:--:|
#' |  #1  | One point is more than 3 standard deviations from the mean             |    1   |     1    |   1  |    1   |  1 |
#' |  #2  | `n` (or more) points in a row are on the same side of the mean         |    9   |     9    |   7  |    8   |  8 |
#' |  #3  | `n` (or more) points in a row are continually incr. or decr.           |    6   |     -    |   6  |    6   |  6 |
#' |  #4  | `n` (or more) points in a row alternate in direction, incr. then decr. |   14   |     -    |  14  |   14   |  - |
#' |  #5  | `n - 1` out of `n` points in a row are >2 sd from the mean             |    3   |     3    |   3  |    3   |  3 |
#' |  #6  | `n - 1` out of `n` points in a row are >1 sd from the mean             |    5   |     5    |   5  |    5   |  - |
#' |  #7  | >=`n` points in a row are within 1 sd of the mean                      |   15   |     -    |  15  |   15   | 15 |
#' |  #8  | >=`n` points in a row outside 1 sd of the mean, in both directions     |    8   |     -    |   8  |    8   |  - |
#' 
#' *Montg.*: Montgomery, *HC*: Healthcare
#' 
#' @source Nelson LS. **The Shewhart Control Chart—Tests for Special Causes**. Journal of Quality Technology. Informa UK Limited; 1984 Oct;16(4):237–9. \doi{10.1080/00224065.1984.11978921}.
#' @name qc_rules
#' @rdname qc_rules
#' @export
qc_rule1 <- function(x, m = mean(x), s = sd(x)) {
  which(abs((x - m) / s) >= 3)
}

#' @rdname qc_rules
#' @export
qc_rule2 <- function(x, m = mean(x), threshold = 9) {
  if (length(x) < threshold) {
    return(integer(0))
  }
  n <- length(x)
  counts <- sign(x - m)
  result <- counts
  for (runlength in 2:threshold)
    result <- result + c(counts[runlength:n], rep(0, runlength - 1))
  which(abs(result) >= threshold)
}

#' @rdname qc_rules
#' @export
qc_rule3 <- function(x, threshold = 6) {
  if (length(x) < threshold) {
    return(integer(0))
  }
  n <- length(x)
  signs <- sign(c(x[-1], x[n]) - x)
  counts <- signs
  for (rl in 2:(threshold - 1)) {
    counts <- counts + c(signs[rl:n], rep(0, rl - 1))
  }
  # Between 6 observations you have 5 instances of increasing or decreasing. Therefore threshold - 1.
  which(abs(counts) >= threshold - 1)
}

#' @rdname qc_rules
#' @export
qc_rule4 <- function(x, m = mean(x), threshold = 14, direction_mean = FALSE) {
  if (length(x) < threshold) {
    return(integer(0))
  }
  n <- length(x)
  if (direction_mean == TRUE) {
    signs <- sign(x - m)
  } else {
    signs <- sign(c(x[-1],x[n]) - x)
  }
  counts <- signs
  fac <- -1
  for (rl in 2:threshold) {
    counts <- counts + fac * c(signs[rl:n], rep(0, rl - 1))
    fac <- -fac
  }
  counts <- abs(counts)
  which(counts >= threshold)
}

#' @rdname qc_rules
#' @export
qc_rule5 <- function(x, m = mean(x), s = sd(x), threshold = 3) {
  if (length(x) < threshold) {
    return(integer(0))
  }
  n <- length(x)
  pos <- 1 * ((x - m) / s > 2)
  neg <- 1 * ((x - m) / s < -2)
  poscounts <- pos
  negcounts <- neg
  for (rl in 2:threshold) {
    poscounts <- poscounts + c(pos[rl:n], rep(0, rl - 1))
    negcounts <- negcounts + c(neg[rl:n], rep(0, rl - 1))
  }
  counts <- apply(cbind(poscounts, negcounts), 1, max)
  which(counts >= threshold - 1)
}

#' @rdname qc_rules
#' @export
qc_rule6 <- function(x, m = mean(x), s = sd(x), threshold = 5) {
  if (length(x) < threshold) {
    return(integer(0))
  }
  n <- length(x)
  pos <- 1 * ((x - m) / s > 1)
  neg <- 1 * ((x - m) / s < -1)
  poscounts <- pos
  negcounts <- neg
  for (rl in 2:threshold) {
    poscounts <- poscounts + c(pos[rl:n], rep(0, rl - 1))
    negcounts <- negcounts + c(neg[rl:n], rep(0, rl - 1))
  }
  counts <- apply(cbind(poscounts, negcounts), 1, max)
  which(counts >= threshold - 1)
}

#' @rdname qc_rules
#' @export
qc_rule7 <- function(x, m = mean(x), s = sd(x), threshold = 15) {
  if (length(x) < threshold) {
    return(integer(0))
  }
  n <- length(x)
  within <- 1 * (abs((x - m) / s) < 1)
  counts <- within
  for (rl in 2:threshold)
    counts <- counts + c(within[rl:n], rep(0, rl - 1))
  which(counts >= threshold)
}

#' @rdname qc_rules
#' @export
qc_rule8 <- function(x, m = mean(x), s = sd(x), threshold = 8) {
  if (length(x) < threshold) {
    return(integer(0))
  }
  n <- length(x)
  outofrange <- 1 * (abs((x - m) / s) > 1)
  counts <- outofrange
  for (rl in 2:threshold)
    counts <- counts + c(outofrange[rl:n], rep(0, rl - 1))
  which(counts >= threshold)
}

#' @rdname qc_rules
#' @export
qc_rule_text <- function(rule, threshold) {
  text <- switch(rule,
                 `1` = "One point is more than 3 standard deviations from the mean",
                 `2` = "{n} (or more) points in a row are on the same side of the mean",
                 `3` = "{n} (or more) points in a row are continually increasing or decreasing",
                 `4` = "{n} (or more) points in a row alternate in direction, increasing then decreasing",
                 `5` = "{n-1} out of {n} points in a row are >2 sd from the mean",
                 `6` = "{n-1} out of {n} points in a row are >1 sd from the mean",
                 `7` = ">={n} points in a row are within 1 sd of the mean",
                 `8` = ">={n} points in a row outside 1 sd of the mean, in both directions")
  text <- gsub("{n-1}", threshold - 1, text, fixed = TRUE)
  text <- gsub("{n}", threshold, text, fixed = TRUE)
  text
}

#' @rdname qc_rules
#' @param guideline guideline of QC rules set, must be `"Nelson"`, `"Westgard"`, `"AIAG"`, `"Montgomery"`, or `"Healthcare"`
#' @export
#' @examples 
#' x <- qc_test(rnorm(250))
#' x
#' 
#' # turn into data.frame, e.g. for export
#' head(as.data.frame(x))
#' 
#' if (require("certeplot2")) {
#'   plot2(x,
#'         subtitle = "Workflow 'example123'")
#' }
qc_test <- function(x, m = mean(x), s = sd(x), guideline = "Nelson") {
  if (guideline == "Nelson") {
    out <- list(rule_1 = qc_rule1(x, m, s),
                rule_2 = qc_rule2(x, m, 9),
                rule_3 = qc_rule3(x, 6),
                rule_4 = qc_rule4(x, m, 14, FALSE),
                rule_5 = qc_rule5(x, m, s, 3),
                rule_6 = qc_rule6(x, m, s, 5),
                rule_7 = qc_rule7(x, m, s, 15),
                rule_8 = qc_rule8(x, m, s, 8))
    attr(out, "threshold") <- c(1, 9, 6, 14, 3, 5, 15, 8)
  } else if (guideline == "Westgard") {
    out <- list(rule_1 = qc_rule1(x, m, s),
                rule_2 = qc_rule2(x, m, 9),
                rule_5 = qc_rule5(x, m, s, 3),
                rule_6 = qc_rule6(x, m, s, 5))
    attr(out, "threshold") <- c(1, 9, 3, 5)
  } else if (guideline == "AIAG") {
    out <- list(rule_1 = qc_rule1(x, m, s),
                rule_2 = qc_rule2(x, m, 7),
                rule_3 = qc_rule3(x, 6),
                rule_4 = qc_rule4(x, m, 14, FALSE),
                rule_5 = qc_rule5(x, m, s, 3),
                rule_6 = qc_rule6(x, m, s, 5),
                rule_7 = qc_rule7(x, m, s, 15),
                rule_8 = qc_rule8(x, m, s, 8))
    attr(out, "threshold") <- c(1, 7, 6, 14, 3, 5, 15, 8)
  } else if (guideline == "Montgomery") {
    out <- list(rule_1 = qc_rule1(x, m, s),
                rule_2 = qc_rule2(x, m, 8),
                rule_3 = qc_rule3(x, 6),
                rule_4 = qc_rule4(x, m, 14, FALSE),
                rule_5 = qc_rule5(x, m, s, 3),
                rule_6 = qc_rule6(x, m, s, 5),
                rule_7 = qc_rule7(x, m, s, 15),
                rule_8 = qc_rule8(x, m, s, 8))
    attr(out, "threshold") <- c(1, 8, 6, 14, 3, 5, 15, 8)
  } else if (guideline == "Healthcare") {
    out <- list(rule_1 = qc_rule1(x, m, s),
                rule_2 = qc_rule2(x, m, 8),
                rule_3 = qc_rule3(x, 6),
                rule_5 = qc_rule5(x, m, s, 3),
                rule_7 = qc_rule7(x, m, s, 15))
    attr(out, "threshold") <- c(1, 8, 6, 3, 15)
  } else {
    stop("Unsupported QC guideline: ", guideline)
  }
  structure(out,
            class = c("qc_test", "list"),
            guideline = guideline,
            values = x)
}

#' @method print qc_test
#' @noRd
#' @export
print.qc_test <- function(x, ...) {
  title <- paste("Quality Control Rules according to", attributes(x)$guideline)
  cat(title, "\n", sep = "")
  cat(strrep("-", nchar(title)), "\n", sep = "")
  cat(" n    = ", length(attributes(x)$values), "\n")
  cat(" mean = ", mean(attributes(x)$values), "\n")
  cat(" sd   = ", sd(attributes(x)$values), "\n")
  cat(strrep("-", nchar(title)), "\n", sep = "")
  for (i in seq_len(length(x))) {
    rule <- as.integer(gsub("[^0-9]", "", names(x)[i]))
    cat("\nQC Rule ", rule, ": ", qc_rule_text(rule, attributes(x)$threshold[i]), "\n", sep = "")
    if (length(x[[i]]) == 0) {
      cat("No violations\n")
    } else {
      cat("Violation on starting position", ifelse(length(x[[i]]) > 1, "s", ""), ":\n", sep = "")
      print(x[[i]])
      cat("Violation on starting value", ifelse(length(x[[i]]) > 1, "s", ""), ":\n", sep = "")
      print(attributes(x)$values[x[[i]]])
    }
  }
}

#' @method as.data.frame qc_test
#' @noRd
#' @export
as.data.frame.qc_test <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- data.frame(index = seq_len(length(attributes(x)$values)),
                    value = attributes(x)$values, ...)
  out[x$rule_1, "rule_1"] <- TRUE
  out[x$rule_2, "rule_2"] <- TRUE
  out[x$rule_3, "rule_3"] <- TRUE
  out[x$rule_4, "rule_4"] <- TRUE
  out[x$rule_5, "rule_5"] <- TRUE
  out[x$rule_6, "rule_6"] <- TRUE
  out[x$rule_7, "rule_7"] <- TRUE
  out[x$rule_8, "rule_8"] <- TRUE
  out
}
