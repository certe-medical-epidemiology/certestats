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

#' Nelson's Quality Control Rules
#'
#' These rules are used for quality control (QC). Default values are set for Nelson's criteria, but they also support Westgard, AIAG, Montgomery and Healthcare QC rules.
#' @param x vector with values
#' @param m mean
#' @param s standard deviation
#' @param threshold minimal number of sequential values before rule is triggered (defaults to Nelson's)
#' @param direction.mean a logical to indicate whether *n* observations in a row must be tested for alternating in direction of the mean
#' @param rule number of the rule
#' @section Rules list:
#' 
#' | Rule | Rule explanation:                                                      | Nelson | Westgard | AIAG | Montg. | HC |
#' |:----:|------------------------------------------------------------------------|:------:|:--------:|:----:|:------:|:--:|
#' |  #1  | One point is more than 3 standard deviations from the mean             |    1   |     1    |   1  |    1   |  1 |
#' |  #2  | *n* (or more) points in a row are on the same side of the mean         |    9   |     9    |   7  |    8   |  8 |
#' |  #3  | *n* (or more) points in a row are continually incr. or decr.           |    6   |     -    |   6  |    6   |  6 |
#' |  #4  | *n* (or more) points in a row alternate in direction, incr. then decr. |   14   |     -    |  14  |   14   |  - |
#' |  #5  | *n*` - 1` out of *n* points in a row are >2 sd from the mean           |    3   |     3    |   3  |    3   |  3 |
#' |  #6  | *n*` - 1` out of *n* points in a row are >1 sd from the mean           |    5   |     5    |   5  |    5   |  - |
#' |  #7  | >=*n* points in a row are within 1 sd of the mean                      |   15   |     -    |  15  |   15   | 15 |
#' |  #8  | >=*n* points in a row outside 1 sd of the mean, in both directions     |    8   |     -    |   8  |    8   |  - |
#' 
#'   *Montg.*: Montgomery, *HC*: Healthcare
#' @source Nelson LS. **The Shewhart Control Chart—Tests for Special Causes**. Journal of Quality Technology [Internet]. Informa UK Limited; 1984 Oct;16(4):237–9. Available from: <http://dx.doi.org/10.1080/00224065.1984.11978921>
#' @name nelson_rules
#' @rdname nelson_rules
#' @export
nelson_rule1 <- function(x, m = mean(x), s = sd(x)) {
  which(abs((x - m) / s) >= 3)
}

#' @rdname nelson_rules
#' @export
nelson_rule2 <- function(x, m = mean(x), threshold = 9) {
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

#' @rdname nelson_rules
#' @export
nelson_rule3 <- function(x, threshold = 6) {
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

#' @rdname nelson_rules
#' @export
nelson_rule4 <- function(x, m = mean(x), threshold = 14, direction.mean = FALSE) {
  if (length(x) < threshold) {
    return(integer(0))
  }
  n <- length(x)
  if (direction.mean == TRUE) {
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

#' @rdname nelson_rules
#' @export
nelson_rule5 <- function(x, m = mean(x), s = sd(x), threshold = 3) {
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

#' @rdname nelson_rules
#' @export
nelson_rule6 <- function(x, m = mean(x), s = sd(x), threshold = 5) {
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

#' @rdname nelson_rules
#' @export
nelson_rule7 <- function(x, m = mean(x), s = sd(x), threshold = 15) {
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

#' @rdname nelson_rules
#' @export
nelson_rule8 <- function(x, m = mean(x), s = sd(x), threshold = 8) {
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

#' @rdname nelson_rules
#' @export
nelson_text <- function(rule, threshold) {
  text <- switch(rule,
                 `1` = "One point is more than 3 standard deviations from the mean",
                 `2` = "{n} (or more) points in a row are on the same side of the mean",
                 `3` = "{n} (or more) points in a row are continually increasing or decreasing",
                 `4` = "{n} (or more) points in a row alternate in direction, increasing then decreasing",
                 `5` = "{n-1} out of {n} points in a row are >2 sd from the mean",
                 `6` = "{n-1} out of {n} points in a row are >1 sd from the mean",
                 `7` = ">= {n} points in a row are within 1 sd of the mean",
                 `8` = ">= {n} points in a row outside 1 sd of the mean, in both directions")
  text <- gsub("{n-1}", threshold - 1, text, fixed = TRUE)
  text <- gsub("{n}", threshold, text, fixed = TRUE)
  text
}
