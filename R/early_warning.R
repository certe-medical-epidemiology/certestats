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

#' Early Warning for Disease Clusters
#' 
#' Detect disease clusters with [early_warning_cluster()]. Use [has_clusters()] to return `TRUE` or `FALSE` based on its output, or employ [format()] to format the result.
#' @param df Data set: This must consist of **only positive results**. The minimal data set should include a date column and a patient column. Do not summarize on patient IDs; this will be handled automatically.
#' @param ... Filter arguments for defining the data to study, e.g., `year == 2023`. All other observations will serve as controls.
#' @param group_by Column to use in [group_by()], e.g., the name of a microorganism.
#' @param column_date Name of the column to use for dates. If left blank, the first date column will be used.
#' @param column_patientid Name of the column to use for patient IDs. If left blank, the first column resembling `"patient|patid"` will be used.
#' @param based_on_historic_maximum A [logical] to indicate whether the percentile should be based on the maximum of previous years. The default is `FALSE`, which uses all historic data points.
#' @param minimum_case_days Number of days with cases that must pass above the set percentile before detecting the elevation as a cluster.
#' @param minimum_number_cases Number of cases that a cluster must have to be considered a cluster.
#' @param threshold_percentile Threshold to set.
#' @param remove_outliers A [logical] to indicate whether outliers should be removed before determining the threshold.
#' @param remove_outliers_coefficient Coefficient used for outlier determination.
#' @param moving_average_days Number of days to set in [moving_average()]. Defaults to a whole week (`7`).
#' @param moving_average_side Side of days to set in [moving_average()]. Defaults to `"left"` for retrospective analysis.
#' @param case_free_days Number of days to set in [`get_episode()`][AMR::get_episode()].
#' @details
#' A (disease) cluster is defined as an unusually large aggregation of disease events in time or space ([ATSDR, 2008](https://www.atsdr.cdc.gov/hec/csem/cluster/docs/clusters.pdf)). They are common, particularly in large populations. From a statistical standpoint, it is nearly inevitable that some clusters of chronic diseases will emerge within various communities, be it schools, church groups, social circles, or neighborhoods. Initially, these clusters are often perceived as products of specific, predictable processes rather than random occurrences in a particular location, akin to a coin toss.
#' 
#' Whether a (suspected) cluster corresponds to an actual increase of disease in the area, needs to be assessed by an epidemiologist or biostatistician ([ATSDR, 2008](https://www.atsdr.cdc.gov/hec/csem/cluster/docs/clusters.pdf)).
#' @importFrom dplyr n_distinct group_by summarise filter mutate ungroup select row_number tibble
#' @importFrom tidyr fill complete
#' @importFrom certestyle format2
#' @importFrom AMR get_episode
#' @seealso [early_warning_biomarker()]
#' @rdname early_warning_cluster
#' @export
#' @examples
#' cases <- data.frame(date = sample(seq(as.Date("2015-01-01"),
#'                                       as.Date("2022-12-31"),
#'                                       "1 day"),
#'                                   size = 300),
#'                     patient = sample(LETTERS, size = 300, replace = TRUE))
#'
#' # -----------------------------------------------------------
#' 
#' check <- early_warning_cluster(cases,
#'                                date >= "2022-01-01",
#'                                threshold_percentile = 0.99)
#' 
#' has_clusters(check)
#' check
#' 
#' 
#' check2 <- early_warning_cluster(cases,
#'                                 date >= "2022-01-01",
#'                                 minimum_number_cases = 1,
#'                                 threshold_percentile = 0.75)
#' 
#' check2
#' check2 |> format()
#' 
#' check2 |> n_clusters()
#' check2 |> has_clusters()
#' check2 |> has_clusters(n = 15)
#' 
#' check2 |> has_ongoing_cluster("2022-06-01")
#' check2 |> has_ongoing_cluster(c("2022-06-01", "2022-06-20"))
#' 
#' check2 |> unclass()
early_warning_cluster <- function(df,
                                  ...,
                                  group_by = NULL,
                                  column_date = NULL,
                                  column_patientid = NULL,
                                  based_on_historic_maximum = FALSE,
                                  minimum_case_days = 2,
                                  minimum_number_cases = 5,
                                  threshold_percentile = 97.5,
                                  remove_outliers = TRUE,
                                  remove_outliers_coefficient = 1.5,
                                  moving_average_days = 7,
                                  moving_average_side = "left",
                                  case_free_days = 14) {
  if (!is.null(group_by)) {
    stop("'group_by' is not yet implemented")
  }
  
  year <- function(x) as.integer(format(x, "%Y"))
  unify_years <- function(x) {
    is_leap_year <- any(x |> format() |> substr(6, 10) == "02-29", na.rm = TRUE)
    if (inherits(x, "Date")) {
      as.Date(paste0(ifelse(is_leap_year, "1972", "1970"), x |> format() |> substr(5, 10)))
    } else {
      as.POSIXct(paste0(ifelse(is_leap_year, "1972", "1970"), x |> format() |> substr(5, 99)))
    }
  }
  
  if (is.null(column_date)) {
    column_date <- df |> 
      vapply(FUN.VALUE = logical(1), inherits, c("Date", "POSIXt"))
    if (length(which(column_date)) == 0) {
      stop("No date column found")
    } else {
      column_date <- names(column_date[which(column_date)][1])
      message("Using column '", column_date, "' for dates")
    }
  }
  if (is.null(column_patientid)) {
    column_patientid <- colnames(df) %like% "patient|patid"
    if (length(which(column_patientid)) == 0) {
      stop("No patient column found")
    } else {
      column_patientid <- colnames(df)[which(column_patientid)][1]
      message("Using column '", column_patientid, "' for patient IDs")
    }
  }
  if (threshold_percentile < 1) {
    threshold_percentile <- threshold_percentile * 100
  }
  
  empty_output <- list(clusters = tibble(date = Sys.Date()[0], cases = integer(0), cluster = integer(0),
                                         days = integer(0), case_days = integer(0)),
                       details = tibble(year = integer(0), month_day = Sys.Date()[0], in_scope = logical(0), cases = integer(0),
                                        ma_5c = double(0), max_ma_5c = double(0), ma_5c_pct_outscope = double(0)))
  
  # add the new columns
  df.bak <- df
  df$date <- as.Date(df[, column_date, drop = TRUE])
  df$patient <- df[, column_patientid, drop = TRUE]
  df$row <- seq_len(nrow(df))
  df$in_scope <- df$row %in% (df |> filter(...) |> pull(row))
  
  # some checks
  if (all(df$in_scope, na.rm = TRUE)) {
    warning("All cases are 'in scope' - no clusters found. Use ... to filter on specific cases.")
    return(structure(empty_output,
                     threshold_percentile = threshold_percentile,
                     based_on_historic_maximum = based_on_historic_maximum,
                     remove_outliers = remove_outliers,
                     remove_outliers_coefficient = remove_outliers_coefficient,
                     moving_average_days = moving_average_days,
                     minimum_case_days = minimum_case_days,
                     minimum_number_cases = minimum_number_cases,
                     class = c("early_warning_cluster", "list")))
  }
  if (n_distinct(df$patient) == nrow(df)) {
    warning("Only unique patients found - this is uncommon. Did you summarise accidentally on patient IDs?")
  }
  
  df_early <- df |>
    group_by(date, in_scope) |>
    summarise(cases = n_distinct(patient), .groups = "drop") |> 
    complete(date = seq(from = min(as.Date(date), na.rm = TRUE),
                        to = max(as.Date(date), na.rm = TRUE),
                        by = "1 day"),
             fill = list(cases = 0)) |>
    fill(in_scope, .direction = "down") |> 
    mutate(ma_5c = moving_average(cases, w = moving_average_days, side = moving_average_side, na.rm = TRUE),
           year = year(date),
           month_day = unify_years(date)) |> 
    group_by(in_scope) |> 
    mutate(is_outlier = ma_5c %in% grDevices::boxplot.stats(ma_5c[!is.na(ma_5c)], coef = remove_outliers_coefficient)$out) |> 
    group_by(month_day) |>
    mutate(max_ma_5c = ifelse(length(ma_5c[!is.na(ma_5c) & !in_scope & !(remove_outliers & is_outlier)]) > 0,
                              max(ma_5c[!is.na(ma_5c) & !in_scope & !(remove_outliers & is_outlier)], na.rm = TRUE),
                              NA_real_)) |>
    ungroup() |> 
    mutate(ma_5c_pct_outscope = ifelse(
      isTRUE(based_on_historic_maximum),
      quantile(max_ma_5c, threshold_percentile / 100, na.rm = TRUE),
      quantile(ma_5c[!is.na(ma_5c) & !in_scope & !(remove_outliers & is_outlier)], threshold_percentile / 100, na.rm = TRUE))) |> 
    group_by(week = format2(date, "yyyy-ww")) |>
    mutate(cases_week = sum(cases, na.rm = TRUE)) |> 
    ungroup()
  
  df_filter <- df_early |> 
    filter(in_scope,
           ma_5c > ma_5c_pct_outscope,
           cases > 0)
  
  if (nrow(df_filter) == 0) {
    clusters <- empty_output$clusters
  } else {
    clusters <- df_filter |> 
      mutate(cluster = get_episode(date, case_free_days = case_free_days)) |> 
      group_by(cluster) |> 
      filter(n_distinct(date) >= minimum_case_days,
             sum(cases, na.rm = TRUE) >= minimum_number_cases)
    
    if (nrow(clusters) == 0) {
      clusters <- empty_output$clusters
    } else {
      clusters <- clusters |> 
        # determine episodes again, since some might have been filtered
        ungroup() |> 
        mutate(cluster = get_episode(date, case_free_days = case_free_days)) |> 
        group_by(cluster) |> 
        mutate(case_days = row_number()) |> 
        complete(date = seq(from = min(as.Date(date), na.rm = TRUE),
                            to = max(as.Date(date), na.rm = TRUE),
                            by = "1 day"),
                 fill = list(cases = 0)) |>
        fill(case_days, .direction = "down") |> 
        mutate(days = row_number()) |> 
        ungroup() |> 
        select(date, cases, cluster, days, case_days)
    }
  }
  
  details <- df_early |>
    select(year, month_day, in_scope, cases, ma_5c, max_ma_5c, ma_5c_pct_outscope)
  
  x <- structure(list(clusters = clusters, details = details),
                 threshold_percentile = threshold_percentile,
                 based_on_historic_maximum = based_on_historic_maximum,
                 remove_outliers = remove_outliers,
                 remove_outliers_coefficient = remove_outliers_coefficient,
                 moving_average_days = moving_average_days,
                 minimum_case_days = minimum_case_days,
                 minimum_number_cases = minimum_number_cases,
                 class = c("early_warning_cluster", "list"))
  x
}

#' @importFrom dplyr n_distinct
#' @rdname early_warning_cluster
#' @param x output of [early_warning_cluster()]
#' @export
n_clusters <- function(x) {
  n_distinct(x$clusters$cluster)
}

#' @rdname early_warning_cluster
#' @param n number of clusters, defaults to 1
#' @export
has_clusters <- function(x, n = 1) {
  n_clusters(x) >= n
}

#' @importFrom dplyr filter
#' @rdname early_warning_cluster
#' @param dates date(s) to test whether any of the clusters currently has this date in it, defaults to yesterday. [has_ongoing_cluster()] will return a [logical] vector with the same length as `dates`, so `dates` can have any length.
#' @export
has_ongoing_cluster <- function(x, dates = Sys.Date() - 1) {
  dates <- as.Date(dates)
  out <- format(x)
  vapply(FUN.VALUE = logical(1),
         dates,
         function(dt) any(out$first_day <= dt & out$last_day >= dt, na.rm = TRUE))
}

#' @noRd
#' @importFrom dplyr group_by summarise
#' @export
format.early_warning_cluster <- function(x, ...) {
  if (nrow(x$clusters) == 0) {
    data.frame(cluster = integer(0),
               first_day = Sys.Date()[0],
               last_day = Sys.Date()[0],
               days = integer(0),
               case_days = integer(0),
               total_cases = integer(0))
  } else {
    x$clusters |> 
      group_by(cluster) |> 
      summarise(first_day = min(date, na.rm = TRUE),
                last_day = max(date, na.rm = TRUE),
                days = max(days, na.rm = TRUE),
                case_days = max(case_days, na.rm = TRUE),
                total_cases = sum(cases, na.rm = TRUE))
  }
}

#' @importFrom dplyr case_when
#' @importFrom certestyle format2
#' @noRd
#' @export
print.early_warning_cluster <- function(x, ...) {
  out <- format(x)
  signed_nr <- function(n) {
    n <- format(n)
    last_char <- substr(n, nchar(n), nchar(n))
    paste0(n,
           case_when(grepl(".", n, fixed = TRUE) ~ "th",
                     last_char == "1" ~ "st",
                     last_char == "2" ~ "nd",
                     last_char == "3" ~ "rd",
                     TRUE ~ "th"))
  }
  intro <- paste0("=> Detected ",
                  ifelse(nrow(out) == 0,
                         "no disease clusters ",
                         ifelse(nrow(out) == 1,
                                "1 disease cluster",
                                paste0(nrow(out), " disease clusters"))))
  
  based_on <- paste0("based on ",
                     ifelse(attributes(x)$minimum_number_cases > 0,
                            paste0(">=", attributes(x)$minimum_number_cases, " case", ifelse(attributes(x)$minimum_number_cases == 1, "", "s"), ", "),
                            ""),
                     ifelse(isTRUE(attributes(x)$based_on_historic_maximum),
                            "the historic maximum and ",
                            ""),
                     ifelse(isTRUE(attributes(x)$remove_outliers),
                            paste0("an outlier-free history (coeff = ", format(attributes(x)$remove_outliers_coefficient),
                                   ") and "),
                            ""),
                     "the ", signed_nr(attributes(x)$threshold_percentile), " pct",
                     " lasting for >=", attributes(x)$minimum_case_days, " case days.")
  if (nrow(out)) {
    message(intro, " with a total of ", sum(out$total_cases), " cases ", based_on)
    dates <- function(x, y) {
      if (format2(x, "yyyy-mm") == format2(y, "yyyy-mm")) {
        return(paste0(format2(x, "d"), " and ", format2(y, "d mmmm yyyy")))
      } else if (format2(x, "yyyy") == format2(y, "yyyy")) {
        return(paste0(format2(x, "d mmmm"), " and ", format2(y, "d mmmm yyyy")))
      } else {
        return(paste0(format2(x, "d mmmm yyyy"), " and ", format2(y, "d mmmm yyyy")))
      }
    }
    for (i in seq_len(nrow(out))) {
      message("   - The ", signed_nr(i), " cluster has ", out$total_cases[i], " cases between ", dates(out$first_day[i], out$last_day[i]))
    }
    message("Use plot2() to plot the results.")
  } else {
    message(intro, based_on)
    invisible(NULL)
  }
}

#' Early Warning for Biomarkers
#' 
#' The `early_warning_biomarker` function is designed to detect unexpected changes in biomarker values for individual patients based on clinical chemistry data. It will flag cases where patients' biomarker results deviate from defined thresholds or exhibit significant changes within a specified time window.
#' @inheritParams early_warning_cluster
#' @param df A data frame containing clinical chemistry data with columns for patient ID, date, test code, and test result.
#' @param ... Filter arguments, e.g. `testcode == "eGFR"`
#' @param testcode Value of the column containing test codes to filter on.
#' @param column_testcode Name of the column containing test codes.
#' @param column_testresult Name of the column containing test results.
#' @param threshold_min Minimum threshold for biomarkers.
#' @param threshold_max Maximum threshold for biomarkers.
#' @param window_days Number of days for the time window to check for changes.
#' @param max_delta_absolute Maximum allowable absolute change in biomarkers.
#' @param max_delta_relative Maximum allowable relative change in biomarkers.
#' @param direction Direction of change to check ("up", "down", or "any").
#' @details
#' *This whole function, including the documentation, was written by ChatGPT 3.5 in October 2023. Only minor changes were applied manually.*
#' 
#' This function is particularly useful in early detection of anomalous biomarker results, which can be indicative of health issues or treatment response. By providing detailed flags, it allows healthcare professionals and researchers to take timely action, conduct further investigations, or make informed clinical decisions.
#'
#' The output of this function can be utilised for:
#' - Generating patient-specific reports for healthcare providers.
#' - Identifying trends and patterns in biomarker changes for research purposes.
#' - Enhancing patient care by enabling proactive interventions when necessary.
#' - Supporting data-driven clinical epidemiology studies and research.
#' 
#' The [format()] function allows you to format the results of the `early_warning_biomarker()` function for better readability and analysis. It organises the flag information into a structured data frame for easier inspection.
#' @return A list with the following components:
#' 
#'   - `flags`: A list of flags per patient, containing data frames for each patient with details on dates, test codes, test results, and flagging criteria. The structure of each data frame includes the following columns:
#'     - `patient`: Patient identifier.
#'     - `date`: Date of the biomarker measurement.
#'     - `testcode`: Code of the biomarker test.
#'     - `testresult`: Biomarker test result.
#'     - `delta_absolute`: Absolute change in biomarker values.
#'     - `delta_relative`: Relative change in biomarker values.
#'     - `threshold_min_flag`: A logical flag indicating if the threshold minimum is exceeded.
#'     - `threshold_max_flag`: A logical flag indicating if the threshold maximum is exceeded.
#'     - `delta_absolute_flag`: A logical flag indicating if the absolute change exceeds the threshold.
#'     - `delta_relative_flag`: A logical flag indicating if the relative change exceeds the threshold.
#'
#' - `details`: A data frame containing all patient details and calculated flags, regardless of whether they meet the flagging criteria. The data frame includes the same columns as the individual patient data frames.
#' @importFrom dplyr arrange group_by filter mutate select lag ungroup
#' @seealso [early_warning_cluster()]
#' @rdname early_warning_biomarker
#' @export
#' @examples
#' data <- data.frame(date = Sys.Date() + 1:10,
#'                    patient = "test",
#'                    value = c(10,12,14,15,13,21,22,19,14, 12))
#' 
#' check <- data |> early_warning_biomarker(window_days = 6, max_delta_absolute = 10)
#' 
#' check
#' 
#' unlist(check)
early_warning_biomarker <- function(df,
                                    testcode = NULL,
                                    ...,
                                    column_date = NULL,
                                    column_patientid = NULL,
                                    column_testcode = NULL,
                                    column_testresult = NULL,
                                    threshold_min = NULL,
                                    threshold_max = NULL,
                                    window_days = NULL,
                                    max_delta_absolute = NULL,
                                    max_delta_relative = NULL,
                                    direction = "any") {
  
  if (!is.null(max_delta_relative) && max_delta_relative > 1) {
    max_delta_relative <- max_delta_relative / 100
  }
  direction <- tolower(direction[1])
  if (!direction %in% c("up", "down", "any")) {
    stop('direction must be "up", "down", or "any"')
  }
  
  if (is.null(column_date)) {
    column_date <- df |> 
      vapply(FUN.VALUE = logical(1), inherits, c("Date", "POSIXt"))
    if (length(which(column_date)) == 0) {
      stop("No date column found")
    } else {
      column_date <- names(column_date[which(column_date)][1])
      message("Using column '", column_date, "' for dates")
    }
  }
  if (is.null(column_patientid)) {
    column_patientid <- colnames(df) %like% "patient|patid"
    if (length(which(column_patientid)) == 0) {
      stop("No patient column found")
    } else {
      column_patientid <- colnames(df)[which(column_patientid)][1]
      message("Using column '", column_patientid, "' for patient IDs")
    }
  }
  if (is.null(column_testcode) && !is.null(testcode)) {
    column_testcode <- colnames(df) %like% "test.*code|bepaling.*code"
    if (length(which(column_testcode)) == 0) {
      stop("No test code column found")
    } else {
      column_testcode <- colnames(df)[which(column_testcode)][1]
      message("Using column '", column_testcode, "' for test codes")
    }
  }
  if (is.null(column_testresult)) {
    column_testresult <- colnames(df) %like% "result|resultaat|uitslag|waarde|value" & vapply(FUN.VALUE = logical(1), df, is.numeric)
    if (length(which(column_testresult)) == 0) {
      stop("No numeric test result column found")
    } else {
      column_testresult <- colnames(df)[which(column_testresult)][1]
      message("Using column '", column_testresult, "' for test results")
    }
  }
  
  `%|%` <- function(x, y) if (is.null(x)) y else x
  na_0 <- function(x) {
    x[is.na(x)] <- 0
    x
  }
  first_0 <- function(x) {
    x[1] <- 0
    x
  }
  
  # add the new columns
  df.bak <- df
  testcode_set <- testcode
  df$date <- as.Date(df[, column_date, drop = TRUE])
  df$patient <- df[, column_patientid, drop = TRUE]
  if (is.null(column_testcode)) {
    df$testcode <- NA_character_
  } else {
    df$testcode <- df[, column_testcode, drop = TRUE]
  }
  df$testresult <- df[, column_testresult, drop = TRUE]
  df$row <- seq_len(nrow(df))
  df_filtered <- df |> filter(...)
  if (!is.null(testcode_set)) {
    df_filtered <- df |> filter(testcode == testcode_set)
  }
  df$in_scope <- df$row %in% df_filtered$row
  
  # Calculate flags for each patient
  df_details <- df |>
    filter(in_scope) |> 
    group_by(patient) |>
    arrange(date) |>
    mutate(
      # Calculate differences in test results
      delta_absolute = c(0, diff(testresult)),
      # Calculate relative change
      delta_relative = delta_absolute / lag(testresult, default = 1),
      # Check for exceeding thresholds and direction
      threshold_min_flag = testresult < (threshold_min %|% testresult),
      threshold_max_flag = testresult > (threshold_max %|% testresult),
      delta_absolute_flag = abs(delta_absolute) > (max_delta_absolute %|% abs(delta_absolute)),
      delta_relative_flag = (direction == "up" & delta_relative > (max_delta_relative %|% delta_relative)) |
        (direction == "down" & delta_relative < -(max_delta_relative %|% -delta_relative)) |
        (direction == "any" & abs(delta_relative) > (max_delta_relative %|% abs(delta_relative)))) |> 
    select(patient, date, testcode, testresult, delta_absolute, delta_relative,
           threshold_min_flag, threshold_max_flag, delta_absolute_flag, delta_relative_flag)
  
  # Calculate flags within the specified time window
  if (!is.null(window_days)) {
    df_details <- df_details |>
      mutate(sum_delta_absolute = cumsum(na_0(delta_absolute)),
             cumulative_days = cumsum(first_0(as.double(difftime(date, lag(date), units = "days")))),
             delta_absolute_flag = (sum_delta_absolute > (max_delta_absolute %|% 0) & cumulative_days <= window_days))
  }
  df_details <- df_details |>
    ungroup() |> 
    mutate(any_flag = threshold_min_flag | threshold_max_flag | delta_absolute_flag | delta_relative_flag)
  
  # Return the flags as a list
  df_flags <- df_details |> 
    filter(any_flag) |> 
    select(-any_flag)
  flags <- split(df_flags, df_flags$patient)
  return(structure(list(flags = flags, details = df_details),
                   class = c("early_warning_biomarker", "list")))
}

#' @importFrom dplyr bind_rows as_tibble
#' @noRd
#' @export
format.early_warning_biomarker <- function(x, ...) {
  if (length(x$flags) == 0) {
    out <- data.frame(patient = NA_character_,
                      total_flags = NA_integer_,
                      flag_threshold_min = NA_integer_,
                      flag_threshold_max = NA_integer_,
                      flag_delta_absolute = NA_integer_,
                      flag_delta_relative = NA_integer_)
  } else {
    out <- do.call(bind_rows,
                   lapply(x$flags,
                          function(df) data.frame(patient = unique(df$patient),
                                                  total_flags = nrow(df),
                                                  flag_threshold_min = sum(df$threshold_min_flag, na.rm = TRUE),
                                                  flag_threshold_max = sum(df$threshold_max_flag, na.rm = TRUE),
                                                  flag_delta_absolute = sum(df$delta_absolute_flag, na.rm = TRUE),
                                                  flag_delta_relative = sum(df$delta_relative_flag, na.rm = TRUE))))
  }
  as_tibble(out)
}

#' @noRd
#' @export
print.early_warning_biomarker <- function(x, ...) {
  cat("A total of ", length(x$flags), " patients with a total of ",
      sum(vapply(FUN.VALUE = integer(1), x$flags, nrow)), " flags.\n\n", sep = "")
  print(format(x), ...)
}