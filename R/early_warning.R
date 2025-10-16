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

#' Detect Disease Clusters
#' 
#' Detect disease clusters with [detect_disease_clusters()]. Use [has_clusters()] to return `TRUE` or `FALSE` based on its output, or employ [format()] to format the result.
#' @param df Data set: This must consist of **only positive results**. The minimal data set should include a date column and a patient column. Do not summarize on patient IDs; this will be handled automatically.
#' @param column_date Name of the column to use for dates. If left blank, the first date column will be used.
#' @param column_patientid Name of the column to use for patient IDs. If left blank, the first column resembling `"patient|patid"` will be used.
#' @param period_length_months Number of months per period.
#' @param based_on_historic_maximum A [logical] to indicate whether the cluster detection should be based on the maximum of previous years. The default is `FALSE`, which uses all historic data points.
#' @param minimum_cases Minimum number of *cases* that a cluster requires to be considered a cluster.
#' @param minimum_days Minimum number of *days* that a cluster requires to be considered a cluster.
#' @param minimum_case_days Minimum number of *days with cases* that a cluster requires to be considered a cluster.
#' @param minimum_case_fraction_in_period Minimum fraction of *cluster cases in a period* that a cluster requires to be considered a cluster.
#' @param threshold_percentile Threshold to set.
#' @param remove_outliers A [logical] to indicate whether outliers should be removed before determining the threshold.
#' @param remove_outliers_coefficient Coefficient used for outlier determination.
#' @param moving_average_days Number of days to set in [moving_average()]. Defaults to a whole week (`7`).
#' @param moving_average_side Side of days to set in [moving_average()]. Defaults to `"left"` for retrospective analysis.
#' @param case_free_days Number of days to set in [`get_episode()`][AMR::get_episode()].
#' @param ... not used at the moment
#' @details
#' A (disease) cluster is defined as an unusually large aggregation of disease events in time or space ([ATSDR, 2008](https://www.atsdr.cdc.gov/hec/csem/cluster/docs/clusters.pdf)). They are common, particularly in large populations. From a statistical standpoint, it is nearly inevitable that some clusters of chronic diseases will emerge within various communities, be it schools, church groups, social circles, or neighborhoods. Initially, these clusters are often perceived as products of specific, predictable processes rather than random occurrences in a particular location, akin to a coin toss.
#' 
#' Whether a (suspected) cluster corresponds to an actual increase of disease in the area, needs to be assessed by an epidemiologist or biostatistician ([ATSDR, 2008](https://www.atsdr.cdc.gov/hec/csem/cluster/docs/clusters.pdf)).
#' @importFrom dplyr n_distinct group_by summarise filter mutate ungroup select row_number tibble all_of count pull
#' @importFrom lubridate interval days years year<- `%m-%`
#' @importFrom tidyr fill complete
#' @importFrom certestyle format2
#' @importFrom AMR get_episode
#' @seealso [detect_biomarker_changes()]
#' @rdname detect_disease_clusters
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
#' check <- detect_disease_clusters(cases, threshold_percentile = 0.99)
#' 
#' has_clusters(check)
#' check
#' 
#' 
#' check2 <- detect_disease_clusters(cases,
#'                                   minimum_cases = 1,
#'                                   threshold_percentile = 0.75)
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
#' check2 |> has_cluster_before("2022-06-01")
#' check2 |> has_cluster_after("2022-06-01")
#' 
#' check2 |> unclass()
#' 
#' if (require("certeplot2")) {
#'   check2 |> plot2()
#' }
detect_disease_clusters <- function(df,
                                    column_date = NULL,
                                    column_patientid = NULL,
                                    based_on_historic_maximum = FALSE,
                                    period_length_months = 12,
                                    minimum_cases = 5,
                                    minimum_days = 0,
                                    minimum_case_days = 2,
                                    minimum_case_fraction_in_period = 0.02,
                                    threshold_percentile = 97.5,
                                    remove_outliers = TRUE,
                                    remove_outliers_coefficient = 1.5,
                                    moving_average_days = 7,
                                    moving_average_side = "left",
                                    case_free_days = 14,
                                    ...) {
  
  year <- function(x) as.integer(format(x, "%Y"))
  unify_years <- function(x) {
    max_date <- max(x, na.rm = TRUE)
    year(x) <- 2000
    year(max_date) <- 2000
    x[x > max_date] <- x[x > max_date] - years(1)
    x
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
  
  empty_output <- list(clusters = tibble(date = Sys.Date()[0],
                                         cases = integer(0),
                                         cluster = integer(0),
                                         day_in_period = integer(0),
                                         days = integer(0),
                                         case_days = integer(0)),
                       details = tibble(year = integer(0),
                                        date = Sys.Date()[0],
                                        period_date = Sys.Date()[0],
                                        day_in_period = integer(0),
                                        period = integer(0),
                                        cases = integer(0),
                                        moving_avg = double(0),
                                        moving_avg_max = double(0),
                                        moving_avg_pctile = double(0),
                                        moving_avg_limit = double(0)))
  
  # add the new columns
  df.bak <- df
  df$date <- as.Date(df[, column_date, drop = TRUE])
  df$patient <- df[, column_patientid, drop = TRUE]
  df$row <- seq_len(nrow(df))
  
  all_period_starts <- max(df$date, na.rm = TRUE) %m-% months(period_length_months * 1:999) + 1
  all_period_starts <- c(all_period_starts[all_period_starts >= min(df$date, na.rm = TRUE)],
                         min(df$date, na.rm = TRUE))
  all_period_starts <- sort(unique(all_period_starts))
  
  # some checks
  if (length(all_period_starts) == 1) {
    warning("All cases are within one period - no clusters found.")
    return(structure(empty_output,
                     threshold_percentile = threshold_percentile,
                     based_on_historic_maximum = based_on_historic_maximum,
                     remove_outliers = remove_outliers,
                     remove_outliers_coefficient = remove_outliers_coefficient,
                     moving_average_days = moving_average_days,
                     minimum_cases = minimum_cases,
                     minimum_days = minimum_days,
                     minimum_case_days = minimum_case_days,
                     minimum_case_fraction_in_period = minimum_case_fraction_in_period,
                     period_length_months = period_length_months,
                     class = "detect_disease_clusters"))
  }
  if (n_distinct(df$patient) == nrow(df)) {
    warning("Only unique patients found - this is uncommon. Did you summarise accidentally on patient IDs?")
  }
  
  df_details <- df |>
    group_by(date) |>
    summarise(cases = n_distinct(patient), .groups = "drop") |> 
    arrange(desc(date)) |> 
    complete(date = seq(from = min(as.Date(date), na.rm = TRUE),
                        to = max(as.Date(date), na.rm = TRUE),
                        by = "1 day"),
             fill = list(cases = 0)) |>
    mutate(period = length(all_period_starts) - findInterval(x = date, vec = all_period_starts, rightmost.closed = FALSE)) |>
    group_by(period) |>
    mutate(day_in_period = row_number()) |>
    ungroup()
  
  if (nrow(df_details) < 2 || nrow(df_details) < minimum_case_days) {
    # too few cases
    return(structure(empty_output,
                     threshold_percentile = threshold_percentile,
                     based_on_historic_maximum = based_on_historic_maximum,
                     remove_outliers = remove_outliers,
                     remove_outliers_coefficient = remove_outliers_coefficient,
                     moving_average_days = moving_average_days,
                     minimum_cases = minimum_cases,
                     minimum_days = minimum_days,
                     minimum_case_days = minimum_case_days,
                     minimum_case_fraction_in_period = minimum_case_fraction_in_period,
                     period_length_months = period_length_months,
                     class = "detect_disease_clusters"))
  }
  
  df_details <- df_details |>
    mutate(moving_avg = moving_average(cases, w = min(length(cases) - 1, moving_average_days), side = moving_average_side, na.rm = TRUE),
           year = year(date),
           period_date = unify_years(date)) |> 
    group_by(period)
  max_period_length <- df_details |> count() |> pull(n) |> max()
  
  df_details <- df_details |> 
    arrange(desc(date)) |> 
    mutate(is_outlier = moving_avg %in% grDevices::boxplot.stats(moving_avg[!is.na(moving_avg)], coef = remove_outliers_coefficient)$out) |>
    arrange(date) |> 
    group_by(period_date) |>
    mutate(moving_avg_max = ifelse(length(moving_avg[!is.na(moving_avg) & period != 0 & !(remove_outliers & is_outlier)]) > 0,
                                   max(moving_avg[!is.na(moving_avg) & period != 0 & !(remove_outliers & is_outlier)], na.rm = TRUE),
                                   NA_real_)) |>
    ungroup() |> 
    mutate(moving_avg_limit = ifelse(
      isTRUE(based_on_historic_maximum),
      quantile(moving_avg_max, threshold_percentile / 100, na.rm = TRUE),
      quantile(moving_avg[!is.na(moving_avg) & period != 0 & !(remove_outliers & is_outlier)], threshold_percentile / 100, na.rm = TRUE))) |> 
    group_by(week = format2(date, "yyyy-ww")) |>
    mutate(cases_week = sum(cases, na.rm = TRUE)) |> 
    ungroup() |> 
    mutate(moving_avg_pctile = ntiles(moving_avg, 0.001, na.rm = TRUE) / 10,
           period_end = max(df$date, na.rm = TRUE) - months(abs(period) * period_length_months),
           period_start = period_end - months(period_length_months) + days(1),
           period_start = dplyr::if_else(period == min(period, na.rm = TRUE), min(date, na.rm = TRUE), period_start),
           period_txt = paste0("**Periode ", abs(period), ":** ", format2(period_start, "d mmm \u2019yy"), " - ", format2(period_end, "d mmm \u2019yy")))
  
  df_current_period <- df_details |> 
    filter(period == 0,
           moving_avg > moving_avg_limit,
           cases > 0)
  
  if (nrow(df_current_period) == 0) {
    df_clusters <- empty_output$clusters
  } else {
    df_clusters <- df_current_period |> 
      mutate(cluster = get_episode(date, case_free_days = case_free_days)) |> 
      group_by(cluster) |> 
      filter(n_distinct(date) >= minimum_case_days,
             sum(cases, na.rm = TRUE) >= minimum_cases,
             (sum(cases, na.rm = TRUE) / sum(df_current_period$cases, na.rm = TRUE)) >= minimum_case_fraction_in_period) |>
      ungroup()
    
    if (nrow(df_clusters) == 0) {
      df_clusters <- empty_output$clusters
    } else {
      df_clusters <- df_clusters |> 
        # determine episodes again, since some might have been filtered
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
        filter(days >= minimum_days)
    }
  }
  
  clusters <- df_clusters |> select(all_of(colnames(empty_output$clusters)))
  details <- df_details |> select(all_of(colnames(empty_output$details)))
  
  structure(list(clusters = clusters, details = details),
            threshold_percentile = threshold_percentile,
            based_on_historic_maximum = based_on_historic_maximum,
            remove_outliers = remove_outliers,
            remove_outliers_coefficient = remove_outliers_coefficient,
            moving_average_days = moving_average_days,
            minimum_cases = minimum_cases,
            minimum_days = minimum_days,
            minimum_case_days = minimum_case_days,
            minimum_case_fraction_in_period = minimum_case_fraction_in_period,
            period_length_months = period_length_months,
            class = "detect_disease_clusters")
}

#' @importFrom dplyr n_distinct
#' @rdname detect_disease_clusters
#' @param x output of [detect_disease_clusters()]
#' @export
n_clusters <- function(x) {
  n_distinct(x$clusters$cluster)
}

#' @rdname detect_disease_clusters
#' @param n number of clusters, defaults to 1
#' @export
has_clusters <- function(x, n = 1) {
  n_clusters(x) >= n
}

#' @rdname detect_disease_clusters
#' @param dates date(s) to test whether any of the clusters currently has this date in it, defaults to yesterday.
#' @details
#' The function [has_ongoing_cluster()] returns a [logical] vector with the same length as `dates`, so `dates` can have any length.
#' @export
has_ongoing_cluster <- function(x, dates = Sys.Date() - 1) {
  dates <- as.Date(dates)
  out <- format(x)
  vapply(FUN.VALUE = logical(1),
         dates,
         function(dt) any(out$first_day <= dt & out$last_day >= dt, na.rm = TRUE))
}

#' @param date date to test whether there are any clusters since or until this date.
#' @rdname detect_disease_clusters
#' @export
has_cluster_before <- function(x, date) {
  out <- format(x)
  any(out$first_day < as.Date(date), na.rm = TRUE)
}

#' @rdname detect_disease_clusters
#' @export
has_cluster_after <- function(x, date) {
  out <- format(x)
  any(out$last_day > as.Date(date), na.rm = TRUE)
}

#' @noRd
#' @importFrom dplyr group_by summarise
#' @export
format.detect_disease_clusters <- function(x, ...) {
  if (nrow(x$clusters) == 0) {
    data.frame(cluster = integer(0),
               first_day = Sys.Date()[0],
               last_day = Sys.Date()[0],
               first_day_in_period = integer(0),
               last_day_in_period = integer(0),
               cases = integer(0),
               days = integer(0),
               case_days = integer(0))
  } else {
    x$clusters |> 
      group_by(cluster) |> 
      summarise(first_day = min(date, na.rm = TRUE),
                last_day = max(date, na.rm = TRUE),
                first_day_in_period = min(day_in_period, na.rm = TRUE),
                last_day_in_period = max(day_in_period, na.rm = TRUE),
                cases = sum(cases, na.rm = TRUE),
                days = max(days, na.rm = TRUE),
                case_days = max(case_days, na.rm = TRUE))
  }
}

#' @importFrom dplyr case_when
#' @importFrom certestyle format2
#' @importFrom cli cli_ol cli_ul cli_li cli_text cli_end cli_h2
#' @noRd
#' @export
print.detect_disease_clusters <- function(x, ...) {
  out <- format(x)
  signed_nr <- function(n) {
    n <- format(n)
    last_char <- substr(n, nchar(n), nchar(n))
    paste0(n,
           case_when(last_char == "1" ~ "st",
                     last_char == "2" ~ "nd",
                     last_char == "3" ~ "rd",
                     TRUE ~ "th"))
  }
  format_Inf <- function(x) {
    ifelse(is.infinite(x) || x == 0, "any number of", paste0(">=", format(x)))
  }
  intro <- paste0("=> Detected ",
                  ifelse(nrow(out) == 0,
                         "no disease clusters ",
                         ifelse(nrow(out) == 1,
                                "1 disease cluster",
                                paste0(nrow(out), " disease clusters"))),
                  " over the last ", attributes(x)$period_length_months, "-month period")
  
  opts <- attributes(x)
  opts <- opts[!names(opts) %in% c("names", "class")]
  opts <- sort(paste(paste0(names(opts), ":"), opts))
  
  if (nrow(out) > 0) {
    cli_text(intro, " with a total of ", sum(out$cases), " cases.")
    dates <- function(x, y) {
      if (format2(x, "yyyy-mm") == format2(y, "yyyy-mm")) {
        return(paste0(format2(x, "d"), " and ", format2(y, "d mmmm yyyy")))
      } else if (format2(x, "yyyy") == format2(y, "yyyy")) {
        return(paste0(format2(x, "d mmmm"), " and ", format2(y, "d mmmm yyyy")))
      } else {
        return(paste0(format2(x, "d mmmm yyyy"), " and ", format2(y, "d mmmm yyyy")))
      }
    }
    cli_h2("Disease Clusters")
    cli_text("These disease clusters were found:")
    ol <- cli_ol()
    for (i in seq_len(nrow(out))) {
      cli_li("Between {dates(out$first_day[i], out$last_day[i])}: {.strong {out$cases[i]} cases}")
    }
    cli_end(ol)
    
    cli_h2("Parameters Used")
    ul <- cli_ul()
    cli_li(opts)
    cli_end(ul)
    
    cli_h2("Summary")
    cli_text("In total {.strong {sum(out$cases)} cases} between {dates(min(out$first_day), max(out$last_day))}, spread over {nrow(out)} cluster(s).")
    cli_text("Use {.fn certeplot2::plot2} to plot the results.")
  } else {
    cli_text(intro)
    invisible(NULL)
  }
}

#' Detect Unexpected Changes in Biomarkers
#' 
#' The `detect_biomarker_changes` function is designed to detect unexpected changes in biomarker values for individual patients based on clinical chemistry data. It will flag cases where patients' biomarker results deviate from defined thresholds or exhibit significant changes within a specified time window.
#' @inheritParams detect_disease_clusters
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
#' The [format()] function allows you to format the results of the `detect_biomarker_changes()` function for better readability and analysis. It organises the flag information into a structured data frame for easier inspection.
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
#' @seealso [detect_disease_clusters()]
#' @rdname detect_biomarker_changes
#' @export
#' @examples
#' data <- data.frame(date = Sys.Date() + 1:10,
#'                    patient = "test",
#'                    value = c(10,12,14,15,13,21,22,19,14,12))
#' 
#' check <- data |> detect_biomarker_changes(window_days = 6, max_delta_absolute = 10)
#' 
#' check
#' 
#' unlist(check)
detect_biomarker_changes <- function(df,
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
                   class = c("detect_biomarker_changes", "list")))
}

#' @importFrom dplyr bind_rows as_tibble
#' @noRd
#' @export
format.detect_biomarker_changes <- function(x, ...) {
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
print.detect_biomarker_changes <- function(x, ...) {
  cat("A total of ", length(x$flags), " patients with a total of ",
      sum(vapply(FUN.VALUE = integer(1), x$flags, nrow)), " flags.\n\n", sep = "")
  print(format(x), ...)
}
