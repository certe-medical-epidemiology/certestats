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
#' This function can detect so-called [disease clusters](https://en.wikipedia.org/wiki/Disease_cluster). Use [format()] to format the output.
#' @param df data set, this must be a data set containing `only positive results`. The most bare-bone data set to use must have at least a date column and patient column. Do not summarise on patient IDs, this will happen automatically.
#' @param ... filter arguments to define the data to study, e.g., `jaar == 2023`. All other observations will be used as a control.
#' @param group_by column to use in [group_by()], e.g., a microorganism name
#' @param column_date name of the column to use for dates, will take the first date column if left blank
#' @param column_patientid name of the column to use for patient IDs, will take the first column names resembling `"patient|patid"` if left blank
#' @param based_on_historic_maximum [logical] to indicate whether the percentile must be based on the maximum of previous years. With `FALSE` (default), the percentile will be based on all historic data points.
#' @param minimum_ongoing_days number of days that must have passed above the set percentile before detecting the elevation as cluster
#' @param threshold_percentile threshold to set
#' @param remove_outliers [logical] to indicate whether outliers must be removed before threshold determination
#' @param remove_outliers_coefficient coefficient to use for outlier determination
#' @param moving_average_days number of days to set in [moving_average()]
#' @param moving_average_side side of days to set in [moving_average()]
#' @param case_free_days number of days to set in [`get_episode()`][AMR::get_episode()]
#' @details
#' A (disease) cluster is defined as an unusually large aggregation of disease events in time or space ([ATSDR, 2008](https://www.atsdr.cdc.gov/hec/csem/cluster/docs/clusters.pdf)). They are common, particularly in large populations. From a statistical standpoint, it is nearly inevitable that some clusters of chronic diseases will emerge within various communities, be it schools, church groups, social circles, or neighborhoods. Initially, these clusters are often perceived as products of specific, predictable processes rather than random occurrences in a particular location, akin to a coin toss.
#' 
#' Whether a (suspected) cluster corresponds to an actual increase of disease in the area, needs to be assessed by an epidemiologists or biostatistician ([ATSDR, 2008](https://www.atsdr.cdc.gov/hec/csem/cluster/docs/clusters.pdf)).
#' @importFrom dplyr n_distinct group_by summarise filter mutate ungroup select row_number
#' @importFrom tidyr fill complete
#' @importFrom certestyle format2
#' @importFrom AMR get_episode
#' @export
#' @examples
#' cases <- data.frame(date = sample(seq(as.Date("2018-01-01"),
#'                                       as.Date("2022-12-31"),
#'                                       "1 day"),
#'                                   size = 300),
#'                     patient = sample(LETTERS, size = 300, replace = TRUE))
#' check <- early_warning_cluster(cases,
#'                                date >= "2022-01-01")
#' 
#' format(check)
#' 
#' check2 <- early_warning_cluster(cases,
#'                                 date >= "2022-01-01",
#'                                 threshold_percentile = 0.75)
#' 
#' print(check2)
#' 
#' 
#' unclass(check2)
early_warning_cluster <- function(df,
                                  ...,
                                  group_by = NULL,
                                  column_date = NULL,
                                  column_patientid = NULL,
                                  based_on_historic_maximum = FALSE,
                                  minimum_ongoing_days = 2,
                                  threshold_percentile = 97.5,
                                  remove_outliers = TRUE,
                                  remove_outliers_coefficient = 1.5,
                                  moving_average_days = 5,
                                  moving_average_side = "centre",
                                  case_free_days = 14) {
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
      stop("No date column found")
    } else {
      column_patientid <- colnames(df)[which(column_patientid)][1]
      message("Using column '", column_patientid, "' for patient IDs")
    }
  }
  if (threshold_percentile < 1) {
    threshold_percentile <- threshold_percentile * 100
  }
  
  # add the new columns
  df.bak <- df
  df$date <- df[, column_date, drop = TRUE]
  df$patient <- df[, column_patientid, drop = TRUE]
  df$row <- seq_len(nrow(df))
  df$in_scope <- df$row %in% (df |> filter(...) |> pull(row))
  
  # some checks
  if (all(df$in_scope, na.rm = TRUE)) {
    stop("All cases are 'in scope'. Use ... to filter on specific cases.")
  }
  if (n_distinct(df$patient_id) == nrow(df)) {
    warning("Only unique patients found - this is uncommon. Did you summarise accidentally on patient IDs?")
  }
  
  df_early <- df |>
    group_by(date, in_scope) |>
    summarise(cases = n_distinct(patient), .groups = "drop") |> 
    complete(date = as.Date(min(date):max(date)), fill = list(cases = 0)) |>
    fill(in_scope, .direction = "down") |> 
    mutate(ma_5c = moving_average(cases, w = moving_average_days, side = moving_average_side, na.rm = TRUE),
           year = year(date),
           month_day = unify_years(date)) |> 
    group_by(in_scope) |> 
    mutate(is_outlier = ma_5c %in% grDevices::boxplot.stats(ma_5c[!is.na(ma_5c)], coef = remove_outliers_coefficient)$out) |> 
    group_by(month_day) |>
    mutate(max_ma_5c = max(ma_5c[!is.na(ma_5c) & !in_scope & !(remove_outliers & is_outlier)], na.rm = TRUE)) |> 
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
    clusters <- data.frame(date = Sys.Date()[0], cases = integer(0), cluster = integer(0), ongoing_days = integer(0))
  } else {
    clusters <- df_filter |> 
      mutate(cluster = get_episode(date, case_free_days = case_free_days)) |> 
      group_by(cluster) |> 
      filter(difftime(max(date), min(date), units = "days") >= minimum_ongoing_days)
    
    if (nrow(clusters) == 0) {
      clusters <- data.frame(date = Sys.Date()[0], cases = integer(0), cluster = integer(0), ongoing_days = integer(0))
    } else {
      clusters <- clusters |> 
        # determine episodes again, since some might have been filtered
        ungroup() |> 
        mutate(cluster = get_episode(date, case_free_days = case_free_days)) |> 
        group_by(cluster) |> 
        mutate(ongoing_days = row_number()) |> 
        complete(date = as.Date(min(date):max(date)), fill = list(cases = 0)) |>
        fill(ongoing_days, .direction = "down") |> 
        ungroup() |> 
        select(date, cases, cluster, ongoing_days)
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
                 minimum_ongoing_days = minimum_ongoing_days,
                 class = c("early_warning_cluster", "list"))
  x
}

#' @noRd
#' @importFrom dplyr group_by summarise
#' @export
format.early_warning_cluster <- function(x, ...) {
  if (nrow(x$clusters) == 0) {
    data.frame(cluster = integer(0),
               first_day = Sys.Date()[0],
               last_day = Sys.Date()[0],
               ongoing_days = integer(0),
               total_cases = integer(0))
  } else {
    x$clusters |> 
      group_by(cluster) |> 
      summarise(first_day = min(date, na.rm = TRUE),
                last_day = max(date, na.rm = TRUE),
                ongoing_days = max(ongoing_days, na.rm = TRUE),
                total_cases = sum(cases, na.rm = TRUE))
  }
}

#' @importFrom dplyr case_when
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
                     ifelse(isTRUE(attributes(x)$based_on_historic_maximum),
                            "the historic maximum and ",
                            ""),
                     ifelse(isTRUE(attributes(x)$remove_outliers),
                            paste0("an outlier-free history (coeff = ", format(attributes(x)$remove_outliers_coefficient),
                                   ") and "),
                            ""),
                     "the ", signed_nr(attributes(x)$threshold_percentile), " pct",
                     " lasting for >=", attributes(x)$minimum_ongoing_days, " days.")
  if (nrow(out)) {
    message(intro, " with a total of ", sum(out$total_cases), " cases ", based_on)
    print(out)
    message("Use plot2() to plot the results.")
  } else {
    message(intro, based_on)
    invisible(NULL)
  }
}
