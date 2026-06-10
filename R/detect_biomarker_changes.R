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
