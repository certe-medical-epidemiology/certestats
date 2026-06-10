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

#' Detect Aberrations Using the Farrington Algorithm
#'
#' Detect aberrations (unexpected increases) in surveillance count data using
#' the Farrington algorithm (Farrington et al., 1996) or its improved flexible
#' variant (Noufaily et al., 2012). For time series
#' without sufficient historic baselines, the EARS methods (C1, C2, C3) are
#' available as a fallback via the `method` argument.
#'
#' The data are internally converted from a line list (one row per isolate or
#' case) to an aggregated `sts` object from the \pkg{surveillance} package,
#' which is then passed to [surveillance::farringtonFlexible()] or
#' [surveillance::earsC()].
#'
#' Use [has_farrington_clusters()] to return `TRUE` or `FALSE` based on the
#' output, or employ [format()] to format the result into a summary data frame.
#' Use [autoplot()][autoplot.farrington_clusters] for a `ggplot2` visualisation,
#' or [plot()] for the base graphics version.
#'
#' @param df Data set. This must consist of **only positive results**. The
#'   minimal data set should include a date column and a patient column. Do not
#'   summarise on patient IDs; deduplication to unique patient-dates is handled
#'   automatically.
#' @param column_date Name of the column to use for dates. If left blank, the
#'   first date column will be used.
#' @param column_patientid Name of the column to use for patient IDs. If left
#'   blank, the first column resembling `"patient|patid"` will be used.
#' @param method Detection method to use. One of:
#'   - `"farrington"` (default): the improved Farrington Flexible method
#'     (Noufaily et al., 2012), suitable when >= 3 years of historic data are
#'     available.
#'   - `"ears_c1"`, `"ears_c2"`, `"ears_c3"`: the CDC EARS methods, suitable
#'     as a fallback for short time series without years of baseline data.
#' @param frequency Number of observations per year. Use `52` for weekly data
#'   (the default) or `12` for monthly data.
#' @param years_back Number of years back in time to include for the baseline
#'   (Farrington only). Defaults to `5` for the flexible method.
#' @param window_width Total width of the reference window around the current
#'   period in each reference year (Farrington only). The default of `7` means
#'   3 periods before, the current period, and 3 periods after. Internally
#'   converted to the half-size `w = round((window_width - 1) / 2)` before
#'   passing to [surveillance::farringtonFlexible()].
#' @param reweight A [logical] indicating whether to perform the reweighting
#'   step to down-weight past outbreaks (Farrington only). Defaults to `TRUE`,
#'   which is the Noufaily et al. (2012) recommendation.
#' @param alpha Significance level for the one-sided prediction interval.
#'   Defaults to `0.05` for Farrington and `0.001` for EARS C1/C2, `0.025`
#'   for EARS C3.
#' @param trend A [logical] indicating whether to include a time trend in the
#'   GLM (Farrington only). Defaults to `TRUE`.
#' @param population_offset A [logical] indicating whether to include a
#'   population offset in the GLM (Farrington only). Defaults to `FALSE`. If
#'   `TRUE`, a `column_population` must be provided or the `sts` object must
#'   have a populated `populationFrac` slot.
#' @param n_periods Number of reference periods in the factor variable for the
#'   baseline (Farrington Flexible only). Defaults to `1`, which corresponds to
#'   the original Farrington et al. (1996) definition. Setting this to e.g. `10`
#'   expands the reference window, which can be useful for large regions with
#'   more data.
#' @param past_periods_ignored Number of recent periods to exclude from the
#'   baseline to avoid influence of ongoing outbreaks (Farrington only).
#'   Defaults to `NULL`, which uses the value of `w`. Noufaily et al. (2012)
#'   advise `26` for weekly data.
#' @param threshold_method Method to derive the upper bound. One of `"delta"`
#'   (Farrington et al., 1996, the default), `"nbPlugin"` (Noufaily et al.,
#'   2012), or `"muan"` (extended from Noufaily et al., 2012).
#' @param case_free_days Number of case-free days to separate distinct
#'   aberration episodes. Passed to [AMR::get_episode()]. Defaults to `14`.
#' @param minimum_cases Minimum number of cases for an aberration episode to be
#'   retained. Defaults to `1`.
#' @param minimum_duration Minimum number of days (inclusive) for an aberration
#'   episode to be retained. Defaults to `1`.
#' @param range Index of timepoints to monitor. If `NULL` (the default), the
#'   last `frequency` timepoints (i.e. the most recent year) are evaluated.
#' @param ... Additional arguments passed to [surveillance::farringtonFlexible()]
#'   or [surveillance::earsC()].
#' @details
#' ## Farrington (recommended)
#'
#' The Farrington algorithm is the standard method for automated aberration
#' detection in European infectious disease surveillance, used by ECDC, Public
#' Health England, and the Robert Koch Institute (RKI), among others. For each
#' evaluated time point, a quasi-Poisson GLM is fitted to reference counts from
#' the same calendar period in previous years. The predicted count and its
#' overdispersion are used to derive an upper threshold via a
#' variance-stabilising transformation. An aberration is flagged when the
#' observed count exceeds this threshold.
#'
#' ### Farrington Flexible
#'
#' The improved "Flexible" variant (Noufaily et al., 2012) adds reweighting of
#' past outbreaks so that a historic epidemic does not inflate the baseline and
#' mask future events.
#'
#' ## EARS (fallback for short series)
#'
#' The Early Aberration Reporting System (EARS) methods from the CDC are
#' Shewhart-type control charts that only require counts from the recent past
#' (default: 7 time points). They are useful when insufficient historic data
#' are available for the Farrington approach.
#'
#' ## Aberration clusters
#'
#' After the surveillance algorithm flags individual time points as aberrations,
#' consecutive (or near-consecutive) aberrations are grouped into clusters using
#' [AMR::get_episode()] with the `case_free_days` parameter. This produces
#' operationally useful clusters with start dates, end dates, case counts, and
#' durations, analogous to [detect_disease_clusters()].
#'
#' @references
#' - Farrington CP, Andrews NJ, Beale AD, Catchpole MA (1996). A statistical
#'   algorithm for the early detection of outbreaks of infectious disease.
#'   *J. R. Statist. Soc. A*, **159**, 547-563.
#'
#' - Noufaily A, Enki DG, Farrington CP, Garthwaite PH, Andrews NJ, Charlett A
#'   (2012). An improved algorithm for outbreak detection in multiple
#'   surveillance systems. *Statistics in Medicine*, **32**(7), 1206-1222.
#'
#' - Salmon M, Schumacher D, Hohle M (2016). Monitoring count time series in R:
#'   Aberration detection in public health surveillance. *Journal of Statistical
#'   Software*, **70**(10), 1-35.
#'
#' @importFrom dplyr n_distinct group_by summarise filter mutate ungroup select
#'   arrange tibble
#' @importFrom lubridate days
#' @importFrom AMR get_episode
#' @importFrom certestyle format2
#' @seealso [detect_disease_clusters()], [surveillance::farringtonFlexible()],
#'   [surveillance::earsC()]
#' @rdname detect_farrington
#' @export
#' @examples
#' # generate example line list data spanning several years
#' set.seed(123)
#' cases <- data.frame(
#'   date = sample(seq(as.Date("2018-01-01"),
#'                     as.Date("2024-12-31"),
#'                     "1 day"),
#'                 size = 500,
#'                 replace = TRUE),
#'   patient = sample(LETTERS, size = 500, replace = TRUE)
#' )
#'
#' # --- Farrington Flexible (default) ---
#' result <- detect_farrington(cases)
#' result
#' has_farrington_clusters(result)
#' n_farrington_clusters(result)
#' format(result)
#'
#' # check for ongoing cluster
#' has_ongoing_farrington_cluster(result, Sys.Date() - 1)
#'
#' # plot the results
#' plot(result)
#' if (require("ggplot2")) autoplot(result)
#'
#' # --- EARS C2 (short baseline) ---
#' recent <- cases[cases$date >= as.Date("2024-06-01"), ]
#' result_ears <- detect_farrington(recent, method = "ears_c2")
#' result_ears
#' if (require("ggplot2")) autoplot(result_ears)
#'
#' # --- Farrington with expanded reference window for large regions ---
#' result_large <- detect_farrington(cases, n_periods = 10,
#'                                   past_periods_ignored = 26,
#'                                   threshold_method = "nbPlugin")
#' result_large
#' if (require("ggplot2")) autoplot(result_large)
detect_farrington <- function(df,
                              column_date = NULL,
                              column_patientid = NULL,
                              method = "farrington",
                              frequency = 52,
                              years_back = 5,
                              window_width = 7,
                              reweight = TRUE,
                              alpha = NULL,
                              trend = TRUE,
                              population_offset = FALSE,
                              n_periods = 1,
                              past_periods_ignored = NULL,
                              threshold_method = "delta",
                              case_free_days = 14,
                              minimum_cases = 1,
                              minimum_duration = 1,
                              range = NULL,
                              ...) {
  
  # validate method
  method <- tolower(trimws(method[1]))
  valid_methods <- c("farrington", "ears_c1", "ears_c2", "ears_c3")
  if (!method %in% valid_methods) {
    stop("method must be one of: ", paste0('"', valid_methods, '"', collapse = ", "),
         call. = FALSE)
  }
  
  # require the surveillance package
  if (!requireNamespace("surveillance", quietly = TRUE)) {
    stop("This function requires the 'surveillance' package. ",
         "Install it with: install.packages('surveillance')",
         call. = FALSE)
  }
  
  # --- resolve column names ---
  if (is.null(column_date)) {
    date_cols <- vapply(df, FUN.VALUE = logical(1),
                        inherits, c("Date", "POSIXt"))
    if (!any(date_cols)) {
      stop("No date column found.", call. = FALSE)
    }
    column_date <- names(date_cols[which(date_cols)][1])
    message("Using column '", column_date, "' for dates")
  }
  if (is.null(column_patientid)) {
    pat_cols <- colnames(df) %like% "patient|patid"
    if (!any(pat_cols)) {
      stop("No patient column found.", call. = FALSE)
    }
    column_patientid <- colnames(df)[which(pat_cols)][1]
    message("Using column '", column_patientid, "' for patient IDs")
  }
  
  # --- aggregate to counts per time unit ---
  df$`.date` <- as.Date(df[, column_date, drop = TRUE])
  df$`.patient` <- df[, column_patientid, drop = TRUE]
  
  date_range <- range(df$`.date`, na.rm = TRUE)
  
  # aggregate: unique patients per date, then per week/month
  if (frequency == 52) {
    aggregate_by <- "1 week"
  } else if (frequency == 12) {
    aggregate_by <- "1 month"
  } else {
    aggregate_by <- "1 day"
  }
  
  # use linelist2sts for the conversion, it handles epoch alignment properly
  linelist_df <- data.frame(date = df$`.date`,
                            patient = df$`.patient`)
  # deduplicate to unique patients per date
  linelist_df <- unique(linelist_df)
  
  sts_obj <- surveillance::linelist2sts(
    linelist = linelist_df,
    dateCol = "date",
    aggregate.by = aggregate_by
  )
  
  n_obs <- nrow(sts_obj)
  
  # --- determine range to evaluate ---
  # convert window_width to half-size w for the surveillance package
  w <- round((window_width - 1) / 2, 0)
  
  if (is.null(range)) {
    if (method == "farrington") {
      # evaluate the most recent year, but ensure enough baseline
      min_baseline <- (years_back * frequency) + (2 * w) + 1
      if (n_obs <= min_baseline) {
        warning("Insufficient data for Farrington with years_back=", years_back,
                " and window_width=", window_width, ". Only ", n_obs,
                " time points available, need at least ", min_baseline + 1, ". ",
                "Consider using method='ears_c2' or reducing years_back.",
                call. = FALSE)
        range <- max(1, n_obs):n_obs
      } else {
        range_start <- max(min_baseline + 1, n_obs - frequency + 1)
        range <- range_start:n_obs
      }
    } else {
      # EARS methods: need baseline + a few lag periods
      ears_baseline <- 7
      ears_lag <- switch(method,
                         ears_c1 = 1,
                         ears_c2 = 3,
                         ears_c3 = 5)
      range_start <- min(ears_baseline + ears_lag + 1, n_obs)
      range <- range_start:n_obs
    }
  }
  
  # --- run the detection algorithm ---
  if (method == "farrington") {
    if (is.null(alpha)) alpha <- 0.05
    
    control <- list(
      range = range,
      b = years_back,
      w = w,
      reweight = reweight,
      weightsThreshold = 2.58,
      verbose = FALSE,
      glmWarnings = FALSE,
      alpha = alpha,
      trend = trend,
      pThresholdTrend = 0.05,
      limit54 = c(5, 4),
      powertrans = "2/3",
      fitFun = "algo.farrington.fitGLM.flexible",
      populationOffset = population_offset,
      noPeriods = n_periods,
      pastWeeksNotIncluded = past_periods_ignored,
      thresholdMethod = threshold_method
    )
    
    sts_result <- surveillance::farringtonFlexible(sts_obj, control = control)
    
  } else {
    # EARS
    ears_method <- switch(method,
                          ears_c1 = "C1",
                          ears_c2 = "C2",
                          ears_c3 = "C3")
    
    control <- list(
      range = range,
      method = ears_method,
      baseline = 7,
      minSigma = 0,
      alpha = alpha  # NULL is handled by earsC itself
    )
    
    sts_result <- surveillance::earsC(sts_obj, control = control)
  }
  
  # --- extract aberrations and build cluster episodes ---
  alarm_vec <- as.logical(surveillance::alarms(sts_result)[, 1])
  observed_vec <- surveillance::observed(sts_result)[, 1]
  upperbound_vec <- surveillance::upperbound(sts_result)[, 1]
  
  # get dates from the sts result
  epoch_dates <- surveillance::epoch(sts_result, as.Date = TRUE)
  
  aberration_dates <- epoch_dates[alarm_vec]
  aberration_cases <- observed_vec[alarm_vec]
  aberration_upper <- upperbound_vec[alarm_vec]
  
  if (length(aberration_dates) == 0) {
    aberrations <- tibble(
      cluster = integer(0),
      date = as.Date(character(0)),
      cases = integer(0),
      upperbound = numeric(0)
    )
  } else {
    aberrations <- tibble(
      date = aberration_dates,
      cases = as.integer(aberration_cases),
      upperbound = aberration_upper
    ) |>
      arrange(date) |>
      mutate(cluster = get_episode(date, case_free_days = case_free_days))
  }
  
  # --- build cluster summary ---
  if (nrow(aberrations) == 0) {
    clusters <- tibble(
      cluster = integer(0),
      first_day = as.Date(character(0)),
      last_day = as.Date(character(0)),
      cases = integer(0),
      aberrations = integer(0),
      duration_days = integer(0)
    )
  } else {
    clusters <- aberrations |>
      group_by(cluster) |>
      summarise(
        first_day = min(date, na.rm = TRUE),
        last_day = max(date, na.rm = TRUE),
        cases = sum(cases, na.rm = TRUE),
        aberrations = dplyr::n(),
        .groups = "drop"
      ) |>
      mutate(duration_days = as.integer(last_day - first_day + 1)) |>
      # apply minimum filters
      filter(cases >= minimum_cases,
             duration_days >= minimum_duration) |>
      ungroup()
    
    if (nrow(clusters) > 0) {
      # renumber clusters sequentially after filtering
      clusters$cluster <- seq_len(nrow(clusters))
    }
  }
  
  # --- assemble the details data frame ---
  details <- tibble(
    date = epoch_dates,
    observed = as.integer(observed_vec),
    upperbound = upperbound_vec,
    aberration = alarm_vec
  )
  
  structure(
    list(
      clusters = clusters,
      aberrations = aberrations,
      details = details,
      sts = sts_result
    ),
    method = method,
    frequency = frequency,
    alpha = alpha,
    years_back = if (method == "farrington") years_back else NA_integer_,
    window_width = if (method == "farrington") window_width else NA_integer_,
    reweight = if (method == "farrington") reweight else NA,
    n_periods = if (method == "farrington") n_periods else NA_integer_,
    threshold_method = if (method == "farrington") threshold_method else NA_character_,
    case_free_days = case_free_days,
    minimum_cases = minimum_cases,
    minimum_duration = minimum_duration,
    class = "farrington_clusters"
  )
}

#' @rdname detect_farrington
#' @param x output of [detect_farrington()]
#' @export
n_farrington_clusters <- function(x) {
  stopifnot(inherits(x, "farrington_clusters"))
  nrow(x$clusters)
}

#' @rdname detect_farrington
#' @param n minimum number of clusters, defaults to 1
#' @export
has_farrington_clusters <- function(x, n = 1) {
  n_farrington_clusters(x) >= n
}

#' @rdname detect_farrington
#' @param dates date(s) to test whether any cluster currently spans this date.
#'   Defaults to yesterday. Returns a [logical] vector with the same length as
#'   `dates`.
#' @export
has_ongoing_farrington_cluster <- function(x, dates = Sys.Date() - 1) {
  stopifnot(inherits(x, "farrington_clusters"))
  dates <- as.Date(dates)
  cl <- x$clusters
  vapply(
    FUN.VALUE = logical(1),
    dates,
    function(dt) any(cl$first_day <= dt & cl$last_day >= dt, na.rm = TRUE)
  )
}

#' @rdname detect_farrington
#' @param date a single date to test whether there are any clusters before or
#'   after this date.
#' @export
has_farrington_cluster_before <- function(x, date) {
  stopifnot(inherits(x, "farrington_clusters"))
  any(x$clusters$first_day < as.Date(date), na.rm = TRUE)
}

#' @rdname detect_farrington
#' @export
has_farrington_cluster_after <- function(x, date) {
  stopifnot(inherits(x, "farrington_clusters"))
  any(x$clusters$last_day > as.Date(date), na.rm = TRUE)
}

#' @noRd
#' @export
format.farrington_clusters <- function(x, ...) {
  x$clusters
}

#' @noRd
#' @importFrom cli cli_text cli_h2 cli_ol cli_li cli_ul cli_end
#' @importFrom certestyle format2
#' @export
print.farrington_clusters <- function(x, ...) {
  
  method_label <- switch(
    attributes(x)$method,
    farrington = "Farrington Flexible",
    ears_c1 = "EARS C1",
    ears_c2 = "EARS C2",
    ears_c3 = "EARS C3"
  )
  
  cl <- x$clusters
  n_cl <- nrow(cl)
  n_aberrations <- sum(x$details$aberration, na.rm = TRUE)
  
  dates_fn <- function(d1, d2) {
    if (format2(d1, "yyyy-mm") == format2(d2, "yyyy-mm")) {
      paste0(format2(d1, "d"), " and ", format2(d2, "d mmmm yyyy"))
    } else if (format2(d1, "yyyy") == format2(d2, "yyyy")) {
      paste0(format2(d1, "d mmmm"), " and ", format2(d2, "d mmmm yyyy"))
    } else {
      paste0(format2(d1, "d mmmm yyyy"), " and ", format2(d2, "d mmmm yyyy"))
    }
  }
  
  print(n_cl)
  intro <- "=> Detected {cli::no(n_cl)} cluster{?s} using {method_label} ({n_aberrations} aberration{?s} across {nrow(x$details)} evaluated time point{?s})"
  
  if (n_cl > 0) {
    cli_text(intro, " with a total of {sum(cl$cases)} case{?s}.")
    
    cli_h2("{method_label} Clusters")
    cli_text("These clusters were found:")
    ol <- cli_ol()
    for (i in seq_len(n_cl)) {
      cli_li("Between {dates_fn(cl$first_day[i], cl$last_day[i])}: {.strong {cl$cases[i]} cases} ({cl$aberrations[i]} aberration(s), {cl$duration_days[i]} days)")
    }
    cli_end(ol)
    
    cli_h2("Parameters Used")
    attrs <- attributes(x)
    opts <- c(
      paste0("method: ", attrs$method),
      paste0("frequency: ", attrs$frequency),
      paste0("alpha: ", attrs$alpha),
      if (!is.na(attrs$years_back)) paste0("years_back: ", attrs$years_back),
      if (!is.na(attrs$window_width)) paste0("window_width: ", attrs$window_width),
      if (!is.na(attrs$reweight)) paste0("reweight: ", attrs$reweight),
      if (!is.na(attrs$n_periods)) paste0("n_periods: ", attrs$n_periods),
      if (!is.na(attrs$threshold_method)) paste0("threshold_method: ", attrs$threshold_method),
      paste0("case_free_days: ", attrs$case_free_days),
      paste0("minimum_cases: ", attrs$minimum_cases),
      paste0("minimum_duration: ", attrs$minimum_duration)
    )
    ul <- cli_ul()
    cli_li(opts)
    cli_end(ul)
    
    cli_h2("Summary")
    cli_text(
      "In total {.strong {sum(cl$cases)} cases} between ",
      "{dates_fn(min(cl$first_day), max(cl$last_day))}, ",
      "spread over {n_cl} cluster(s)."
    )
    cli_text("Use {.fn plot} or {.fn autoplot} on these results to visualise them.")
  } else {
    cli_text(intro)
  }
  
  invisible(x)
}

#' @noRd
#' @export
plot.farrington_clusters <- function(x, ...) {
  plot(x$sts, ...)
}

#' @rdname detect_farrington
#' @param object output of [detect_farrington()]
#' @importFrom ggplot2 autoplot ggplot aes geom_col geom_line geom_point
#'   geom_rect scale_fill_manual scale_colour_manual labs theme_minimal theme
#'   element_text element_line element_blank
#' @export
autoplot.farrington_clusters <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function requires the 'ggplot2' package.", call. = FALSE)
  }
  
  method_label <- switch(
    attributes(object)$method,
    farrington = "Farrington Flexible",
    ears_c1 = "EARS C1",
    ears_c2 = "EARS C2",
    ears_c3 = "EARS C3"
  )
  
  details <- object$details
  clusters <- object$clusters
  
  # base layer: observed counts as bars, coloured by aberration status
  details$status <- ifelse(details$aberration, "Aberration", "Normal")
  
  p <- ggplot(details, aes(x = date)) +
    # shaded cluster regions
    {
      if (nrow(clusters) > 0) {
        geom_rect(
          data = clusters,
          aes(xmin = first_day - 0.5, xmax = last_day + 0.5,
              ymin = -Inf, ymax = Inf),
          fill = "#E74C3C",
          alpha = 0.08,
          inherit.aes = FALSE
        )
      }
    } +
    # observed counts
    geom_col(
      aes(y = observed, fill = status),
      width = ifelse(attributes(object)$frequency == 52, 6, 25)
    ) +
    # upper bound threshold line
    geom_line(
      aes(y = upperbound),
      colour = "#C0392B",
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    # aberration points highlighted
    {
      aberration_data <- details[details$aberration, ]
      if (nrow(aberration_data) > 0) {
        geom_point(
          data = aberration_data,
          aes(y = observed),
          colour = "#C0392B",
          size = 2,
          shape = 17
        )
      }
    } +
    scale_fill_manual(
      values = c("Normal" = "#2C3E50", "Aberration" = "#E74C3C"),
      name = NULL
    ) +
    labs(
      title = paste0(method_label, " Aberration Detection"),
      subtitle = paste0(
        n_farrington_clusters(object), " cluster(s), ",
        sum(details$aberration, na.rm = TRUE), " aberration(s)"
      ),
      x = NULL,
      y = "Observed cases"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(colour = "grey40", size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line.x = element_line(colour = "grey70", linewidth = 0.3)
    )
  
  p
}
