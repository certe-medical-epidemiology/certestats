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

#' Early Warning Algorithms
#' 
#' @param data kolom
#' @param col_patient kolom
#' @param col_date kolom
#' @param col_testcode kolom
#' @param testcode kolom
#' @param days kolom
#' @param threshold_top kolom
#' @param threshold_bottom kolom
#' @param max_delta_raw kolom
#' @param max_delta_percent kolom
#' @param direction kolom
#' @return De list 'early_warning' in de global environment, met de volgende elementen:
#' @export
#' @examples
#' df %>%
#'   early_warning_uni(testcode = "eGFR",
#'                     days = 30,
#'                     max_delta_percent = 20,
#'                     direction = "down") %>%
#'   early_warning_uni(testcode = "eGFR",
#'                     days = 10,
#'                     max_delta_percent = 5,
#'                     direction = "down")
early_warning_uni <- function(data,
                              col_patient = colnames(data)[colnames(data) %like% "pat(ient)?_?(id)?"][1L],
                              col_date = colnames(data)[sapply(data, inherits, "Data")][1L],
                              col_testcode = colnames(data)[colnames(data) %like% "test"][1L],
                              testcode,
                              days,
                              threshold_top = NULL,
                              threshold_bottom = NULL,
                              max_delta_raw = NULL,
                              max_delta_percent = NULL,
                              direction = "both") {
  if ("early_warning" %in% ls()) {
    stop("object 'early_warning' already exists")
  }
  cat("Data: ", paste(dim(data), collapse = "x"), "\n",
      "Column patient:      ", col_patient, "\n",
      "Column date:         ", col_date, "\n",
      "Column col_testcode: ", col_testcode, "\n",
      sep = "")

  l <- list()

  df <- data %>%
    filter(across(col_testcode, ~.x %in% testcode)) %>%
    group_by(across(col_patient)) %>%
    mutate(flag = case_when())


  return(data)
  
}
