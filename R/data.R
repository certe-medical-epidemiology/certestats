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

#' Example Data Set with ESBL Test Outcomes
#' 
#' This data set contains phenotypic test outcomes of the presence of ESBL (Extended Spectrum Beta-Lactamase) genes, with the minimum inhibitory concentrations (MIC, in mg/L) for 17 antibiotic drugs.
#' @format A [tibble]/[data.frame] with `r nrow(esbl_tests)` observations and `r ncol(esbl_tests)` variables:
#' - `esbl`: Outcome of ESBL test (`TRUE` (n = 250), `FALSE` (n = 250))
#' - `genus`: Taxonomic genus of the bacteria (`r paste0("*", names(table(esbl_tests$genus)), "* (n = ", table(esbl_tests$genus), ")", collapse = ", ")`)
#' - `AMC`: MIC values of amoxicillin/clavulanic acid
#' - `AMP`: MIC values of ampicillin
#' - `TZP`: MIC values of piperacillin/tazobactam
#' - `CXM`: MIC values of cefuroxime
#' - `FOX`: MIC values of cefoxitin
#' - `CTX`: MIC values of cefotaxime
#' - `CAZ`: MIC values of ceftazidime
#' - `GEN`: MIC values of gentamicin
#' - `TOB`: MIC values of tobramycin
#' - `TMP`: MIC values of trimethoprim
#' - `SXT`: MIC values of trimethoprim/sulfamethoxazole
#' - `NIT`: MIC values of nitrofurantoin
#' - `FOS`: MIC values of fosfomycin
#' - `CIP`: MIC values of ciprofloxacin
#' - `IPM`: MIC values of imipenem
#' - `MEM`: MIC values of meropenem
#' - `COL`: MIC values of colistin
#' @examples 
#' head(esbl_tests)
"esbl_tests"
