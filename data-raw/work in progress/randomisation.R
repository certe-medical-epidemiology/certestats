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

#' Apply (Startified or Block) Randomisation
# @export
#' @examples
#' library(dplyr)
#' 
#' starwars$group <- randomisation(starwars$sex, starwars$eye_color)
#' 
#' starwars |>
#'   mutate(group = randomisation(sex, eye_color))
randomisation <- function(..., block_size = NULL, levels = c("Study", "Control")) {
  lst <- list(...)
  if (any(vapply(FUN.VALUE = logical(1), lst, function(x) !is.vector(x) && !is.factor(x)), na.rm = TRUE)) {
    stop("all elements for randomisation must be vectors")
  }
  df <- as.data.frame(lst)
  
  strata <- interaction(df)
  out <- rep(levels[0][1], nrow(df))
  
  message("Applying stratified randomisation for ", length(unique(strata)), " strata")
  set.seed(123) # for reproducibility
  for (i in unique(strata)) {
    idx <- which(strata == i)
    out[idx] <- sample(levels, length(idx), replace = TRUE)
  }
  return(out)
}



randomisation2 <- function(data, ..., group_col = NULL, type = "normal", block_size = 4, levels = c("Study", "Control")) {
  
  # Create a column for the study group
  data$study_group <- NA_character_
  
  # Define the stratification variables
  strata <- data |> 
    select({{...}}) |>
    interaction()
  
  # Randomly assign participants to the study or control group within each stratum
  set.seed(123) # for reproducibility
  for (i in unique(strata)) {
    idx <- which(strata == i)
    data$study_group[idx] <- sample(levels, length(idx), replace = TRUE)
  }
  
  
  
  # Select columns to use for randomisation
  select_cols <- c(...)
  if (length(select_cols) == 0) {
    stop("At least one variable must be specified for randomisation.")
  }
  
  # Check for missing values in select columns
  missing_vals <- colSums(is.na(data[, select_cols, drop = FALSE]))
  if (any(missing_vals > 0)) {
    stop(paste0("Selected columns contain missing values: ", 
                paste(names(missing_vals[missing_vals > 0]), collapse = ", ")))
  }
  
  # Check for invalid type argument
  if (!type %in% c("normal", "stratified", "block")) {
    stop("Invalid value specified for 'type' argument.")
  }
  
  # Group data if using stratified randomisation
  if (type == "stratified") {
    data <- data %>% 
      group_by(across(all_of(select_cols))) %>% 
      nest()
  }
  
  # Create list of blocks for block randomisation
  if (type == "block") {
    block_list <- lapply(unique(data[, select_cols]), function(x) {
      idx <- which(apply(data[, select_cols] == x, 1, all))
      sample(idx)
    })
  }
  
  # Perform randomisation for each group
  data <- data %>% 
    mutate(study_group = NA)
  
  if (is.null(group_col)) {
    groups <- 1
  } else {
    groups <- unique(data[, group_col, drop = TRUE])
  }
  
  for (i in seq_along(groups)) {
    if (is.null(group_col)) {
      group_data <- data
    } else {
      group_data <- data %>%
        filter({{group_col}} == groups[i])
    }
    
    if (type == "normal") {
      group_data$study_group <- sample(c("Study", "Control"), nrow(group_data), replace = TRUE)
      
    } else if (type == "stratified") {
      group_data <- group_data %>%
        mutate(study_group = map(data, ~ {
          .x$study_group <- sample(c("Study", "Control"), nrow(.x))
        })) %>% 
            unnest(data) %>% 
            unnest(study_group)
          
    } else if (type == "block") {
      block_idx <- rep(seq_along(block_list), each = block_size, length.out = nrow(group_data))
      group_data$block_idx <- sample(block_idx)
      
      group_data <- group_data %>% 
        group_by(block_idx) %>% 
        mutate(study_group = sample(c("Study", "Control"), n(), replace = TRUE)) %>% 
        ungroup() %>% 
        select(-block_idx)
    }
    
    data[data[, group_col] == groups[i], ] <- group_data                 
  }
  
  # If using stratified randomisation, unnest data
  if (type == "stratified") {
    data <- data %>%
      unnest(data)
  }
  
  return(data)
  
}
