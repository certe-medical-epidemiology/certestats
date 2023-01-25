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

#' Fast Regression Models
#' 
#' Functions to do fast regression modelling. The functions return a [tibble] with statistics. Use [plot()] for an extensive model visualisation.
#' @param x vector of values, or a data.frame
#' @param y vector of values, optional
#' @param type type of function to use, can be "lm" or "glm"
#' @param var1,var2 column to use of `x`, the `var2` argument is optional
#' @param family only used for [glm()]
#' @param object data to plot
#' @param ... arguments for [lm()] or [glm()]
#' @importFrom broom tidy
#' @rdname regression
#' @export
#' @examples 
#' runif(10) |> regression()
#' 
#' data.frame(x = 1:50, y = runif(50)) |>
#'   regression(x, y)
#' 
#' exponential_growth <- rexp(50, rate = 3)
#' exponential_growth |> plot()
#' 
#' exponential_growth |> regression()
#' 
#' exponential_growth |> regression() |> plot()
regression <- function(x, ...) {
  UseMethod("regression")
}

#' @export
#' @rdname regression
regression.default <- function(x, y = NULL, type = "lm", family = stats::gaussian, ...) {
  if (is.null(y)) {
    y <- seq_len(length(x))
  }
  if (type == "lm") {
    mdl <- stats::lm(y ~ x, ...)
  } else {
    mdl <- stats::glm(y ~ x, family = family, ...)
  }
  out <- tidy(mdl)
  structure(out,
            mdl = mdl,
            type = type,
            class = c("certestats_reg", class(out)))
}

#' @export
#' @importFrom dplyr select pull
#' @rdname regression
regression.data.frame <- function(x, var1, var2 = NULL, type = "lm", ...) {
  var_x <- x |> select({{ var1 }}) |> pull(1)
  if (tryCatch(is.null(var2), error = function(e) FALSE)) {
    var_y <- seq_len(length(var1))
  } else {
    var_y <- x |> select({{ var2 }}) |> pull(1)
  }
  regression(var_x, var_y, ...)
}

#' @export
#' @importFrom performance check_model
#  # we need {see} for {performance} but it is not installed automatically, so as a placeholder:
#' @importFrom see see_colors
#' @importFrom certestyle colourpicker
#' @rdname regression
plot.certestats_reg <- function(x, ...) {
  mdl <- attributes(x)$mdl
  check_model(mdl, colors = colourpicker(c("certeroze", "certeroze3", "certeblauw")))
}

#' @export
#' @importFrom certestyle colourpicker
#' @importFrom ggplot2 autoplot ggplot geom_point geom_smooth theme_minimal aes theme element_text
#' @rdname regression
autoplot.certestats_reg <- function(object, ...) {
  x <- object
  mdl <- attributes(x)$mdl
  type <- attributes(x)$type
  
  r2 <- summary(mdl)$`r.squared`
  p <- x$p.value[nrow(x)]
  
  out <- ggplot(data = mdl,
         mapping = aes(x = y, y = x)) +
    geom_point(colour = colourpicker("certeroze3")) +
    geom_smooth(method = ifelse(type == "lm", "lm", "loess"),
                colour = colourpicker("certeroze")) +
    theme_minimal() +
    labs(x = "x",
         y = "y",
         title = parse(text = paste0("R^2==", round(r2, 3))),
         subtitle = parse(text = paste0("p==", round(p, 3))))
  if (p >= 0.05) {
    out <- out + theme(plot.subtitle = element_text(colour = "red"))
  }
  out
}
