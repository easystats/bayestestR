#' Overlap Coefficient
#'
#' A method to calculate the overlap coefficient between two empirical distributions (that can be used as a measure of similarity between two samples).
#'
#' @param x Vector of x values.
#' @param y Vector of x values.
#' @param method_auc Area Under the Curve (AUC) estimation method. See [area_under_curve()].
#' @param method_density Density estimation method. See [estimate_density()].
#' @inheritParams estimate_density
#'
#' @examples
#' library(bayestestR)
#'
#' x <- distribution_normal(1000, 2, 0.5)
#' y <- distribution_normal(1000, 0, 1)
#'
#' overlap(x, y)
#' plot(overlap(x, y))
#' @export
overlap <- function(x, y, method_density = "kernel", method_auc = "trapezoid", precision = 2^10, extend = TRUE, extend_scale = 0.1, ...) {
  # Generate densities
  dx <- estimate_density(x, method = method_density, precision = precision, extend = extend, extend_scale = extend_scale, ...)
  dy <- estimate_density(y, method = method_density, precision = precision, extend = extend, extend_scale = extend_scale, ...)

  # Create density estimation functions
  fx <- stats::approxfun(dx$x, dx$y, method = "linear", rule = 2)
  fy <- stats::approxfun(dy$x, dy$y, method = "linear", rule = 2)

  x_axis <- seq(min(c(dx$x, dy$x)), max(c(dx$x, dy$x)), length.out = precision)
  data <- data.frame(x = x_axis, y1 = fx(x_axis), y2 = fy(x_axis))



  # calculate intersection densities
  data$intersection <- pmin(data$y1, data$y2)
  data$exclusion <- pmax(data$y1, data$y2)

  # integrate areas under curves
  area_intersection <- area_under_curve(data$x, data$intersection, method = method_auc)
  # area_exclusion <- area_under_curve(data$x, data$exclusion, method = method_auc)


  # compute overlap coefficient
  overlap <- area_intersection
  attr(overlap, "data") <- data

  class(overlap) <- c("overlap", class(overlap))
  overlap
}


#' @export
print.overlap <- function(x, ...) {
  insight::print_color("# Overlap\n\n", "blue")
  cat(sprintf("%.1f%%\n", 100 * as.numeric(x)))
}


#' @export
plot.overlap <- function(x, ...) {
  # Can be improved through see
  data <- attributes(x)$data
  graphics::plot(data$x, data$exclusion, type = "l")
  graphics::polygon(data$x, data$intersection, col = "red")
}
