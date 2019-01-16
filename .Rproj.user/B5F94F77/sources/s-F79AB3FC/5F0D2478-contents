#' Bayesian p-value based on the density at the Maximum A Priori (MAP)
#'
#' Compute a Bayesian equivalent of the p-value, related to the odds that a parameter (described by its posterior distribution) has againt the null hypothesis (h0) using Mills' (2014, 2017) Objective Bayesian Hypothesis Testing paradigm. It is mathematically based on the density at the Maximum A Priori (MAP).
#'
#' @details It correponds to the density value at 0 divided by the density of the highest density point.
#'
#' @param posterior vector representing a posterior distribution.
#' @param precision number of points for density estimation. See the `n` parameter in \link[=density]{density}.
#'
#' @examples
#' library(bayestestR)
#'
#' p_map(posterior = rnorm(1000, 0, 1))
#' p_map(posterior = rnorm(1000, 10, 1))
#' @references \href{https://www.youtube.com/watch?v=Ip8Ci5KUVRc}{Mill's talk}
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats density
#' @export
p_map <- function(posterior, precision = 2^10) {
  # Highest density point
  map <- map_estimate(posterior)[2]

  # Density at 0
  d <- density(posterior, n = precision)
  closest_value_to_0 <- which.min(abs(d$x))
  d_0 <- d$y[closest_value_to_0]

  # Odds
  return(d_0 / map)
}
