#' Probability of a Given Point
#'
#' Compute the density of a given point of a distribution.
#'
#' @param posterior Vector representing a posterior distribution.
#' @param x The value of which to get the approximate probability.
#' @param precision Number of points for density estimation. See the `n` parameter in \link[=density]{density}.
#'
#' @examples
#' library(bayestestR)
#' posterior <- rnorm_perfect(n = 10)
#' density_at(posterior, 0)
#' density_at(posterior, c(0, 1))
#' @importFrom stats approx density
#' @export
density_at <- function(posterior, x, precision = 2^10) {
  density <- stats::density(posterior, n = precision)
  stats::approx(density$x, density$y, xout = x)$y
}
