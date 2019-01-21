#' Probability of a Given Point
#'
#' Compute the density of a given point of a distribution.
#'
#' @param posterior vector representing a posterior distribution.
#' @param x the value of which to get the approximate probability.
#' @param precision number of points for density estimation. See the `n` parameter in \link[=density]{density}.
#'
#' @examples
#' library(bayestestR)
#' posterior <- rnorm_perfect(n = 10)
#' density_at(posterior, 0)
#' density_at(posterior, c(0, 1))
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats approx
#' @export
density_at <- function(posterior, x, precision = 2^10) {
  density <- density(posterior, n = precision)
  return(approx(density$x, density$y, xout = x)$y)
}
