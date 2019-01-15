#' The (Perfect) Normal Distribution
#'
#' Generate a sample of size n with a near-perfect normal distribution.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param mean vector of means.
#' @param sd vector of standard deviations.
#'
#' @examples
#' library(bayestestR)
#' x <- rnorm_perfect(n = 10)
#' plot(density(x))
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats qnorm
#' @export
rnorm_perfect <- function(n, mean = 0, sd = 1) {
  x <- qnorm(seq(1 / n, 1 - 1 / n, length.out = n), mean, sd)
  return(x)
}
