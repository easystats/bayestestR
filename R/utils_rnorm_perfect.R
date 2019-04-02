#' The (Perfect) Normal Distribution
#'
#' Generate a sample of size \code{n} with a near-perfect normal distribution.
#'
#' @param n Number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' @param mean Vector of means.
#' @param sd Vector of standard deviations.
#'
#' @examples
#' library(bayestestR)
#' x <- rnorm_perfect(n = 10)
#' plot(density(x))
#' @importFrom stats qnorm sd
#' @export
rnorm_perfect <- function(n, mean = 0, sd = 1) {
  stats::qnorm(seq(1 / n, 1 - 1 / n, length.out = n), mean, stats::sd)
}
