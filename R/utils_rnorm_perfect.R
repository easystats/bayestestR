#' Perfect Empirical Distributions
#'
#' Generate a sample of size \code{n} with a near-perfect distribution.
#'
#' @inheritParams stats::rnorm
#'
#' @examples
#' library(bayestestR)
#' x <- rnorm_perfect(n = 10)
#' plot(density(x))
#' @importFrom stats qnorm
#' @export
rnorm_perfect <- function(n, mean = 0, sd = 1) {
  stats::qnorm(seq(1 / n, 1 - 1 / n, length.out = n), mean, sd)
}


#' @rdname rnorm_perfect
#' @inheritParams stats::rcauchy
#' @importFrom stats qcauchy
#' @export
rcauchy_perfect <- function(n, location = 0, scale = 1) {
  stats::qcauchy(seq(1 / n, 1 - 1 / n, length.out = n), location, scale)
}


#' @rdname rnorm_perfect
#' @inheritParams stats::rpois
#' @importFrom stats qcauchy
#' @export
rpois_perfect <- function(n, lambda) {
  stats::qcauchy(seq(1 / n, 1 - 1 / n, length.out = n), lambda)
}


#' @rdname rnorm_perfect
#' @inheritParams stats::rt
#' @importFrom stats qt
#' @export
rt_perfect <- function(n, df, ncp) {
  stats::qt(seq(1 / n, 1 - 1 / n, length.out = n), df, ncp)
}