#' Empirical Distributions
#'
#' Generate a sample of size \code{n} with a near-perfect distribution.
#'
#' @param type Can be \code{"normal"} (default), \code{"cauchy"}, \code{"poisson"} or \code{"student"}.
#' @param random Generate near-perfect or random (simple wrappers for the base R \code{r*} functions) distributions.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(bayestestR)
#' x <- distribution(n = 10)
#' plot(density(x))
#'
#' @export
distribution <- function(type = "normal", ...) {
  if(type == "normal"){
    return(distribution_normal(...))
  }
}



#' @rdname distribution
#' @inheritParams stats::rnorm
#' @importFrom stats qnorm
#' @export
distribution_normal <- function(n, mean = 0, sd = 1, random = FALSE) {
  if(random){
    stats::rnorm(n, mean, sd)
  } else{
    stats::qnorm(seq(1 / n, 1 - 1 / n, length.out = n), mean, sd)
  }
}


























#' @rdname distribution
#' @inheritParams stats::rnorm
#' @importFrom stats qnorm
#' @export
rnorm_perfect <- function(n, mean = 0, sd = 1) {
  .Deprecated("distribution_normal")
  stats::qnorm(seq(1 / n, 1 - 1 / n, length.out = n), mean, sd)
}


#' @rdname distribution
#' @inheritParams stats::rcauchy
#' @importFrom stats qcauchy
#' @export
rcauchy_perfect <- function(n, location = 0, scale = 1) {
  stats::qcauchy(seq(1 / n, 1 - 1 / n, length.out = n), location, scale)
}


#' @rdname distribution
#' @inheritParams stats::rpois
#' @importFrom stats qcauchy
#' @export
rpois_perfect <- function(n, lambda) {
  stats::qcauchy(seq(1 / n, 1 - 1 / n, length.out = n), lambda)
}


#' @rdname distribution
#' @inheritParams stats::rt
#' @importFrom stats qt
#' @export
rt_perfect <- function(n, df, ncp) {
  stats::qt(seq(1 / n, 1 - 1 / n, length.out = n), df, ncp)
}