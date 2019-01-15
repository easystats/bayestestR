#' Overlap Coefficient of Two Empirical Distributions
#'
#' Calculate the overlap coefficient between two kernel density estimates.
#'
#' @param x vector of values from a probability distribution.
#' @param y vector of values from a probability distribution.
#' @param precision number of points for density estimation. See the `n` parameter in \link[=density]{density}.
#' @param normalize use normalized density kernels.
#' @param onesided if false (default), it will compute the proportion of the total density mass common to the 2 distributions. Otherwise, it will compute the proportion of the density mass of x "included" in y.
#'
#' @examples
#' library(bayestestR)
#' 
#' overlap(x = rnorm(1000, 0, 1), y = rnorm(1000, 0, 1))
#' overlap(x = rnorm(1000, 0, 1), y = rnorm(1000, 10, 1))
#' 
#' overlap(x = rnorm(1000, 0, 0.1), y = rnorm(1000, 0, 1), normalize = TRUE, onesided = TRUE)
#' overlap(x = rnorm(1000, 0, 1), y = rnorm(1000, 0, 0.1), normalize = TRUE, onesided = TRUE)
#' @author S. Venne
#'
#' @importFrom stats density
#' @export
overlap <- function(x, y, precision = 2^10, normalize = FALSE, onesided = FALSE) {

  # define limits of a common grid, adding a buffer so that tails aren't cut off
  lower <- min(c(x, y)) - 1
  upper <- max(c(x, y)) + 1


  # generate kernel densities
  dx <- stats::density(x, from = lower, to = upper, n = precision)
  dy <- stats::density(y, from = lower, to = upper, n = precision)

  if (normalize) {
    d <- data.frame(x = dx$x, a = .normalize(dx$y), b = .normalize(dy$y))
  } else {
    d <- data.frame(x = dx$x, a = dx$y, b = dy$y)
  }

  # calculate intersection densities
  d$intersection <- pmin(d$a, d$b)
  d$total <- pmax(d$a, d$b)

  # compute overlap coefficient
  if (onesided) {
    overlap <- .AUC(d$x, d$intersection) / .AUC(d$x, d$a)
  } else {
    overlap <- .AUC(d$x, d$intersection) / .AUC(d$x, d$total)
  }

  return(overlap)
}





#' @keywords internal
.normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}






#' @importFrom utils head tail
#' @keywords internal
.AUC <- function(x, y) {
  sum(diff(x) * (head(y, -1) + tail(y, -1))) / 2
}
