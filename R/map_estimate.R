#' Maximum A Posteriori (MAP) Estimate
#'
#' Find the Highest Maximum A Posteriori (MAP) estimate of a posterior.
#'
#' @param posterior vector representing a posterior distribution.
#' @param precision number of points for density estimation. See the `n` parameter in \link[=density]{density}.
#' @return The x, y coordinates of the MAP, corresponding to the MAP (x) and its density (y).
#' @examples
#' library(bayestestR)
#' 
#' posterior <- rnorm(1000)
#' map_estimate(posterior)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats density
#' @export
map_estimate <- function(posterior, precision = 2^10) {
  d <- density(posterior, n = precision)

  hdp_x <- d$x[which.max(d$y)]
  hdp_y <- max(d$y)
  return(c(hdp_x, hdp_y))
}
