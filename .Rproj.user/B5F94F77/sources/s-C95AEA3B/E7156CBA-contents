#' Odds againt the null hypothesis (h0)
#'
#' Compute the odds that a parameter (described by its posterior distribution) has againt the null hypothesis (h0) using Mills' (2014, 2017) Objective Bayesian Hypothesis Testing paradigm.
#'
#' @param posterior vector representing a posterior distribution.
#' @param precision number of points for density estimation. See the `n` parameter in \link[=density]{density}.
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(1000, 1, 1)
#' odds_h0(posterior)
#'
#' @references \href{https://www.youtube.com/watch?v=Ip8Ci5KUVRc}{Mill's talk}
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats density
#' @export
odds_h0 <- function(posterior, precision = 2^10) {
  if(pd(posterior) < 100){
    # Highest density point
    map <- map_estimate(posterior)[2]

    # Density at 0
    d <- density(posterior, n = precision)
    closest_value_to_0 <- which.min(abs(d$x))
    d_0 <- d$y[closest_value_to_0]

    # Odds
    return(map / d_0)
  } else{
    return(Inf)
  }
}

