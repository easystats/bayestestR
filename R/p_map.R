#' Bayesian p-value based on the density at the Maximum A Priori (MAP)
#'
#' Compute a Bayesian equivalent of the p-value, related to the odds that a parameter (described by its posterior distribution) has against the null hypothesis (h0) using Mills' (2014, 2017) Objective Bayesian Hypothesis Testing paradigm. It is mathematically based on the density at the Maximum A Priori (MAP). It corresponds to the density value at 0 divided by the density of the highest density point.
#'
#' @param posterior Vector representing a posterior distribution. Can also be a `stanreg` or `brmsfit` model.
#' @param precision Number of points for density estimation. See the `n` parameter in \link[=density]{density}.
#'
#' @examples
#' library(bayestestR)
#' 
#' p_map(posterior = rnorm(1000, 0, 1))
#' p_map(posterior = rnorm(1000, 10, 1))
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' p_map(model)
#' 
#' # Will fail until get_predictors is implemented.
#' # library(brms)
#' # model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' # p_map(model)
#' }
#' @references \href{https://www.youtube.com/watch?v=Ip8Ci5KUVRc}{Mill's talk}
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats density
#' @export
p_map <- function(posterior, precision = 2^10) {
  UseMethod("p_map")
}


#' @export
print.p_map <- function(x, ...) {
  cat(sprintf("p (MAP) = %.2f", x))
}



#' @export
p_map.numeric <- function(posterior, precision = 2^10) {
  # Highest density point
  map <- map_estimate(posterior, precision = precision, density = TRUE)$MAP_density

  # Density at 0
  d_0 <- density_at(posterior, 0, precision = precision)
  if (is.na(d_0)) d_0 <- 0

  # Odds
  p <- d_0 / map
  class(p) <- c("p_map", class(p))
  return(p)
}




#' @keywords internal
.p_map_models <- function(posterior, precision = 2^10) {
  out <- data.frame(
    "Parameter" = find_parameters(posterior),
    "p_MAP" = sapply(get_parameters(posterior), p_map, precision = precision, simplify = TRUE),
    row.names = NULL
  )
  return(out)
}

#' @export
p_map.stanreg <- .p_map_models

#' @export
p_map.brmsfit <- .p_map_models
