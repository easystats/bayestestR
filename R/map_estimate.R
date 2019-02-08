#' Maximum A Posteriori (MAP) Estimate
#'
#' Find the Highest Maximum A Posteriori (MAP) estimate of a posterior. It corresponds to the 'peak' of the posterior distribution.
#'
#' @param posterior Vector representing a posterior distribution. Can also be a \code{stanreg} or \code{brmsfit} model.
#' @param precision Number of points for density estimation. See the \code{n} parameter in \link[=density]{density}.
#' @param density If TRUE, it will return a data.frame with the x, y coordinates of the MAP, corresponding respectively to the MAP and its density value.
#'
#' @examples
#' library(bayestestR)
#' 
#' posterior <- rnorm(1000)
#' map_estimate(posterior)
#' map_estimate(posterior, density = TRUE)
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' map_estimate(model)
#' 
#' # Will fail until get_predictors is implemented.
#' # library(brms)
#' # model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' # map_estimate(model)
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats density
#' @export
map_estimate <- function(posterior, precision = 2^10, density = FALSE) {
  UseMethod("map_estimate")
}






#' @export
map_estimate.numeric <- function(posterior, precision = 2^10, density = FALSE) {
  d <- density(posterior, n = precision)

  hdp_x <- d$x[which.max(d$y)]
  hdp_y <- max(d$y)

  if (density == FALSE) {
    return(hdp_x)
  } else {
    return(data.frame(
      "MAP" = hdp_x,
      "MAP_density" = hdp_y
    ))
  }
}








#' @keywords internal
.map_estimate_models <- function(posterior, precision = 2^10, density = FALSE) {
  list <- sapply(get_parameters(posterior), map_estimate, precision = precision, density = TRUE, simplify = FALSE)
  out <- flatten_list(list, name = "Parameter")
  if (density == TRUE) {
    return(out)
  } else {
    return(out[names(out) != "MAP_density"])
  }
}

#' @export
map_estimate.stanreg <- .map_estimate_models

#' @export
map_estimate.brmsfit <- .map_estimate_models
