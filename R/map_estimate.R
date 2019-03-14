#' Maximum A Posteriori (MAP) Estimate
#'
#' Find the Highest Maximum A Posteriori (MAP) estimate of a posterior. It corresponds to the 'peak' of the posterior distribution.
#'
#' @param posterior Vector representing a posterior distribution. Can also be a \code{stanreg} or \code{brmsfit} model.
#' @param precision Number of points for density estimation. See the \code{n} parameter in \link[=density]{density}.
#' @param density If \code{TRUE}, it will return a data.frame with the x, y coordinates of the MAP, corresponding respectively to the MAP and its density value.
#'
#' @inheritParams hdi
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' map_estimate(posterior)
#' map_estimate(posterior, density = TRUE)
#'
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' map_estimate(model)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' map_estimate(model)}
#'
#' @importFrom stats density
#' @export
map_estimate <- function(posterior, ...) {
  UseMethod("map_estimate")
}


#' @rdname map_estimate
#' @export
map_estimate.numeric <- function(posterior, precision = 2^10, density = FALSE, ...) {
  d <- stats::density(posterior, n = precision)

  hdp_x <- d$x[which.max(d$y)]
  hdp_y <- max(d$y)

  if (density == FALSE) {
    hdp_x
  } else {
    data.frame(
      "MAP" = hdp_x,
      "MAP_density" = hdp_y
    )
  }
}


#' @importFrom insight get_parameters
#' @keywords internal
.map_estimate_models <- function(posterior, precision, density) {
  list <- sapply(posterior, map_estimate, precision = precision, density = TRUE, simplify = FALSE)
  out <- flatten_list(list, name = "Parameter")
  rownames(out) <- NULL

  if (density == TRUE) {
    out
  } else {
    out[names(out) != "MAP_density"]
  }
}

#' @rdname map_estimate
#' @export
map_estimate.stanreg <- function(posterior, precision = 2^10, density = FALSE, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  .map_estimate_models(
    posterior = insight::get_parameters(posterior, effects = effects, parameters = parameters),
    precision = precision,
    density = density
  )
}

#' @rdname map_estimate
#' @export
map_estimate.brmsfit <- function(posterior, precision = 2^10, density = FALSE, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  .map_estimate_models(
    posterior = insight::get_parameters(posterior, effects = effects, parameters = parameters),
    precision = precision,
    density = density
  )
}
