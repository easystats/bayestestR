#' Bayesian p-value based on the density at the Maximum A Priori (MAP)
#'
#' Compute a Bayesian equivalent of the p-value, related to the odds that a parameter (described by its posterior distribution) has against the null hypothesis (\emph{h0}) using Mills' (2014, 2017) \emph{Objective Bayesian Hypothesis Testing} framework. It is mathematically based on the density at the Maximum A Priori (MAP) and corresponds to the density value at 0 divided by the density of the MAP estimate.
#'
#' @param posterior Vector representing a posterior distribution. Can also be a \code{stanreg} or \code{brmsfit} model.
#' @param precision Number of points for density estimation. See the \code{n}-parameter in \link[=density]{density}.
#'
#' @inheritParams hdi
#'
#' @examples
#' library(bayestestR)
#'
#' p_map(posterior = rnorm(1000, 0, 1))
#' p_map(posterior = rnorm(1000, 10, 1))
#'
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' p_map(model)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' p_map(model)}
#'
#' @references \href{https://www.youtube.com/watch?v=Ip8Ci5KUVRc}{Mill's talk}
#'
#' @importFrom stats density
#' @export
p_map <- function(posterior, ...) {
  UseMethod("p_map")
}


#' @export
print.p_map <- function(x, ...) {
  cat(sprintf("p (MAP) = %.2f", x))
}


#' @rdname p_map
#' @export
p_map.numeric <- function(posterior, precision = 2^10, ...) {
  # Highest density point
  map <- map_estimate(posterior, precision = precision, density = TRUE)$MAP_density

  # Density at 0
  d_0 <- density_at(posterior, 0, precision = precision)
  if (is.na(d_0)) d_0 <- 0

  # Odds
  p <- d_0 / map
  class(p) <- c("p_map", class(p))
  p
}



#' @importFrom insight get_parameters
#' @keywords internal
.p_map_models <- function(posterior, precision, effects, component, parameters) {
  data.frame(
    "Parameter" = .get_parameter_names(posterior, effects = effects, component = component, parameters = parameters),
    "p_MAP" = sapply(insight::get_parameters(posterior, effects = effects, component = component, parameters = parameters), p_map, precision = precision, simplify = TRUE),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' @rdname p_map
#' @export
p_map.stanreg <- function(posterior, precision = 2^10, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  .p_map_models(
    posterior = posterior,
    precision = precision,
    effects = effects,
    component = "conditional",
    parameters = parameters
  )
}

#' @rdname p_map
#' @export
p_map.brmsfit <- function(posterior, precision = 2^10, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  .p_map_models(
    posterior = posterior,
    precision = precision,
    effects = effects,
    component = component,
    parameters = parameters
  )
}

