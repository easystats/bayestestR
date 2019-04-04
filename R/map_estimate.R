#' Maximum A Posteriori (MAP) Estimate
#'
#' Find the \strong{Highest Maximum A Posteriori (MAP)} estimate of a posterior, \emph{i.e.,} the most probable value. It corresponds to the "peak" (or the \emph{mode}) of the posterior distribution. This returns a dataframe with one column containing the MAP value and a second column containing the \emph{probability} (\emph{i.e.,} the value of the estimated density function) associated with the MAP.
#'
#' @inheritParams hdi
#' @param precision Number of points for density estimation. See the \code{n} parameter in \link[=density]{density}.
#'
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(10000)
#' map_estimate(posterior)
#' map_estimate(posterior, density = TRUE)
#'
#' plot(density(posterior))
#' abline(v=map_estimate(posterior),
#'        col="red")  # The x coordinate of MAP
#' abline(h=map_estimate(posterior, density = TRUE)$MAP_density,
#'        col="blue")  # The y coordinate of MAP
#'
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' map_estimate(model)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' map_estimate(model)
#' }
#'
#' @importFrom stats density
#' @export
map_estimate <- function(x, ...) {
  UseMethod("map_estimate")
}


#' @rdname map_estimate
#' @export
map_estimate.numeric <- function(x, precision = 2^10, ...) {
  d <- stats::density(x, n = precision)

  hdp_x <- d$x[which.max(d$y)]
  hdp_y <- max(d$y)

  data.frame(
    "MAP" = hdp_x,
    "MAP_density" = hdp_y
  )
}


#' @importFrom insight get_parameters
#' @keywords internal
.map_estimate_models <- function(x, precision, density = FALSE) {
  list <- sapply(x, map_estimate, precision = precision, simplify = FALSE)
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
map_estimate.stanreg <- function(x, precision = 2^10, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  .map_estimate_models(
    x = insight::get_parameters(x, effects = effects, parameters = parameters),
    precision = precision,
    ...
  )
}

#' @rdname map_estimate
#' @export
map_estimate.brmsfit <- function(x, precision = 2^10, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  .map_estimate_models(
    x = insight::get_parameters(x, effects = effects, parameters = parameters),
    precision = precision,
    ...
  )
}
