#' Maximum A Posteriori (MAP) Estimate
#'
#' Find the \strong{Highest Maximum A Posteriori (MAP)} estimate of a posterior, \emph{i.e.,} the most probable value. It corresponds to the "peak" (or the \emph{mode}) of the posterior distribution. This function returns a dataframe containing the MAP value. If the \code{density} is set to \code{TRUE}, it will include a second column containing the \emph{probability} (\emph{i.e.,} the value of the estimated density function) associated with the MAP (the value of the y axis of the density curve at the MAP).
#'
#' @inheritParams hdi
#' @param precision Number of points for density estimation. See the \code{n} parameter in \link[=density]{density}.
#' @param density Turning this parameter
#'
#' @return A numeric value if \code{posterior} is a vector and \code{density = FALSE}.
#'   If \code{density = TRUE}, or if \code{posterior} is a model-object, returns
#'   a data frame with following columns:
#'   \itemize{
#'     \item \code{Parameter} The model parameter(s), if \code{x} is a model-object. If \code{x} is a vector, this column is missing.
#'     \item \code{MAP} The MAP estimate for the posterior or each model parameter.
#'     \item \code{MAP_density}
#'   }
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(10000)
#'
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
map_estimate.numeric <- function(x, precision = 2^10, density = FALSE, ...) {
  d <- stats::density(x, n = precision)

  hdp_x <- d$x[which.max(d$y)]
  hdp_y <- max(d$y)

  out <- data.frame(
    "MAP" = hdp_x,
    "MAP_density" = hdp_y
  )

  if (density == TRUE) {
    out
  } else {
    out[names(out) != "MAP_density"]
  }
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
map_estimate.stanreg <- function(x, precision = 2^10, effects = c("fixed", "random", "all"), parameters = NULL, density = FALSE, ...) {
  effects <- match.arg(effects)

  .map_estimate_models(
    x = insight::get_parameters(x, effects = effects, parameters = parameters),
    precision = precision,
    density = density
  )
}

#' @rdname map_estimate
#' @export
map_estimate.brmsfit <- function(x, precision = 2^10, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, density = FALSE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  .map_estimate_models(
    x = insight::get_parameters(x, effects = effects, parameters = parameters),
    precision = precision,
    density = density
  )
}
