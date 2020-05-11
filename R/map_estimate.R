#' Maximum A Posteriori probability estimate (MAP)
#'
#' Find the \strong{Highest Maximum A Posteriori probability estimate (MAP)} of a posterior, i.e., the value associated with the highest probability density (the "peak" of the posterior distribution). In other words, it is an estimation of the \emph{mode} for continuous parameters. Note that this function relies on \link{estimate_density}, which by default uses a different smoothing bandwidth (\code{"SJ"}) compared to the legacy default implemented the base R \link{density} function (\code{"nrd0"}).
#'
#' @inheritParams hdi
#' @inheritParams estimate_density
#'
#' @return A numeric value if \code{posterior} is a vector. If \code{posterior}
#' is a model-object, returns a data frame with following columns:
#'   \itemize{
#'     \item \code{Parameter} The model parameter(s), if \code{x} is a model-object. If \code{x} is a vector, this column is missing.
#'     \item \code{MAP_Estimate} The MAP estimate for the posterior or each model parameter.
#'   }
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#'
#' posterior <- rnorm(10000)
#' map_estimate(posterior)
#'
#' plot(density(posterior))
#' abline(v = map_estimate(posterior), col = "red")
#'
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
map_estimate <- function(x, precision = 2^10, method = "kernel", ...) {
  UseMethod("map_estimate")
}





#' @rdname map_estimate
#' @export
map_estimate.numeric <- function(x, precision = 2^10, method = "kernel", ...) {
  d <- estimate_density(x, precision = precision, method = method, ...)

  hdp_x <- d$x[which.max(d$y)]
  hdp_y <- max(d$y)

  out <- hdp_x
  attr(out, "MAP_density") <- hdp_y

  attr(out, "data") <- x
  attr(out, "centrality") <- "map"
  class(out) <- unique(c("map_estimate", "see_point_estimate", class(out)))

  out
}







#' @importFrom insight get_parameters
#' @keywords internal
.map_estimate_models <- function(x, precision, method, ...) {
  l <- sapply(x, map_estimate, precision = precision, method = method, simplify = FALSE, ...)

  out <- data.frame(
    Parameter = colnames(x),
    MAP_Estimate = unlist(l),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "MAP_density") <- sapply(l, attr, "MAP_density")
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  attr(out, "centrality") <- "map"
  class(out) <- unique(c("map_estimate", "see_point_estimate", class(out)))

  class(out) <- unique(c("map_estimate", class(out)))
  out
}

#' @rdname map_estimate
#' @export
map_estimate.stanreg <- function(x, precision = 2^10, method = "kernel", effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  .map_estimate_models(
    x = insight::get_parameters(x, effects = effects, parameters = parameters),
    precision = precision,
    method = method
  )
}

#' @rdname map_estimate
#' @export
map_estimate.brmsfit <- function(x, precision = 2^10, method = "kernel", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  .map_estimate_models(
    x = insight::get_parameters(x, effects = effects, component = component, parameters = parameters),
    precision = precision, method = method
  )
}


#' @rdname map_estimate
#' @export
map_estimate.data.frame <- function(x, precision = 2^10, method = "kernel", ...) {
  .map_estimate_models(x, precision = precision, method = method)
}


#' @rdname map_estimate
#' @export
map_estimate.emmGrid <- function(x, precision = 2^10, method = "kernel", ...) {
  x <- .clean_emmeans_draws(x)
  .map_estimate_models(x, precision = precision, method = method)
}


#' @rdname as.numeric.p_direction
#' @method as.numeric map_estimate
#' @export
as.numeric.map_estimate <- function(x, ...) {
  if (inherits(x, "data.frame")) {
    me <- as.numeric(as.vector(x$MAP_Estimate))
    names(me) <- x$Parameter
    me
  } else {
    as.vector(x)
  }
}


#' @method as.double map_estimate
#' @export
as.double.map_estimate <- as.numeric.map_estimate
