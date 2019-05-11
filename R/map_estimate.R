#' Maximum A Posteriori (MAP) Estimate
#'
#' Find the \strong{Highest Maximum A Posteriori (MAP)} estimate of a posterior, \emph{i.e.,} the most probable value. It corresponds to the "peak" (or the \emph{mode}) of the posterior distribution. Note this function relies on \link{estimate_density}, which by default uses a different smoothing bandwidth (\code{"SJ"}) from the legacy default implemented the base R \link{density} function (\code{"nrd0"}).
#'
#' @inheritParams hdi
#' @inheritParams estimate_density
#'
#' @return A numeric value if \code{posterior} is a vector.
#'   If \code{density = TRUE}, or if \code{posterior} is a model-object, returns
#'   a data frame with following columns:
#'   \itemize{
#'     \item \code{Parameter} The model parameter(s), if \code{x} is a model-object. If \code{x} is a vector, this column is missing.
#'     \item \code{MAP} The MAP estimate for the posterior or each model parameter.
#'   }
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(10000)
#' map_estimate(posterior)
#'
#' plot(density(posterior))
#' abline(v = map_estimate(posterior), col = "red")
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




#' @export
print.MAP <- function(x, ...) {
  cat(sprintf("MAP = %.2f", x))
}





#' @rdname map_estimate
#' @export
map_estimate.numeric <- function(x, precision = 2^10, ...) {
  d <- estimate_density(x, precision = precision, ...)

  hdp_x <- d$x[which.max(d$y)]
  hdp_y <- max(d$y)

  out <- hdp_x
  attr(out, "MAP_density") <- hdp_y

  class(out) <- c(class(out), "MAP")
  return(out)
}







#' @importFrom insight get_parameters
#' @keywords internal
.map_estimate_models <- function(x, precision, ...) {
  list <- sapply(x, map_estimate, precision = precision, simplify = FALSE, ...)
  out <- .flatten_list(list, name = "Parameter")
  rownames(out) <- NULL

  as.data.frame(out)
}

#' @rdname map_estimate
#' @export
map_estimate.stanreg <- function(x, precision = 2^10, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  .map_estimate_models(
    x = insight::get_parameters(x, effects = effects, parameters = parameters),
    precision = precision
  )
}

#' @rdname map_estimate
#' @export
map_estimate.brmsfit <- function(x, precision = 2^10, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  .map_estimate_models(
    x = insight::get_parameters(x, effects = effects, parameters = parameters),
    precision = precision
  )
}
