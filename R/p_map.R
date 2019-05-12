#' Bayesian p-value based on the density at the Maximum A Priori (MAP)
#'
#' Compute a Bayesian equivalent of the p-value, related to the odds that a parameter (described by its posterior distribution) has against the null hypothesis (\emph{h0}) using Mills' (2014, 2017) \emph{Objective Bayesian Hypothesis Testing} framework. It is mathematically based on the density at the Maximum A Priori (MAP) and corresponds to the density value at 0 divided by the density of the MAP estimate.
#'
#'
#' @param precision Number of points for density estimation. See the \code{n}-parameter in \link[=density]{density}.
#'
#' @inheritParams hdi
#'
#' @examples
#' library(bayestestR)
#'
#' p_map(rnorm(1000, 0, 1))
#' p_map(rnorm(1000, 10, 1))
#'
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' p_map(model)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' p_map(model)
#' }
#'
#' @references \href{https://www.youtube.com/watch?v=Ip8Ci5KUVRc}{Mill's talk}
#'
#' @importFrom stats density
#' @export
p_map <- function(x, ...) {
  UseMethod("p_map")
}






#' @rdname p_map
#' @export
p_map.numeric <- function(x, precision = 2^10, ...) {
  # Density at MAP
  map <- attributes(map_estimate(x, precision = precision, ...))$MAP_density

  # Density at 0
  d_0 <- density_at(x, 0, precision = precision)
  if (is.na(d_0)) d_0 <- 0

  # Odds
  p <- d_0 / map
  class(p) <- c("p_map", class(p))
  p
}



#' @export
p_map.data.frame <- function(x, precision = 2^10, ...) {
  if(ncol(x) == 1){
    p_MAP <- p_map(x[, 1], precision = precision, ...)
  } else{
    p_MAP <- sapply(.select_nums(x), p_map, precision = precision, simplify = TRUE, ...)
  }

  out <- data.frame(
    "Parameter" = names(x),
    "p_MAP" = p_MAP,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  class(out) <- c("p_map", class(out))
  out

}



#' @importFrom insight get_parameters
#' @keywords internal
.p_map_models <- function(x, precision, effects, component, parameters, ...) {
  out <- p_map(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), precision = precision, ...)
  out$Parameter <- .get_parameter_names(x, effects = effects, component = component, parameters = parameters)

  out
}

#' @rdname p_map
#' @export
p_map.stanreg <- function(x, precision = 2^10, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  out <- .p_map_models(
    x = x,
    precision = precision,
    effects = effects,
    component = "conditional",
    parameters = parameters,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}

#' @rdname p_map
#' @export
p_map.brmsfit <- function(x, precision = 2^10, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .p_map_models(
    x = x,
    precision = precision,
    effects = effects,
    component = component,
    parameters = parameters,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}




#' Numeric Vectors
#'
#' @inheritParams base::as.numeric
#' @method as.numeric p_map
#' @export
as.numeric.p_map <- function(x, ...){
  if("data.frame" %in% class(x)){
    return(as.numeric(as.vector(x$p_MAP)))
  } else{
    return(as.vector(x))
  }
}


#' @method as.double p_map
#' @export
as.double.p_map <- as.numeric.p_map