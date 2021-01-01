#' Bayesian p-value based on the density at the Maximum A Posteriori (MAP)
#'
#' Compute a Bayesian equivalent of the \emph{p}-value, related to the odds that a parameter (described by its posterior distribution) has against the null hypothesis (\emph{h0}) using Mills' (2014, 2017) \emph{Objective Bayesian Hypothesis Testing} framework. It corresponds to the density value at 0 divided by the density at the Maximum A Posteriori (MAP).
#'
#' @details Note that this method is sensitive to the density estimation \code{method} (see the section in the examples below).
#' \subsection{Strengths and Limitations}{
#' \strong{Strengths:} Straightforward computation. Objective property of the posterior distribution.
#' \cr \cr
#' \strong{Limitations:} Limited information favoring the null hypothesis. Relates on density approximation. Indirect relationship between mathematical definition and interpretation. Only suitable for weak / very diffused priors.
#' }
#'
#' @inheritParams hdi
#' @inheritParams density_at
#'
#' @examples
#' library(bayestestR)
#'
#' p_map(rnorm(1000, 0, 1))
#' p_map(rnorm(1000, 10, 1))
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' p_map(model)
#'
#' library(emmeans)
#' p_map(emtrends(model, ~1, "wt"))
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' p_map(model)
#'
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' p_map(bf)
#' }
#'
#' \donttest{
#' # ---------------------------------------
#' # Robustness to density estimation method
#' set.seed(333)
#' data <- data.frame()
#' for (iteration in 1:250) {
#'   x <- rnorm(1000, 1, 1)
#'   result <- data.frame(
#'     "Kernel" = p_map(x, method = "kernel"),
#'     "KernSmooth" = p_map(x, method = "KernSmooth"),
#'     "logspline" = p_map(x, method = "logspline")
#'   )
#'   data <- rbind(data, result)
#' }
#' data$KernSmooth <- data$Kernel - data$KernSmooth
#' data$logspline <- data$Kernel - data$logspline
#'
#' summary(data$KernSmooth)
#' summary(data$logspline)
#' boxplot(data[c("KernSmooth", "logspline")])
#' }
#' @seealso \href{https://www.youtube.com/watch?v=Ip8Ci5KUVRc}{Jeff Mill's talk}
#'
#' @references \itemize{
#'   \item Makowski D, Ben-Shachar MS, Chen SHA, Lüdecke D (2019) Indices of Effect Existence and Significance in the Bayesian Framework. Frontiers in Psychology 2019;10:2767. \doi{10.3389/fpsyg.2019.02767}
#'   \item Mills, J. A. (2018). Objective Bayesian Precise Hypothesis Testing. University of Cincinnati.
#' }
#'
#' @importFrom stats density
#' @export
p_map <- function(x, precision = 2^10, method = "kernel", ...) {
  UseMethod("p_map")
}

#' @rdname p_map
#' @export
p_pointnull <- p_map



#' @export
p_map.numeric <- function(x, precision = 2^10, method = "kernel", ...) {
  # Density at MAP
  map <- attributes(map_estimate(x, precision = precision, method = method, ...))$MAP_density

  # Density at 0
  d_0 <- density_at(x, 0, precision = precision, method = method, ...)
  if (is.na(d_0)) d_0 <- 0

  # Odds
  p <- d_0 / map
  class(p) <- c("p_map", class(p))
  p
}



#' @export
p_map.data.frame <- function(x, precision = 2^10, method = "kernel", ...) {
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    p_MAP <- p_map(x[, 1], precision = precision, method = method, ...)
  } else {
    p_MAP <- sapply(x, p_map, precision = precision, method = method, simplify = TRUE, ...)
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




#' @export
p_map.emmGrid <- function(x, precision = 2^10, method = "kernel", ...) {
  xdf <- insight::get_parameters(x)

  out <- p_map(xdf, precision = precision, method = method, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
p_map.emm_list <- p_map.emmGrid




#' @importFrom insight get_parameters
#' @keywords internal
.p_map_models <- function(x, precision, method, effects, component, parameters, ...) {
  p_map(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), precision = precision, method = method, ...)
}




#' @export
p_map.mcmc <- function(x, precision = 2^10, method = "kernel", parameters = NULL, ...) {
  out <- .p_map_models(
    x = x,
    precision = precision,
    method = method,
    effects = "fixed",
    component = "conditional",
    parameters = parameters,
    ...
  )

  attr(out, "data") <- insight::get_parameters(x, parameters = parameters)
  out
}


#' @export
p_map.bcplm <- p_map.mcmc


#' @export
p_map.mcmc.list <- p_map.mcmc



#' @export
p_map.bamlss <- function(x, precision = 2^10, method = "kernel", component = c("all", "conditional", "location"), parameters = NULL, ...) {
  component <- match.arg(component)
  out <- .p_map_models(
    x = x,
    precision = precision,
    method = method,
    effects = "all",
    component = component,
    parameters = parameters,
    ...
  )

  attr(out, "data") <- insight::get_parameters(x, parameters = parameters)
  out
}



#' @export
p_map.sim.merMod <- function(x, precision = 2^10, method = "kernel", effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  out <- .p_map_models(
    x = x,
    precision = precision,
    method = method,
    effects = effects,
    component = "conditional",
    parameters = parameters,
    ...
  )

  attr(out, "data") <- insight::get_parameters(x, effects = effects, parameters = parameters)
  out
}




#' @export
p_map.sim <- function(x, precision = 2^10, method = "kernel", parameters = NULL, ...) {
  out <- .p_map_models(
    x = x,
    precision = precision,
    method = method,
    effects = "fixed",
    component = "conditional",
    parameters = parameters,
    ...
  )

  attr(out, "data") <- insight::get_parameters(x, parameters = parameters)
  out
}




#' @rdname p_map
#' @export
p_map.stanreg <- function(x, precision = 2^10, method = "kernel", effects = c("fixed", "random", "all"), component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .prepare_output(
    p_map(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), precision = precision, method = method),
    insight::clean_parameters(x),
    inherits(x, "stanmvreg")
  )

  class(out) <- unique(c("p_map", class(out)))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
p_map.stanfit <- p_map.stanreg



#' @rdname p_map
#' @export
p_map.brmsfit <- function(x, precision = 2^10, method = "kernel", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .prepare_output(
    p_map(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), precision = precision, method = method, ...),
    insight::clean_parameters(x)
  )

  class(out) <- unique(c("p_map", class(out)))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}





#' @export
p_map.BFBayesFactor <- function(x, precision = 2^10, method = "kernel", ...) {
  out <- p_map(insight::get_parameters(x), precision = precision, method = method, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}



#' @export
p_map.MCMCglmm <- function(x, precision = 2^10, method = "kernel", ...) {
  nF <- x$Fixed$nfl
  out <- p_map(as.data.frame(x$Sol[, 1:nF, drop = FALSE]), precision = precision, method = method, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}



#' @export
p_map.bayesQR <- function(x, precision = 2^10, method = "kernel", ...) {
  out <- p_map(insight::get_parameters(x), precision = precision, method = method, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}




#' @rdname as.numeric.p_direction
#' @method as.numeric p_map
#' @export
as.numeric.p_map <- function(x, ...) {
  if ("data.frame" %in% class(x)) {
    return(as.numeric(as.vector(x$p_MAP)))
  } else {
    return(as.vector(x))
  }
}


#' @method as.double p_map
#' @export
as.double.p_map <- as.numeric.p_map
