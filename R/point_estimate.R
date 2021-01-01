#' Point-estimates of posterior distributions
#'
#' Compute various point-estimates, such as the mean, the median or the MAP, to describe posterior distributions.
#'
#' @param centrality The point-estimates (centrality indices) to compute.  Character (vector) or list with one or more of these options: \code{"median"}, \code{"mean"}, \code{"MAP"} or \code{"all"}.
#' @param dispersion Logical, if \code{TRUE}, computes indices of dispersion related to the estimate(s) (\code{SD} and \code{MAD} for \code{mean} and \code{median}, respectively).
#' @param threshold For \code{centrality = "trimmed"} (i.e. trimmed mean), indicates the fraction (0 to 0.5) of observations to be trimmed from each end of the vector before the mean is computed.
#' @param ... Additional arguments to be passed to or from methods.
#' @inheritParams hdi
#'
#' @references \href{https://easystats.github.io/bayestestR/articles/indicesEstimationComparison.html}{Vignette In-Depth 1: Comparison of Point-Estimates}
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/bayestestR.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' library(bayestestR)
#'
#' point_estimate(rnorm(1000))
#' point_estimate(rnorm(1000), centrality = "all", dispersion = TRUE)
#' point_estimate(rnorm(1000), centrality = c("median", "MAP"))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' point_estimate(df, centrality = "all", dispersion = TRUE)
#' point_estimate(df, centrality = c("median", "MAP"))
#' \dontrun{
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' point_estimate(model, centrality = "all", dispersion = TRUE)
#' point_estimate(model, centrality = c("median", "MAP"))
#'
#'
#' # emmeans estimates
#' # -----------------------------------------------
#' library(emmeans)
#' point_estimate(emtrends(model, ~1, "wt"), centrality = c("median", "MAP"))
#'
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' point_estimate(model, centrality = "all", dispersion = TRUE)
#' point_estimate(model, centrality = c("median", "MAP"))
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' point_estimate(bf, centrality = "all", dispersion = TRUE)
#' point_estimate(bf, centrality = c("median", "MAP"))
#' }
#'
#' @importFrom stats mad median sd
#' @export
point_estimate <- function(x, centrality = "all", dispersion = FALSE, ...) {
  UseMethod("point_estimate")
}



#' @export
point_estimate.numeric <- function(x, centrality = "all", dispersion = FALSE, threshold = .1, ...) {
  centrality <- match.arg(tolower(centrality), c("median", "mean", "map", "trimmed", "all"), several.ok = TRUE)
  if ("all" %in% centrality) {
    estimate_list <- c("median", "mean", "map")
  } else {
    estimate_list <- centrality
  }

  out <- data.frame(".temp" = 0)

  # Median
  if ("median" %in% estimate_list) {
    out$Median <- stats::median(x)
    if (dispersion) {
      out$MAD <- stats::mad(x)
    }
  }

  # Mean
  if ("mean" %in% estimate_list) {
    out$Mean <- mean(x)
    if (dispersion) {
      out$SD <- stats::sd(x)
    }
  }

  # trimmed mean
  if ("trimmed" %in% estimate_list) {
    out$Trimmed_Mean <- mean(x, trim = threshold)
    if (dispersion) {
      out$SD <- stats::sd(x)
    }
  }

  # MAP
  if ("map" %in% estimate_list) {
    out$MAP <- as.numeric(map_estimate(x))
  }

  out <- out[names(out) != ".temp"]
  attr(out, "data") <- x
  attr(out, "centrality") <- centrality
  class(out) <- unique(c("point_estimate", "see_point_estimate", class(out)))

  out
}



#' @export
point_estimate.data.frame <- function(x, centrality = "all", dispersion = FALSE, threshold = .1, ...) {
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    estimates <- point_estimate(x[, 1], centrality = centrality, dispersion = dispersion, threshold = threshold, ...)
  } else {
    estimates <- sapply(x, point_estimate, centrality = centrality, dispersion = dispersion, simplify = FALSE, ...)
    estimates <- do.call(rbind, estimates)
  }

  out <- cbind(data.frame("Parameter" = names(x), stringsAsFactors = FALSE), estimates)
  rownames(out) <- NULL
  attr(out, "data") <- x
  attr(out, "centrality") <- centrality
  class(out) <- unique(c("point_estimate", "see_point_estimate", class(out)))

  out
}


#' @export
point_estimate.mcmc <- function(x, centrality = "all", dispersion = FALSE, ...) {
  point_estimate(as.data.frame(x), centrality = centrality, dispersion = dispersion, ...)
}


#' @export
point_estimate.bcplm <- function(x, centrality = "all", dispersion = FALSE, ...) {
  point_estimate(insight::get_parameters(x), centrality = centrality, dispersion = dispersion, ...)
}

#' @export
point_estimate.bayesQR <- point_estimate.bcplm


#' @export
point_estimate.mcmc.list <- point_estimate.bcplm


#' @export
point_estimate.bamlss <- function(x, centrality = "all", dispersion = FALSE,  component = c("conditional", "location", "all"), ...) {
  point_estimate(insight::get_parameters(x, component = component), centrality = centrality, dispersion = dispersion, ...)
}


#' @export
point_estimate.MCMCglmm <- function(x, centrality = "all", dispersion = FALSE, ...) {
  nF <- x$Fixed$nfl
  point_estimate(as.data.frame(x$Sol[, 1:nF, drop = FALSE]), centrality = centrality, dispersion = dispersion, ...)
}


#' @export
point_estimate.emmGrid <- function(x, centrality = "all", dispersion = FALSE, ...) {
  xdf <- insight::get_parameters(x)

  out <- point_estimate(xdf, centrality = centrality, dispersion = dispersion, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
point_estimate.emm_list <- point_estimate.emmGrid



#' @importFrom insight get_parameters
#' @keywords internal
.point_estimate_models <- function(x, effects, component, parameters, centrality = "all", dispersion = FALSE, ...) {
  out <- point_estimate(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), centrality = centrality, dispersion = dispersion, ...)
  # out$Parameter <- .get_parameter_names(x, effects = effects, component = component, parameters = parameters)
  out
}


#' @importFrom insight get_parameters clean_parameters
#' @rdname point_estimate
#' @export
point_estimate.stanreg <- function(x, centrality = "all", dispersion = FALSE, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  out <- .prepare_output(
    point_estimate(insight::get_parameters(x, effects = effects, parameters = parameters), centrality = centrality, dispersion = dispersion, ...),
    insight::clean_parameters(x),
    inherits(x, "stanmvreg")
  )

  attr(out, "object_name") <- .safe_deparse(substitute(x))
  attr(out, "centrality") <- centrality
  class(out) <- unique(c("point_estimate", "see_point_estimate", class(out)))

  out
}

#' @export
point_estimate.stanfit <- point_estimate.stanreg


#' @rdname point_estimate
#' @export
point_estimate.brmsfit <- function(x, centrality = "all", dispersion = FALSE, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .prepare_output(
    point_estimate(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), centrality = centrality, dispersion = dispersion, ...),
    insight::clean_parameters(x)
  )

  attr(out, "object_name") <- .safe_deparse(substitute(x))
  attr(out, "centrality") <- centrality
  class(out) <- unique(c("point_estimate", "see_point_estimate", class(out)))

  out
}


#' @export
point_estimate.sim.merMod <- function(x, centrality = "all", dispersion = FALSE, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  out <- .point_estimate_models(
    x = x,
    effects = effects,
    component = "conditional",
    parameters = parameters,
    centrality = centrality,
    dispersion = dispersion,
    ...
  )
  attr(out, "data") <- insight::get_parameters(x, effects = effects, parameters = parameters)
  attr(out, "centrality") <- centrality
  class(out) <- unique(c("point_estimate", "see_point_estimate", class(out)))

  out
}



#' @export
point_estimate.sim <- function(x, centrality = "all", dispersion = FALSE, parameters = NULL, ...) {
  out <- .point_estimate_models(
    x = x,
    effects = "fixed",
    component = "conditional",
    parameters = parameters,
    centrality = centrality,
    dispersion = dispersion,
    ...
  )
  attr(out, "data") <- insight::get_parameters(x, parameters = parameters)
  attr(out, "centrality") <- centrality
  class(out) <- unique(c("point_estimate", "see_point_estimate", class(out)))

  out
}



#' @rdname point_estimate
#' @export
point_estimate.BFBayesFactor <- function(x, centrality = "all", dispersion = FALSE, ...) {
  out <- point_estimate(insight::get_parameters(x), centrality = centrality, dispersion = dispersion, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  attr(out, "centrality") <- centrality
  class(out) <- unique(c("point_estimate", "see_point_estimate", class(out)))

  out
}
