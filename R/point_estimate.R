#' Compute point-estimates of posterior distributions
#'
#' Compute various point-estimates, such as the mean, the median or the MAP, to describe posterior distributions.
#'
#' @inheritParams hdi
#' @param estimate The \href{https://easystats.github.io/bayestestR/articles/indicesEstimationComparison.html}{point-estimate(s)} to compute. Can be a character or a list with "median", "mean", "MAP" or "all".
#' @param dispersion if \code{TRUE}, computes indices of dispersion related to the estimate(s) (\code{SD} and \code{MAD} for \code{mean} and \code{median}, respectively).
#' @param ... Additional arguments to be passed to or from methods.
#'
#'
#' @examples
#' library(bayestestR)
#'
#' point_estimate(rnorm(1000))
#' point_estimate(rnorm(1000), estimate = "all", dispersion = TRUE)
#' point_estimate(rnorm(1000), estimate = c("median", "MAP"))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' point_estimate(df, estimate = "all", dispersion = TRUE)
#' point_estimate(df, estimate = c("median", "MAP"))
#' \dontrun{
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' point_estimate(model, estimate = "all", dispersion = TRUE)
#' point_estimate(model, estimate = c("median", "MAP"))
#'
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' point_estimate(model, estimate = "all", dispersion = TRUE)
#' point_estimate(model, estimate = c("median", "MAP"))
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' point_estimate(bf, estimate = "all", dispersion = TRUE)
#' point_estimate(bf, estimate = c("median", "MAP"))
#' }
#'
#' @importFrom stats mad median sd
#' @export
point_estimate <- function(x, estimate = "median", dispersion = FALSE, ...) {
  UseMethod("point_estimate")
}



#' @export
point_estimate.numeric <- function(x, estimate = "median", dispersion = FALSE, ...) {
  estimate <- match.arg(tolower(estimate), c("median", "mean", "map", "all"), several.ok = TRUE)
  if ("all" %in% estimate) {
    estimate_list <- c("median", "mean", "map")
  } else {
    estimate_list <- c(estimate)
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

  # MAP
  if ("map" %in% estimate_list) {
    out$MAP <- as.numeric(map_estimate(x))
  }

  out <- out[names(out) != ".temp"]
  out
}



#' @export
point_estimate.data.frame <- function(x, estimate = "median", dispersion = FALSE, ...) {
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    estimates <- point_estimate(x[, 1], estimate = estimate, dispersion = dispersion, ...)
  } else {
    estimates <- sapply(x, point_estimate, estimate = estimate, dispersion = dispersion, simplify = FALSE, ...)
    estimates <- do.call(rbind, estimates)
  }

  out <- cbind(data.frame("Parameter" = names(x)), estimates)
  rownames(out) <- NULL

  out
}



#' @importFrom insight get_parameters
#' @keywords internal
.point_estimate_models <- function(x, effects, component, parameters, estimate = "median", dispersion = FALSE, ...) {
  out <- point_estimate(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), estimate = estimate, dispersion = dispersion, ...)
  out$Parameter <- .get_parameter_names(x, effects = effects, component = component, parameters = parameters)

  out
}


#' @rdname point_estimate
#' @export
point_estimate.stanreg <- function(x, estimate = "median", dispersion = FALSE, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  out <- .point_estimate_models(
    x = x,
    effects = effects,
    component = "conditional",
    parameters = parameters,
    estimate = estimate,
    dispersion = dispersion,
    ...
  )
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}

#' @rdname point_estimate
#' @export
point_estimate.brmsfit <- function(x, estimate = "median", dispersion = FALSE, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .point_estimate_models(
    x = x,
    effects = effects,
    component = component,
    parameters = parameters,
    estimate = estimate,
    dispersion = dispersion,
    ...
  )
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @rdname point_estimate
#' @export
point_estimate.BFBayesFactor <- function(x, estimate = "median", dispersion = FALSE, ...) {
  out <- point_estimate(insight::get_parameters(x), , estimate = estimate, dispersion = dispersion, ...)
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}
