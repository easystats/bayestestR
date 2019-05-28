#' Point-estimates of posterior distributions
#'
#' Compute various point-estimates, such as the mean, the median or the MAP, to describe posterior distributions.
#'
#' @param centrality The point-estimates (centrality indices) to compute. Can be a character or a list with "median", "mean", "MAP" or "all".
#' @param dispersion Logical, if \code{TRUE}, computes indices of dispersion related to the estimate(s) (\code{SD} and \code{MAD} for \code{mean} and \code{median}, respectively).
#' @param ... Additional arguments to be passed to or from methods.
#' @inheritParams hdi
#'
#' @references \href{https://easystats.github.io/bayestestR/articles/indicesEstimationComparison.html}{Vignette In-Depth 1: Comparison of Point-Estimates}
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
point_estimate <- function(x, centrality = "median", dispersion = FALSE, ...) {
  UseMethod("point_estimate")
}



#' @export
point_estimate.numeric <- function(x, centrality = "median", dispersion = FALSE, ...) {
  centrality <- match.arg(tolower(centrality), c("median", "mean", "map", "all"), several.ok = TRUE)
  if ("all" %in% centrality) {
    estimate_list <- c("median", "mean", "map")
  } else {
    estimate_list <- c(centrality)
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
point_estimate.data.frame <- function(x, centrality = "median", dispersion = FALSE, ...) {
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    estimates <- point_estimate(x[, 1], centrality = centrality, dispersion = dispersion, ...)
  } else {
    estimates <- sapply(x, point_estimate, centrality = centrality, dispersion = dispersion, simplify = FALSE, ...)
    estimates <- do.call(rbind, estimates)
  }

  out <- cbind(data.frame("Parameter" = names(x), stringsAsFactors = FALSE), estimates)
  rownames(out) <- NULL

  out
}



#' @importFrom insight get_parameters
#' @keywords internal
.point_estimate_models <- function(x, effects, component, parameters, centrality = "median", dispersion = FALSE, ...) {
  out <- point_estimate(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), centrality = centrality, dispersion = dispersion, ...)
  # out$Parameter <- .get_parameter_names(x, effects = effects, component = component, parameters = parameters)

  out
}



#' @rdname point_estimate
#' @export
point_estimate.stanreg <- function(x, centrality = "median", dispersion = FALSE, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
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
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}

#' @rdname point_estimate
#' @export
point_estimate.brmsfit <- function(x, centrality = "median", dispersion = FALSE, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .point_estimate_models(
    x = x,
    effects = effects,
    component = component,
    parameters = parameters,
    centrality = centrality,
    dispersion = dispersion,
    ...
  )
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @rdname point_estimate
#' @export
point_estimate.BFBayesFactor <- function(x, centrality = "median", dispersion = FALSE, ...) {
  out <- point_estimate(insight::get_parameters(x), centrality = centrality, dispersion = dispersion, ...)
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}
