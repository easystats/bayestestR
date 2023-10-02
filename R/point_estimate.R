#' Point-estimates of posterior distributions
#'
#' Compute various point-estimates, such as the mean, the median or the MAP, to describe posterior distributions.
#'
#' @param centrality The point-estimates (centrality indices) to compute. Character
#' (vector) or list with one or more of these options: `"median"`, `"mean"`, `"MAP"`
#' (see [`map_estimate()`]), `"trimmed"` (which is just `mean(x, trim = threshold)`),
#' `"mode"` or `"all"`.
#' @param dispersion Logical, if `TRUE`, computes indices of dispersion related
#' to the estimate(s) (`SD` and `MAD` for `mean` and `median`, respectively).
#' Dispersion is not available for `"MAP"` or `"mode"` centrality indices.
#' @param threshold For `centrality = "trimmed"` (i.e. trimmed mean), indicates
#' the fraction (0 to 0.5) of observations to be trimmed from each end of the
#' vector before the mean is computed.
#' @param ... Additional arguments to be passed to or from methods.
#' @inheritParams hdi
#'
#' @references Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., and Lüdecke, D.
#' (2019). *Indices of Effect Existence and Significance in the Bayesian Framework*.
#' Frontiers in Psychology 2019;10:2767. \doi{10.3389/fpsyg.2019.02767}
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examplesIf require("rstanarm") && require("emmeans") && require("brms") && require("BayesFactor")
#' library(bayestestR)
#'
#' point_estimate(rnorm(1000))
#' point_estimate(rnorm(1000), centrality = "all", dispersion = TRUE)
#' point_estimate(rnorm(1000), centrality = c("median", "MAP"))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' point_estimate(df, centrality = "all", dispersion = TRUE)
#' point_estimate(df, centrality = c("median", "MAP"))
#' \donttest{
#' # rstanarm models
#' # -----------------------------------------------
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' point_estimate(model, centrality = "all", dispersion = TRUE)
#' point_estimate(model, centrality = c("median", "MAP"))
#'
#'
#' # emmeans estimates
#' # -----------------------------------------------
#' point_estimate(
#'   emmeans::emtrends(model, ~1, "wt", data = mtcars),
#'   centrality = c("median", "MAP")
#' )
#'
#' # brms models
#' # -----------------------------------------------
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' point_estimate(model, centrality = "all", dispersion = TRUE)
#' point_estimate(model, centrality = c("median", "MAP"))
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
#' point_estimate(bf, centrality = "all", dispersion = TRUE)
#' point_estimate(bf, centrality = c("median", "MAP"))
#' }
#'
#' @export
point_estimate <- function(x, ...) {
  UseMethod("point_estimate")
}


#' @export
point_estimate.default <- function(x, ...) {
  insight::format_error(
    paste0("'point_estimate()' is not yet implemented for objects of class '", class(x)[1], "'.")
  )
}


#' @rdname point_estimate
#' @export
point_estimate.numeric <- function(x, centrality = "all", dispersion = FALSE, threshold = 0.1, ...) {
  centrality <- match.arg(tolower(centrality), c("median", "mean", "map", "trimmed", "mode", "all"), several.ok = TRUE)
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

  # MODE
  if ("mode" %in% estimate_list) {
    out$Mode <- .mode_estimate(x)
  }


  out <- out[names(out) != ".temp"]
  attr(out, "data") <- x
  attr(out, "centrality") <- centrality
  class(out) <- unique(c("point_estimate", "see_point_estimate", class(out)))

  out
}


#' @export
point_estimate.data.frame <- function(x, centrality = "all", dispersion = FALSE, threshold = 0.1, ...) {
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
point_estimate.draws <- function(x, centrality = "all", dispersion = FALSE, threshold = 0.1, ...) {
  point_estimate(
    .posterior_draws_to_df(x),
    centrality = centrality,
    dispersion = dispersion,
    threshold = threshold,
    ...
  )
}

#' @export
point_estimate.rvar <- point_estimate.draws


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
point_estimate.blrm <- point_estimate.bcplm


#' @export
point_estimate.mcmc.list <- point_estimate.bcplm


#' @export
point_estimate.BGGM <- point_estimate.bcplm


#' @export
point_estimate.bamlss <- function(x, centrality = "all", dispersion = FALSE, component = c("conditional", "location", "all"), ...) {
  component <- match.arg(component)
  out <- point_estimate(
    insight::get_parameters(x, component = component),
    centrality = centrality,
    dispersion = dispersion,
    ...
  )
  .add_clean_parameters_attribute(out, x)
}


#' @export
point_estimate.MCMCglmm <- function(x, centrality = "all", dispersion = FALSE, ...) {
  nF <- x$Fixed$nfl
  point_estimate(
    as.data.frame(x$Sol[, 1:nF, drop = FALSE]),
    centrality = centrality,
    dispersion = dispersion,
    ...
  )
}


#' @export
point_estimate.emmGrid <- function(x, centrality = "all", dispersion = FALSE, ...) {
  xdf <- insight::get_parameters(x)

  out <- point_estimate(xdf, centrality = centrality, dispersion = dispersion, ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
point_estimate.emm_list <- point_estimate.emmGrid


#' @rdname point_estimate
#' @export
point_estimate.stanreg <- function(x, centrality = "all", dispersion = FALSE, effects = c("fixed", "random", "all"), component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  cleaned_parameters <- insight::clean_parameters(x)

  out <- .prepare_output(
    point_estimate(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), centrality = centrality, dispersion = dispersion, ...),
    cleaned_parameters,
    inherits(x, "stanmvreg")
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(out, "centrality") <- centrality
  attr(out, "clean_parameters") <- cleaned_parameters
  class(out) <- unique(c("point_estimate", "see_point_estimate", class(out)))

  out
}

#' @export
point_estimate.stanfit <- point_estimate.stanreg

#' @export
point_estimate.blavaan <- point_estimate.stanreg


#' @rdname point_estimate
#' @export
point_estimate.brmsfit <- function(x, centrality = "all", dispersion = FALSE, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  cleaned_parameters <- insight::clean_parameters(x)

  out <- .prepare_output(
    point_estimate(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), centrality = centrality, dispersion = dispersion, ...),
    cleaned_parameters
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(out, "centrality") <- centrality
  attr(out, "clean_parameters") <- cleaned_parameters
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
  out <- .add_clean_parameters_attribute(out, x)
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
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(out, "centrality") <- centrality
  class(out) <- unique(c("point_estimate", "see_point_estimate", class(out)))

  out
}


#' @export
point_estimate.matrix <- function(x, ...) {
  point_estimate(as.data.frame(x), ...)
}


#' @export
point_estimate.get_predicted <- function(x, ...) {
  if ("iterations" %in% names(attributes(x))) {
    point_estimate(as.data.frame(t(attributes(x)$iterations)), ...)
  } else {
    as.numeric(x)
  }
}


# Helper ------------------------------------------------------------------

#' @keywords internal
.point_estimate_models <- function(x, effects, component, parameters, centrality = "all", dispersion = FALSE, ...) {
  point_estimate(
    insight::get_parameters(x, effects = effects, component = component, parameters = parameters),
    centrality = centrality,
    dispersion = dispersion,
    ...
  )
}

#' @keywords internal
.mode_estimate <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
