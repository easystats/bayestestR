#' Monte-Carlo Standard Error (MCSE)
#'
#' This function returns the Monte Carlo Standard Error (MCSE).
#'
#' @param centrality The point-estimate (centrality index) for which to compute
#' the MCSE. Can be `"median"` (default) or `"mean"`. To not break other
#' functions like `describe_posterior()` or `diagnostic_posterior()`, all other
#' values are silently converted to `"median"`.
#' @param ... Additional arguments to be passed to or from methods.
#' @inheritParams effective_sample
#'
#' @inheritSection hdi Model components
#'
#' @details **Monte Carlo Standard Error (MCSE)** is another measure of
#' accuracy of the chains. It is defined as standard deviation of the chains
#' divided by their effective sample size (the formula for `mcse()` is
#' from Kruschke 2015, p. 187). The MCSE \dQuote{provides a quantitative
#' suggestion of how big the estimation noise is}.
#'
#' @references Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press.
#'
#' @examplesIf require("rstanarm")
#' \donttest{
#' library(bayestestR)
#'
#' model <- suppressWarnings(
#'   rstanarm::stan_glm(mpg ~ wt + am, data = mtcars, chains = 1, refresh = 0)
#' )
#' mcse(model)
#' }
#' @export
mcse <- function(model, ...) {
  UseMethod("mcse")
}


#' @export
mcse.brmsfit <- function(
  model,
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  centrality = "median",
  ...
) {
  insight::check_if_installed("posterior")

  # check arguments
  if (is.null(centrality)) {
    centrality <- "median"
  }
  if (length(centrality) > 1 || !centrality %in% c("median", "mean")) {
    centrality <- "median"
  }

  params <- insight::get_parameters(
    model,
    effects = effects,
    component = component,
    parameters = parameters
  )

  mcse <- switch(
    centrality,
    median = vapply(params, posterior::mcse_mean, numeric(1)),
    mean = vapply(params, posterior::mcse_mean, numeric(1))
  )

  data.frame(
    Parameter = colnames(params),
    MCSE = mcse,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @rdname mcse
#' @export
mcse.stanreg <- function(
  model,
  effects = "fixed",
  component = "location",
  parameters = NULL,
  centrality = "median",
  ...
) {
  mcse.brmsfit(model, effects, component, parameters, centrality, ...)
}


#' @export
mcse.stanfit <- mcse.stanreg


#' @export
mcse.blavaan <- function(
  model,
  effects = "fixed",
  component = "location",
  parameters = NULL,
  ...
) {
  params <- insight::get_parameters(
    model,
    effects = effects,
    component = component,
    parameters = parameters
  )

  ess <- effective_sample(
    model,
    effects = effects,
    component = component,
    parameters = parameters
  )

  .mcse(params, stats::setNames(ess$ESS, ess$Parameter))
}


#' @keywords internal
.mcse <- function(params, ess) {
  # get standard deviations from posterior samples
  stddev <- sapply(params, stats::sd)

  # check proper length, and for unequal length, shorten all
  # objects to common parameters
  if (length(stddev) != length(ess)) {
    common <- stats::na.omit(match(names(stddev), names(ess)))
    stddev <- stddev[common]
    ess <- ess[common]
    params <- params[common]
  }

  # compute mcse
  data.frame(
    Parameter = colnames(params),
    MCSE = stddev / sqrt(ess),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
