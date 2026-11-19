#' Monte-Carlo Standard Error (MCSE)
#'
#' This function returns the Monte Carlo Standard Error (MCSE).
#'
#' @param centrality The point-estimate (centrality index) for which to compute
#' the MCSE. Can be `"median"` (default) or `"mean"`. Other values are
#' silently converted to `"median"`.
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

  params <- insight::find_parameters(
    model,
    effects = effects,
    component = component,
    parameters = parameters,
    flatten = TRUE
  )

  .mcse_draws(
    posterior::as_draws_array(model),
    centrality = centrality,
    pars = params
  )
}


#' @rdname mcse
#' @export
mcse.stanreg <- mcse.brmsfit


#' @export
mcse.stanfit <- mcse.stanreg


#' @export
mcse.blavaan <- function(
  model,
  effects = "fixed",
  component = "location",
  parameters = NULL,
  centrality = "median",
  ...
) {
  insight::check_if_installed("blavaan")

  params <- suppressWarnings(insight::find_parameters(
    model,
    effects = effects,
    component = component,
    parameters = parameters,
    flatten = TRUE
  ))

  .mcse_draws(
    blavaan::blavInspect(model, what = "mcmc"),
    centrality = centrality,
    pars = params
  )
}


.mcse_draws <- function(
  draws,
  centrality = "median",
  pars = NULL,
  parameters = NULL
) {
  .diagnostic_posterior_draws(
    draws,
    diagnostic = "MCSE",
    centrality = centrality,
    pars = pars,
    parameters = parameters
  )
}


#' @export
mcse.CmdStanFit <- function(model, centrality = "median", parameters = NULL, ...) {
  insight::check_if_installed("cmdstanr")

  .mcse_draws(
    model$draws(),
    centrality = centrality,
    pars = insight::find_parameters(model, flatten = TRUE),
    parameters = parameters
  )
}

#' @export
mcse.stanli_fit <- function(model, centrality = "median", parameters = NULL, ...) {
  insight::check_if_installed("stanli")

  .mcse_draws(
    stanli::as_draws_array(model),
    centrality = centrality,
    parameters = parameters
  )
}
