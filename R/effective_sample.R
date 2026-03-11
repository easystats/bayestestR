#' Effective Sample Size (ESS)
#'
#' Effective Sample Size (ESS) is a measure of how much independent information
#' there is in autocorrelated chains. It is used to assess the quality of MCMC
#' samples. A higher ESS indicates more reliable estimates. For most
#' applications, an effective sample size greater than 1,000 is sufficient for
#' stable estimates (Bürkner, 2017). This function returns the effective sample
#' size (ESS) for various Bayesian model objects. For `brmsfit` objects, the
#' returned ESS corresponds to the bulk-ESS (and the tail-ESS is also returned).
#'
#' @param model A `stanreg`, `stanfit`, `brmsfit`, `blavaan`, or `MCMCglmm` object.
#' @param ... Currently not used.
#' @inheritParams hdi
#'
#' @inheritSection hdi Model components
#'
#' @return A data frame with two columns: Parameter name and effective sample size (ESS).
#'
#' @details
#'   - **Effective Sample (ESS)** should be as large as possible, altough
#'   for most applications, an effective sample size greater than 1,000 is
#'   sufficient for stable estimates (Bürkner, 2017). The ESS corresponds to the
#'   number of independent samples with the same estimation power as the N
#'   autocorrelated samples. It is is a measure of \dQuote{how much independent
#'   information there is in autocorrelated chains} (*Kruschke 2015, p182-3*).
#'
#'   - **Bulk-ESS** is useful as a diagnostic for the sampling efficiency in
#'   the bulk of the posterior. It is defined as the effective sample size for
#'   rank normalized values using split chains. It can be interpreted as the
#'   reliability of indices of central tendency (mean, median, etc.).
#'
#'   - **Tail-ESS** is useful as a diagnostic for the sampling efficiency in
#'   the tails of the posterior. It is defined as the minimum of the effective
#'   sample sizes for 5% and 95% quantiles. It can be interpreted as the
#'   reliability of indices that depend on the tails of the distribution (e.g.,
#'   credible intervals, tail probabilities, etc.).
#'
#' @references
#' - Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS,
#'   and Stan. Academic Press.
#' - Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models
#'   using Stan. Journal of Statistical Software, 80(1), 1-28
#' - Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C.
#'   (2021). Rank-normalization, folding, and localization: An improved R-hat
#'   for assessing convergence of MCMC. Bayesian Analysis, 16(2), 667-718.
#'
#' @examplesIf all(insight::check_if_installed(c("rstanarm", "brms", "posterior"), quietly = TRUE))
#' \donttest{
#' model <- suppressWarnings(rstanarm::stan_glm(
#'   mpg ~ wt + gear,
#'   data = mtcars,
#'   chains = 2,
#'   iter = 200,
#'   refresh = 0
#' ))
#' effective_sample(model)
#'
#' model <- suppressWarnings(brms::brm(
#'   mpg ~ wt,
#'   data = mtcars,
#'   chains = 2,
#'   iter = 200,
#'   refresh = 0
#' ))
#' effective_sample(model)
#' }
#' @export
effective_sample <- function(model, ...) {
  UseMethod("effective_sample")
}


#' @export
effective_sample.default <- function(model, ...) {
  insight::format_error(
    paste0(
      "'effective_sample()' is not yet implemented for objects of class '",
      class(model)[1],
      "'."
    )
  )
}


#' @rdname effective_sample
#' @export
effective_sample.brmsfit <- function(model,
                                     effects = "fixed",
                                     component = "conditional",
                                     parameters = NULL,
                                     ...) {
  pars <- insight::find_parameters(
    model,
    effects = effects,
    component = component,
    parameters = parameters,
    flatten = TRUE
  )

  insight::check_if_installed("posterior")
  idx <- as.data.frame(posterior::summarise_draws(model))
  rows_to_keep <- idx$variable %in% pars
  # ess_*() functions are defined in:
  # https://github.com/stan-dev/posterior/blob/master/R/convergence.R

  data.frame(
    Parameter = idx$variable[rows_to_keep],
    ESS = round(idx[rows_to_keep, "ess_bulk"]),
    ESS_tail = round(idx[rows_to_keep, "ess_tail"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @export
effective_sample.stanreg <- function(model,
                                     effects = "fixed",
                                     component = "location",
                                     parameters = NULL,
                                     ...) {
  effective_sample.brmsfit(
    model,
    effects = effects,
    component = component,
    parameters = parameters,
    ...
  )
}


#' @export
effective_sample.stanmvreg <- function(model,
                                       effects = "fixed",
                                       component = "location",
                                       parameters = NULL,
                                       ...) {
  pars <- insight::get_parameters(
    model,
    effects = effects,
    component = component,
    parameters = parameters
  )

  insight::check_if_installed("posterior")
  idx <- as.data.frame(posterior::summarise_draws(model))
  rows_to_keep <- idx$variable %in% colnames(pars)
  # ess_*() functions are defined in:
  # https://github.com/stan-dev/posterior/blob/master/R/convergence.R

  data.frame(
    Parameter = idx$variable[rows_to_keep],
    ESS = round(idx[rows_to_keep, "ess_bulk"]),
    ESS_tail = round(idx[rows_to_keep, "ess_tail"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @export
effective_sample.stanfit <- function(model,
                                     effects = "fixed",
                                     parameters = NULL,
                                     ...) {
  pars <- insight::get_parameters(
    model,
    effects = effects,
    parameters = parameters
  )

  insight::check_if_installed("rstan")

  s <- as.data.frame(rstan::summary(model)$summary)
  s <- s[rownames(s) %in% colnames(pars), ]

  data.frame(
    Parameter = rownames(s),
    ESS = s[["n_eff"]],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @export
effective_sample.CmdStanFit <- function(model, ...) {
  diagnostic_posterior(model, diagnostic = c("ESS", "ESS_tail"))
}

#' @export
effective_sample.blavaan <- function(model, parameters = NULL, ...) {
  insight::check_if_installed("blavaan")

  ESS <- blavaan::blavInspect(model, what = "neff")

  data.frame(
    Parameter = colnames(insight::get_parameters(model)),
    ESS = ESS,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @export
effective_sample.MCMCglmm <- function(model,
                                      effects = "fixed",
                                      parameters = NULL,
                                      ...) {
  pars <- insight::get_parameters(
    model,
    effects = effects,
    parameters = parameters,
    summary = TRUE
  )

  s.fixed <- as.data.frame(summary(model)$solutions)
  s.random <- as.data.frame(summary(model)$Gcovariances)

  es <- data.frame(
    Parameter = rownames(s.fixed),
    ESS = round(s.fixed[["eff.samp"]]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (nrow(s.random) > 0L) {
    es <- rbind(es, data.frame(
      Parameter = rownames(s.random),
      ESS = round(s.random[["eff.samp"]]),
      stringsAsFactors = FALSE,
      row.names = NULL
    ))
  }

  es[match(pars[[1]], es$Parameter), ]
}
