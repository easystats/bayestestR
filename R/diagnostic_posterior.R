#' Posteriors Sampling Diagnostic
#'
#' Extract diagnostic metrics (Effective Sample Size (\code{ESS}), \code{Rhat} and Monte Carlo Standard Error \code{MCSE}).
#'
#' @param posteriors A stanreg or brms model.
#' @param diagnostic Diagnostic metrics to compute. Can be a character or a list with "ESS", "Rhat", "MCSE" or "all".
#'
#' @details
#'   \strong{Effective Sample (ESS)} should be as large as possible, altough for most applications, an effective sample size greater than 1,000 is sufficient for stable estimates (Bürkner, 2017). The ESS corresponds to the number of independent samples with the same estimation power as the N autocorrelated samples. It represents the amount by which autocorrelation within the chains increases uncertainty of estimates.
#'   \cr \cr
#'   \strong{Rhat} should be the closest to 1. It should not be larger than 1.1 (Gelman and Rubin, 1992) or 1.01 (Vehtari et al., 2019). The split R-hat statistic quantifies the consistency of an ensemble of Markov chains.
#'   \cr \cr
#'   \strong{Monte Carlo Standard Error (MCSE)} is not implemented yet.
#'
#'
#' @examples
#' \dontrun{
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' diagnostic_posterior(model)
#'
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' diagnostic_posterior(model)
#'
#' }
#'
#' @references
#' \itemize{
#'   \item Gelman, A., \& Rubin, D. B. (1992). Inference from iterative simulation using multiple sequences. Statistical science, 7(4), 457-472.
#'   \item Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., \& Bürkner, P. C. (2019). Rank-normalization, folding, and localization: An improved Rhat for assessing convergence of MCMC. arXiv preprint arXiv:1903.08008.
#' }
#' @export
diagnostic_posterior <- function(posteriors, diagnostic = c("ESS", "Rhat"), ...) {
  UseMethod("diagnostic_posterior")
}



#' @export
diagnostic_posterior.numeric <- function(posteriors, diagnostic = c("ESS", "Rhat"), ...) {
  stop("`diagnostic_posterior` only works with rstanarm or brms models.")
}

#' @export
diagnostic_posterior.data.frame <- diagnostic_posterior.numeric

#' @export
diagnostic_posterior.BFBayesFactor <- diagnostic_posterior.numeric



#' @inheritParams insight::get_parameters
#' @rdname diagnostic_posterior
#' @export
diagnostic_posterior.stanreg <- function(posteriors, diagnostic = c("ESS", "Rhat"), effects = c("fixed", "random", "all"), parameters = NULL, ...) {

  # TODO: MCSE

  diagnostic <- match.arg(diagnostic, c("ESS", "Rhat", "MCSE", "all"), several.ok = TRUE)
  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat") # Add MCSE
  } else {
    diagnostic <- c(diagnostic)
  }

  # Get indices and rename
  diagnostic_df <- as.data.frame(posteriors$stan_summary)
  diagnostic_df$Parameter <- row.names(diagnostic_df)
  diagnostic_df$ESS <- diagnostic_df$n_eff

  # Select columns
  diagnostic_df <- diagnostic_df[, c("Parameter", diagnostic)]
  row.names(diagnostic_df) <- NULL

  # Select rows
  effects <- match.arg(effects)
  params <- insight::find_parameters(posteriors, effects = effects, parameters = parameters, flatten = TRUE)

  diagnostic_df[diagnostic_df$Parameter %in% params, ]
}
