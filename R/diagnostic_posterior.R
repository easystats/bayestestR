#' Posteriors Sampling Diagnostic
#'
#' Extract diagnostic metrics (Effective Sample Size (\code{ESS}), \code{Rhat} and Monte Carlo Standard Error \code{MCSE}).
#'
#' @param posteriors A stanreg or brms model.
#' @param diagnostic Diagnostic metrics to compute.  Character (vector) or list with one or more of these options: \code{"ESS"}, \code{"Rhat"}, \code{"MCSE"} or \code{"all"}.
#'
#' @details
#'   \strong{Effective Sample (ESS)} should be as large as possible, altough for most applications, an effective sample size greater than 1000 is sufficient for stable estimates (Bürkner, 2017). The ESS corresponds to the number of independent samples with the same estimation power as the N autocorrelated samples. It is is a measure of \dQuote{how much independent information there is in autocorrelated chains} (\emph{Kruschke 2015, p182-3}).
#'   \cr \cr
#'   \strong{Rhat} should be the closest to 1. It should not be larger than 1.1 (Gelman and Rubin, 1992) or 1.01 (Vehtari et al., 2019). The split R-hat statistic quantifies the consistency of an ensemble of Markov chains.
#'   \cr \cr
#'   \strong{Monte Carlo Standard Error (MCSE)} is another measure of accuracy of the chains. It is defined as standard deviation of the chains divided by their effective sample size (the formula for \code{mcse()} is from Kruschke 2015, p. 187). The MCSE \dQuote{provides a quantitative suggestion of how big the estimation noise is}.
#'
#'
#' @examples
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' diagnostic_posterior(model)
#' \dontrun{
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' diagnostic_posterior(model)
#' }
#'
#' @references
#' \itemize{
#'   \item Gelman, A., & Rubin, D. B. (1992). Inference from iterative simulation using multiple sequences. Statistical science, 7(4), 457-472.
#'   \item Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., \& Bürkner, P. C. (2019). Rank-normalization, folding, and localization: An improved Rhat for assessing convergence of MCMC. arXiv preprint arXiv:1903.08008.
#'   \item Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press.
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
diagnostic_posterior.stanreg <- function(posteriors, diagnostic = "all", effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  diagnostic <- match.arg(diagnostic, c("ESS", "Rhat", "MCSE", "all"), several.ok = TRUE)
  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE")
  } else {
    diagnostic <- c(diagnostic)
  }

  # Get indices and rename
  diagnostic_df <- as.data.frame(posteriors$stan_summary)
  diagnostic_df$Parameter <- row.names(diagnostic_df)
  diagnostic_df$ESS <- round(diagnostic_df$n_eff)
  # special handling for MCSE, due to some parameters (like lp__) missing in rows
  MCSE <- mcse(posteriors, effects = "all")
  diagnostic_df <- merge(diagnostic_df, MCSE, by = "Parameter", all = FALSE)

  # Select columns
  diagnostic_df <- diagnostic_df[, c("Parameter", diagnostic)]
  row.names(diagnostic_df) <- NULL

  # Select rows
  effects <- match.arg(effects)
  params <- colnames(
    insight::get_parameters(posteriors, effects = effects, parameters = parameters)
  )

  diagnostic_df[diagnostic_df$Parameter %in% params, ]
}


#' @inheritParams insight::get_parameters
#' @rdname diagnostic_posterior
#' @export
diagnostic_posterior.brmsfit <- function(posteriors, diagnostic = "all", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  diagnostic <- match.arg(diagnostic, c("ESS", "Rhat", "MCSE", "all"), several.ok = TRUE)
  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE") # Add MCSE
  } else {
    diagnostic <- c(diagnostic)
  }

  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' required for this function to work. Please install it by running `install.packages('rstan')`.")
  }

  # Get indices and rename
  diagnostic_df <- as.data.frame(rstan::summary(posteriors$fit)$summary)
  diagnostic_df$Parameter <- make.names(row.names(diagnostic_df))
  diagnostic_df$ESS <- round(diagnostic_df$n_eff)
  # special handling for MCSE, due to some parameters (like lp__) missing in rows
  MCSE <- mcse(posteriors, effects = "all", component = "all")
  diagnostic_df <- merge(diagnostic_df, MCSE, by = "Parameter", all = FALSE)

  # Select columns
  diagnostic_df <- diagnostic_df[, c("Parameter", diagnostic)]
  row.names(diagnostic_df) <- NULL

  # Select rows
  effects <- match.arg(effects)
  component <- match.arg(component)
  params <-
    colnames(
      insight::get_parameters(
        posteriors,
        effects = effects,
        component = component,
        parameters = parameters
      )
    )

  diagnostic_df[diagnostic_df$Parameter %in% params, ]
}
