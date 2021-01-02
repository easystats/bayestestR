#' Posteriors Sampling Diagnostic
#'
#' Extract diagnostic metrics (Effective Sample Size (\code{ESS}), \code{Rhat} and Monte Carlo Standard Error \code{MCSE}).
#'
#' @param posteriors A stanreg or brms model.
#' @param diagnostic Diagnostic metrics to compute.  Character (vector) or list with one or more of these options: \code{"ESS"}, \code{"Rhat"}, \code{"MCSE"} or \code{"all"}.
#'
#' @details
#'   \strong{Effective Sample (ESS)} should be as large as possible, although for most applications, an effective sample size greater than 1000 is sufficient for stable estimates (Bürkner, 2017). The ESS corresponds to the number of independent samples with the same estimation power as the N autocorrelated samples. It is is a measure of \dQuote{how much independent information there is in autocorrelated chains} (\cite{Kruschke 2015, p182-3}).
#'   \cr \cr
#'   \strong{Rhat} should be the closest to 1. It should not be larger than 1.1 (\cite{Gelman and Rubin, 1992}) or 1.01 (\cite{Vehtari et al., 2019}). The split R-hat statistic quantifies the consistency of an ensemble of Markov chains.
#'   \cr \cr
#'   \strong{Monte Carlo Standard Error (MCSE)} is another measure of accuracy of the chains. It is defined as standard deviation of the chains divided by their effective sample size (the formula for \code{mcse()} is from Kruschke 2015, p. 187). The MCSE \dQuote{provides a quantitative suggestion of how big the estimation noise is}.
#'
#'
#' @examples
#' \dontrun{
#' # rstanarm models
#' # -----------------------------------------------
#' if (require("rstanarm", quietly = TRUE)) {
#'   model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#'   diagnostic_posterior(model)
#' }
#'
#' # brms models
#' # -----------------------------------------------
#' if (require("brms", quietly = TRUE)) {
#'   model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#'   diagnostic_posterior(model)
#' }
#' }
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



#' @inheritParams insight::get_parameters.BFBayesFactor
#' @inheritParams insight::get_parameters
#' @rdname diagnostic_posterior
#' @export
diagnostic_posterior.stanreg <- function(posteriors, diagnostic = "all", effects = c("fixed", "random", "all"), component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"), parameters = NULL, ...) {

  # Find parameters
  effects <- match.arg(effects)
  component <- match.arg(component)
  params <- insight::find_parameters(posteriors, effects = effects, component = component, parameters = parameters, flatten = TRUE)

  # If no diagnostic
  if (is.null(diagnostic)) {
    return(data.frame("Parameter" = params))
  }

  diagnostic <- match.arg(diagnostic, c("ESS", "Rhat", "MCSE", "all"), several.ok = TRUE)
  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE", "khat")
  } else {
    diagnostic <- c(diagnostic)
    if ("Rhat" %in% diagnostic) diagnostic <- c(diagnostic, "khat")
  }

  # Get indices and rename
  diagnostic_df <- as.data.frame(posteriors$stan_summary)
  diagnostic_df$Parameter <- row.names(diagnostic_df)
  if ("n_eff" %in% names(diagnostic_df)) {
    diagnostic_df$ESS <- diagnostic_df$n_eff
  }
  # special handling for MCSE, due to some parameters (like lp__) missing in rows
  MCSE <- mcse(posteriors, effects = "all")
  diagnostic_df <- merge(diagnostic_df, MCSE, by = "Parameter", all = FALSE)

  # Select columns
  available_columns <- intersect(colnames(diagnostic_df), c("Parameter", diagnostic))
  diagnostic_df <- diagnostic_df[available_columns]
  names(diagnostic_df)[available_columns == "khat"] <- "Khat"
  row.names(diagnostic_df) <- NULL

  # Remove columns with all Nans
  diagnostic_df <- diagnostic_df[!sapply(diagnostic_df, function(x) all(is.na(x)))]

  # Select rows
  diagnostic_df[diagnostic_df$Parameter %in% params, ]
}


#' @inheritParams insight::get_parameters
#' @rdname diagnostic_posterior
#' @export
diagnostic_posterior.stanmvreg <- function(posteriors, diagnostic = "all", effects = c("fixed", "random", "all"), parameters = NULL, ...) {

  # Find parameters
  effects <- match.arg(effects)
  all_params <- insight::find_parameters(posteriors, effects = effects, parameters = parameters, flatten = FALSE)

  params <- unlist(lapply(names(all_params), function(i) {
    all_params[[i]]$sigma <- NULL
    unlist(all_params[[i]])
  }))

  # If no diagnostic
  if (is.null(diagnostic)) {
    return(data.frame("Parameter" = params))
  }

  diagnostic <- match.arg(diagnostic, c("ESS", "Rhat", "MCSE", "all"), several.ok = TRUE)
  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE", "khat")
  } else {
    diagnostic <- c(diagnostic)
    if ("Rhat" %in% diagnostic) diagnostic <- c(diagnostic, "khat")
  }

  # Get indices and rename
  diagnostic_df <- as.data.frame(posteriors$stan_summary)
  diagnostic_df$Parameter <- row.names(diagnostic_df)
  if ("n_eff" %in% names(diagnostic_df)) {
    diagnostic_df$ESS <- diagnostic_df$n_eff
  }
  # special handling for MCSE, due to some parameters (like lp__) missing in rows
  MCSE <- mcse(posteriors, effects = effects)
  diagnostic_df <- merge(diagnostic_df, MCSE, by = "Parameter", all = FALSE)

  # Select columns
  available_columns <- intersect(colnames(diagnostic_df), c("Parameter", diagnostic))
  diagnostic_df <- diagnostic_df[available_columns]
  names(diagnostic_df)[available_columns == "khat"] <- "Khat"
  row.names(diagnostic_df) <- NULL

  # Remove columns with all Nans
  diagnostic_df <- diagnostic_df[!sapply(diagnostic_df, function(x) all(is.na(x)))]

  diagnostic_df$Response <- gsub("(b\\[)*(.*)\\|(.*)", "\\2", diagnostic_df$Parameter)
  for (i in unique(diagnostic_df$Response)) {
    diagnostic_df$Parameter <- gsub(sprintf("%s|", i), "", diagnostic_df$Parameter, fixed = TRUE)
  }

  # Select rows
  diagnostic_df[diagnostic_df$Parameter %in% params, ]
}


#' @inheritParams insight::get_parameters
#' @rdname diagnostic_posterior
#' @export
diagnostic_posterior.brmsfit <- function(posteriors, diagnostic = "all", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {

  # Find parameters
  effects <- match.arg(effects)
  component <- match.arg(component)
  params <- insight::find_parameters(posteriors,
    effects = effects,
    component = component,
    parameters = parameters,
    flatten = TRUE
  )

  # If no diagnostic
  if (is.null(diagnostic)) {
    return(data.frame("Parameter" = params))
  }

  # Get diagnostic
  diagnostic <- match.arg(diagnostic, c("ESS", "Rhat", "MCSE", "all"), several.ok = TRUE)
  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE", "khat") # Add MCSE
  } else {
    if ("Rhat" %in% diagnostic) diagnostic <- c(diagnostic, "khat")
  }

  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' required for this function to work. Please install it by running `install.packages('rstan')`.")
  }

  # Get indices and rename
  diagnostic_df <- as.data.frame(rstan::summary(posteriors$fit)$summary)
  diagnostic_df$Parameter <- make.names(row.names(diagnostic_df))
  diagnostic_df$ESS <- diagnostic_df$n_eff
  # special handling for MCSE, due to some parameters (like lp__) missing in rows
  MCSE <- mcse(posteriors, effects = "all", component = "all")
  diagnostic_df <- merge(diagnostic_df, MCSE, by = "Parameter", all = FALSE)

  # Select columns
  available_columns <- intersect(colnames(diagnostic_df), c("Parameter", diagnostic))
  diagnostic_df <- diagnostic_df[available_columns]
  names(diagnostic_df)[available_columns == "khat"] <- "Khat"
  row.names(diagnostic_df) <- NULL

  # Remove columns with all Nans
  diagnostic_df <- diagnostic_df[!sapply(diagnostic_df, function(x) all(is.na(x)))]

  # Select rows
  diagnostic_df[diagnostic_df$Parameter %in% params, ]
}



#' @inheritParams insight::get_parameters
#' @export
diagnostic_posterior.stanfit <- function(posteriors, diagnostic = "all", effects = c("fixed", "random", "all"), parameters = NULL, ...) {

  # Find parameters
  effects <- match.arg(effects)
  params <- insight::find_parameters(posteriors, effects = effects, parameters = parameters, flatten = TRUE)

  # If no diagnostic
  if (is.null(diagnostic)) {
    return(data.frame("Parameter" = params))
  }

  # Get diagnostic
  diagnostic <- match.arg(diagnostic, c("ESS", "Rhat", "MCSE", "all"), several.ok = TRUE)
  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE")
  }

  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' required for this function to work. Please install it.")
  }

  all_params <- insight::find_parameters(posteriors, effects = effects, flatten = TRUE)

  diagnostic_df <- data.frame(
    Parameter = all_params,
    stringsAsFactors = FALSE
  )

  if ("ESS" %in% diagnostic) {
    diagnostic_df$ESS <- effective_sample(posteriors, effects = effects)$ESS
  }
  if ("MCSE" %in% diagnostic) {
    diagnostic_df$MCSE <- mcse(posteriors, effects = effects)$MCSE
  }
  if ("Rhat" %in% diagnostic) {
    s <- as.data.frame(rstan::summary(posteriors)$summary)
    diagnostic_df$Rhat <- s[rownames(s) %in% all_params, ]$Rhat
  }

  # Remove columns with all Nans
  diagnostic_df <- diagnostic_df[!sapply(diagnostic_df, function(x) all(is.na(x)))]

  # Select rows
  diagnostic_df[diagnostic_df$Parameter %in% params, ]
}
