#' Posteriors Sampling Diagnostic
#'
#' Extract diagnostic metrics (Effective Sample Size (`ESS`), `Rhat` and Monte
#' Carlo Standard Error `MCSE`).
#'
#' @param posterior A `stanreg`, `stanfit`, `brmsfit`, or `blavaan` object.
#' @param diagnostic Diagnostic metrics to compute.  Character (vector) or list
#'   with one or more of these options: `"ESS"`, `"Rhat"`, `"MCSE"` or `"all"`.
#'
#' @inheritSection hdi Model components
#'
#' @details
#'   **Effective Sample (ESS)** should be as large as possible, although for
#'   most applications, an effective sample size greater than 1000 is sufficient
#'   for stable estimates (_Bürkner, 2017_). The ESS corresponds to the number of
#'   independent samples with the same estimation power as the N autocorrelated
#'   samples. It is is a measure of "how much independent information there is
#'   in autocorrelated chains" (_Kruschke 2015, p182-3_).
#'
#'   **Rhat** should be the closest to 1. It should not be larger than 1.1
#'   (_Gelman and Rubin, 1992_) or 1.01 (_Vehtari et al., 2019_). The split
#'   Rhat statistic quantifies the consistency of an ensemble of Markov chains.
#'
#'   **Monte Carlo Standard Error (MCSE)** is another measure of accuracy of the
#'   chains. It is defined as standard deviation of the chains divided by their
#'   effective sample size (the formula for `mcse()` is from Kruschke 2015, p.
#'   187). The MCSE "provides a quantitative suggestion of how big the estimation
#'   noise is".
#'
#'
#' @examplesIf require("rstanarm") && require("brms")
#' \donttest{
#' # rstanarm models
#' # -----------------------------------------------
#' model <- suppressWarnings(
#'   rstanarm::stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' )
#' diagnostic_posterior(model)
#'
#' # brms models
#' # -----------------------------------------------
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' diagnostic_posterior(model)
#' }
#' @references
#' - Gelman, A., & Rubin, D. B. (1992). Inference from iterative simulation
#'   using multiple sequences. Statistical science, 7(4), 457-472.
#' - Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., and Bürkner, P. C.
#'   (2019). Rank-normalization, folding, and localization: An improved Rhat
#'   for assessing convergence of MCMC. arXiv preprint arXiv:1903.08008.
#' - Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R,
#'   JAGS, and Stan. Academic Press.
#' @export
diagnostic_posterior <- function(posterior, ...) {
  UseMethod("diagnostic_posterior")
}


#' @rdname diagnostic_posterior
#' @export
diagnostic_posterior.default <- function(posterior, diagnostic = c("ESS", "Rhat"), ...) {
  insight::format_error("'diagnostic_posterior()' only works with rstanarm, brms or blavaan models.")
}

#' @inheritParams insight::get_parameters.BFBayesFactor
#' @inheritParams insight::get_parameters
#' @rdname diagnostic_posterior
#' @export
diagnostic_posterior.stanreg <- function(posterior,
                                         diagnostic = "all",
                                         effects = "fixed",
                                         component = "location",
                                         parameters = NULL,
                                         ...) {
  # Find parameters
  params <- insight::find_parameters(
    posterior,
    effects = effects,
    component = component,
    parameters = parameters,
    flatten = TRUE
  )

  # If no diagnostic
  if (is.null(diagnostic)) {
    return(data.frame(Parameter = params))
  }

  diagnostic <- match.arg(
    diagnostic,
    c("ESS", "Rhat", "MCSE", "all"),
    several.ok = TRUE
  )

  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE", "khat")
  } else {
    diagnostic <- diagnostic
    if ("Rhat" %in% diagnostic) diagnostic <- c(diagnostic, "khat")
  }

  # Get indices and rename
  diagnostic_df <- as.data.frame(posterior$stan_summary)
  diagnostic_df$Parameter <- row.names(diagnostic_df)
  if ("n_eff" %in% names(diagnostic_df)) {
    diagnostic_df$ESS <- diagnostic_df$n_eff
  }
  # special handling for MCSE, due to some parameters (like lp__) missing in rows
  MCSE <- mcse(posterior, effects = "all")
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
diagnostic_posterior.stanmvreg <- function(posterior,
                                           diagnostic = "all",
                                           effects = "fixed",
                                           parameters = NULL,
                                           ...) {
  # Find parameters
  all_params <- insight::find_parameters(
    posterior,
    effects = effects,
    parameters = parameters,
    flatten = FALSE
  )

  params <- unlist(lapply(names(all_params), function(i) {
    all_params[[i]]$sigma <- NULL
    unlist(all_params[[i]], use.names = FALSE)
  }), use.names = FALSE)

  # If no diagnostic
  if (is.null(diagnostic)) {
    return(data.frame(Parameter = params))
  }

  diagnostic <- match.arg(
    diagnostic,
    c("ESS", "Rhat", "MCSE", "all"),
    several.ok = TRUE
  )

  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE", "khat")
  } else {
    diagnostic <- diagnostic
    if ("Rhat" %in% diagnostic) diagnostic <- c(diagnostic, "khat")
  }

  # Get indices and rename
  diagnostic_df <- as.data.frame(posterior$stan_summary)
  diagnostic_df$Parameter <- row.names(diagnostic_df)
  if ("n_eff" %in% names(diagnostic_df)) {
    diagnostic_df$ESS <- diagnostic_df$n_eff
  }
  # special handling for MCSE, due to some parameters (like lp__) missing in rows
  MCSE <- mcse(posterior, effects = effects)
  diagnostic_df <- merge(diagnostic_df, MCSE, by = "Parameter", all = FALSE)

  # Select columns
  available_columns <- intersect(colnames(diagnostic_df), c("Parameter", diagnostic))
  diagnostic_df <- diagnostic_df[available_columns]
  names(diagnostic_df)[available_columns == "khat"] <- "Khat"
  row.names(diagnostic_df) <- NULL

  # Remove columns with all Nans
  diagnostic_df <- diagnostic_df[!sapply(diagnostic_df, function(x) all(is.na(x)))]

  diagnostic_df$Response <- gsub("(b\\[)*(.*)\\|(.*)", "\\2", diagnostic_df$Parameter)

  # Select rows
  diagnostic_df <- diagnostic_df[diagnostic_df$Parameter %in% params, ]

  # clean parameters
  for (i in unique(diagnostic_df$Response)) {
    diagnostic_df$Parameter <- gsub(
      sprintf("%s|", i),
      "",
      diagnostic_df$Parameter,
      fixed = TRUE
    )
  }

  diagnostic_df
}


#' @inheritParams insight::get_parameters
#' @export
diagnostic_posterior.brmsfit <- function(posterior,
                                         diagnostic = "all",
                                         effects = "fixed",
                                         component = "conditional",
                                         parameters = NULL,
                                         ...) {
  # Find parameters
  params <- insight::find_parameters(posterior,
    effects = effects,
    component = component,
    parameters = parameters,
    flatten = TRUE
  )

  # If no diagnostic
  if (is.null(diagnostic)) {
    return(data.frame(Parameter = params))
  }

  # Get diagnostic
  diagnostic <- match.arg(
    diagnostic,
    c("ESS", "Rhat", "MCSE", "all"),
    several.ok = TRUE
  )

  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE", "khat") # Add MCSE
  } else if ("Rhat" %in% diagnostic) {
    diagnostic <- c(diagnostic, "khat")
  }

  insight::check_if_installed("rstan")

  # Get indices and rename
  diagnostic_df <- as.data.frame(rstan::summary(posterior$fit)$summary)
  diagnostic_df$Parameter <- row.names(diagnostic_df)
  diagnostic_df$ESS <- diagnostic_df$n_eff
  # special handling for MCSE, due to some parameters (like lp__) missing in rows
  MCSE <- mcse(posterior, effects = "all", component = "all")
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
diagnostic_posterior.stanfit <- function(posterior, diagnostic = "all", effects = "fixed", parameters = NULL, ...) {
  # Find parameters
  params <- insight::find_parameters(
    posterior,
    effects = effects,
    parameters = parameters,
    flatten = TRUE
  )

  # If no diagnostic
  if (is.null(diagnostic)) {
    return(data.frame(Parameter = params))
  }

  # Get diagnostic
  diagnostic <- match.arg(
    diagnostic,
    c("ESS", "Rhat", "MCSE", "all"),
    several.ok = TRUE
  )
  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE")
  }

  insight::check_if_installed("rstan")

  all_params <- insight::find_parameters(posterior,
    effects = effects,
    flatten = TRUE
  )

  diagnostic_df <- data.frame(
    Parameter = all_params,
    stringsAsFactors = FALSE
  )

  if ("ESS" %in% diagnostic) {
    diagnostic_df$ESS <- effective_sample(posterior, effects = effects)$ESS
  }

  if ("MCSE" %in% diagnostic) {
    diagnostic_df$MCSE <- mcse(posterior, effects = effects)$MCSE
  }

  if ("Rhat" %in% diagnostic) {
    s <- as.data.frame(rstan::summary(posterior)$summary)
    diagnostic_df$Rhat <- s[rownames(s) %in% all_params, ]$Rhat
  }

  # Remove columns with all Nans
  diagnostic_df <- diagnostic_df[!sapply(diagnostic_df, function(x) all(is.na(x)))]

  # Select rows
  diagnostic_df[diagnostic_df$Parameter %in% params, ]
}


#' @export
diagnostic_posterior.blavaan <- function(posterior, diagnostic = "all", ...) {
  # Find parameters
  params <- suppressWarnings(insight::find_parameters(posterior, flatten = TRUE))

  out <- data.frame(Parameter = params)

  # If no diagnostic
  if (is.null(diagnostic)) {
    return(out)
  }

  diagnostic <- match.arg(
    diagnostic,
    c("ESS", "Rhat", "MCSE", "all"),
    several.ok = TRUE
  )
  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS", "Rhat", "MCSE")
  } else {
    diagnostic <- diagnostic
    if ("Rhat" %in% diagnostic) diagnostic <- c(diagnostic, "khat")
  }

  # Get indices
  if ("Rhat" %in% diagnostic) {
    insight::check_if_installed("blavaan")

    Rhat <- blavaan::blavInspect(posterior, what = "psrf")
    Rhat <- data.frame(
      Parameter = colnames(insight::get_parameters(posterior)),
      Rhat = Rhat
    )
    out <- merge(out, Rhat, by = "Parameter", all = TRUE)
  }

  if ("ESS" %in% diagnostic) {
    ESS <- effective_sample(posterior)
    out <- merge(out, ESS, by = "Parameter", all = TRUE)
  }


  if ("MCSE" %in% diagnostic) {
    MCSE <- mcse(posterior)
    out <- merge(out, MCSE, by = "Parameter", all = TRUE)
  }

  unique(out)
}
