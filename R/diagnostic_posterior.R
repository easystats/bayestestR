#' Posteriors Sampling Diagnostic
#'
#' Extract diagnostic metrics (Effective Sample Size (`ESS`), `Rhat` and Monte
#' Carlo Standard Error `MCSE`).
#'
#' @param posterior A `stanreg`, `stanfit`, `brmsfit`, `CmdStanFit`, `stanli_fit`, or
#' `blavaan` object; a list of data frames or matrices representing MCMC chains
#' (rows as samples, columns as parameters); or a 3D array (dimensions:
#' samples, chains, parameters)
#' @param diagnostic Diagnostic metrics to compute.  Character (vector) or list
#' with one or more of these options: `"ESS"`, `"ESS_bulk"`, `"Rhat"`, `"MCSE"`
#' or `"all"`. `"ESS"` returns the **tail-ESS** (the minimum of the effective
#' sample sizes for the 5% and 95% quantiles), which is the most relevant
#' diagnostic for assessing the reliability of credible intervals and other
#' tail-based quantities. `"ESS_bulk"` additionally returns the **bulk-ESS**
#' (the effective sample size for the bulk of the posterior, useful for
#' assessing the reliability of central tendency estimates such as the mean or
#' median). `"all"` includes both tail and bulk `"ESS"`, `"Rhat"`, and `"MCSE"`.
#' @param centrality The point-estimate (centrality index) for which to compute
#' the MCSE. Can be `"median"` (default) or `"mean"`. To not break other
#' functions like `describe_posterior()` or `diagnostic_posterior()`, all other
#' values are silently converted to `"median"`.
#'
#' @inheritSection hdi Model components
#'
#' @details
#'   **Effective Sample (ESS)** should be as large as possible, although for
#'   most applications, an effective sample size greater than 1000 is sufficient
#'   for stable estimates (_Bürkner, 2017_). The ESS returned by
#'   `diagnostic_posterior()` is the **tail-ESS**: it corresponds to the
#'   minimum of the effective sample sizes for the 5% and 95% quantiles, and
#'   is a diagnostic for the sampling efficiency in the tails of the posterior
#'   distribution. It is more relevant than the bulk-ESS for assessing
#'   the reliability of credible intervals, probabilities of direction, and
#'   other tail-based quantities. Note that the tail-ESS may differ from the
#'   ESS reported by `brms` (`Bulk_ESS`) or other tools; use `"ESS_bulk"` to
#'   also retrieve the bulk-ESS.
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
#' @examplesIf require("rstan")
#' set.seed(101)
#' mkdata <- function(nrow = 1000, ncol = 2, parnm = LETTERS[1:ncol]) {
#'   x <- as.data.frame(replicate(ncol, rnorm(nrow)))
#'   names(x) <- parnm
#'   x
#' }
#' dd <- replicate(5, mkdata(), simplify = FALSE)
#' diagnostic_posterior(dd)
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
diagnostic_posterior.default <- function(
  posterior,
  diagnostic = "all",
  centrality = "median",
  ...
) {
  ## check input, coerce to array
  if (is.list(posterior)) {
    for (i in seq_along(posterior)) {
      p <- posterior[[i]]
      if (
        !((inherits(p, "data.frame") || inherits(p, "matrix")) &&
          (length(dim(p)) == 2) &&
          (ncol(p) == ncol(posterior[[1]])))
      ) {
        insight::format_error(
          "'posterior' must be a 3D array or a list of data frames with equal numbers of columns."
        )
      }
    }
  } else if (!(inherits(posterior, "array") && length(dim(posterior)) == 3)) {
    insight::format_error("Expecting a 3D array for 'posterior'.")
  }

  .diagnostic_posterior_draws(
    posterior,
    diagnostic = diagnostic,
    centrality = centrality
  )
}

#' @inheritParams insight::get_parameters.BFBayesFactor
#' @inheritParams insight::get_parameters
#'
#' @rdname diagnostic_posterior
#' @export
diagnostic_posterior.stanreg <- function(
  posterior,
  diagnostic = "all",
  effects = "fixed",
  component = "location",
  parameters = NULL,
  centrality = "median",
  ...
) {
  params <- insight::find_parameters(
    posterior,
    effects = effects,
    component = component,
    parameters = parameters,
    flatten = TRUE
  )

  .diagnostic_posterior_stanreg(
    posterior,
    params = params,
    diagnostic = diagnostic,
    centrality = centrality
  )
}


#' @inheritParams insight::get_parameters
#' @export
diagnostic_posterior.stanmvreg <- function(
  posterior,
  diagnostic = "all",
  effects = "fixed",
  parameters = NULL,
  centrality = "median",
  ...
) {
  all_params <- insight::find_parameters(
    posterior,
    effects = effects,
    parameters = parameters,
    flatten = FALSE
  )

  params <- unlist(
    lapply(names(all_params), function(i) {
      all_params[[i]]$sigma <- NULL
      unlist(all_params[[i]], use.names = FALSE)
    }),
    use.names = FALSE
  )

  .diagnostic_posterior_stanreg(
    posterior,
    params = params,
    diagnostic = diagnostic,
    centrality = centrality,
    add_response = TRUE
  )
}


.diagnostic_posterior_stanreg <- function(
  posterior,
  params,
  diagnostic,
  centrality,
  add_response = FALSE
) {
  insight::check_if_installed("posterior")

  out <- .diagnostic_posterior_draws(
    posterior,
    diagnostic = diagnostic,
    centrality = centrality,
    pars = params
  )

  if (is.null(diagnostic)) {
    return(out)
  }

  requested <- match.arg(
    diagnostic,
    c("ESS", "ESS_tail", "ESS_bulk", "Rhat", "MCSE", "all"),
    several.ok = TRUE
  )
  if (
    ("all" %in% requested || "Rhat" %in% requested) &&
      "khat" %in% names(posterior$stan_summary)
  ) {
    khat <- data.frame(
      Parameter = row.names(posterior$stan_summary),
      Khat = posterior$stan_summary$khat,
      stringsAsFactors = FALSE
    )
    out <- merge(out, khat, by = "Parameter", all.x = TRUE, sort = FALSE)
    row.names(out) <- NULL
  }

  if (add_response) {
    out$Response <- gsub("(b\\[)*(.*)\\|(.*)", "\\2", out$Parameter)
    for (response in unique(out$Response)) {
      out$Parameter <- gsub(
        sprintf("%s|", response),
        "",
        out$Parameter,
        fixed = TRUE
      )
    }
  }

  out
}


#' @inheritParams insight::get_parameters
#' @export
diagnostic_posterior.brmsfit <- function(
  posterior,
  diagnostic = "all",
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  centrality = "median",
  ...
) {
  params <- insight::find_parameters(
    posterior,
    effects = effects,
    component = component,
    parameters = parameters,
    flatten = TRUE
  )

  insight::check_if_installed("posterior")
  .diagnostic_posterior_draws(
    posterior,
    diagnostic = diagnostic,
    centrality = centrality,
    pars = params
  )
}


#' @inheritParams insight::get_parameters
#' @export
diagnostic_posterior.stanfit <- function(
  posterior,
  diagnostic = "all",
  effects = "fixed",
  parameters = NULL,
  centrality = "median",
  ...
) {
  insight::check_if_installed("posterior")

  pars <- insight::find_parameters(
    posterior,
    effects = effects,
    parameters = parameters,
    flatten = TRUE
  )

  .diagnostic_posterior_draws(
    posterior,
    diagnostic = diagnostic,
    centrality = centrality,
    pars = pars
  )
}


#' @export
diagnostic_posterior.CmdStanFit <- function(
  posterior,
  diagnostic = "all",
  parameters = NULL,
  centrality = "median",
  ...
) {
  insight::check_if_installed("posterior")
  insight::check_if_installed("cmdstanr")

  .diagnostic_posterior_draws(
    posterior$draws(),
    diagnostic = diagnostic,
    centrality = centrality,
    pars = insight::find_parameters(posterior, flatten = TRUE),
    parameters = parameters
  )
}


#' @export
diagnostic_posterior.stanli_fit <- function(
  posterior,
  diagnostic = "all",
  parameters = NULL,
  centrality = "median",
  ...
) {
  insight::check_if_installed("posterior")
  insight::check_if_installed("stanli")

  .diagnostic_posterior_draws(
    stanli::as_draws_array(posterior),
    diagnostic = diagnostic,
    centrality = centrality,
    parameters = parameters
  )
}


# Shared by methods that provide posterior draws
.diagnostic_posterior_draws <- function(
  draws,
  diagnostic = "all",
  centrality = "median",
  pars = NULL,
  parameters = NULL
) {
  insight::check_if_installed("posterior")
  draws <- posterior::as_draws_array(draws)
  if (is.null(diagnostic)) {
    out <- data.frame(Parameter = posterior::variables(draws))
    return(.select_diagnostic_rows(out, pars, parameters))
  }

  diagnostic <- match.arg(
    diagnostic,
    c("ESS", "ESS_tail", "ESS_bulk", "Rhat", "MCSE", "all"),
    several.ok = TRUE
  )
  if ("all" %in% diagnostic) {
    diagnostic <- c("ESS_tail", "ESS_bulk", "Rhat", "MCSE")
  }
  diagnostic[diagnostic == "ESS"] <- "ESS_tail"

  if (
    is.null(centrality) || length(centrality) > 1 || !centrality %in% c("median", "mean")
  ) {
    centrality <- "median"
  }
  mcse_fun <- switch(
    centrality,
    median = posterior::mcse_median,
    mean = posterior::mcse_mean
  )

  out <- as.data.frame(posterior::summarize_draws(
    draws,
    ESS_tail = posterior::ess_tail,
    ESS_bulk = posterior::ess_bulk,
    Rhat = posterior::rhat,
    MCSE = mcse_fun
  ))
  out <- datawizard::data_rename(out, "variable", "Parameter")
  out$ESS_tail <- round(out$ESS_tail)
  out$ESS_bulk <- round(out$ESS_bulk)

  out <- .select_diagnostic_rows(out, pars, parameters)
  out <- out[, c("Parameter", diagnostic), drop = FALSE]
  row.names(out) <- NULL
  out
}


.select_diagnostic_rows <- function(out, pars = NULL, parameters = NULL) {
  if (!is.null(pars)) {
    out <- out[out$Parameter %in% pars, , drop = FALSE]
  }
  if (!is.null(parameters)) {
    out <- out[grepl(parameters, out$Parameter, perl = TRUE), , drop = FALSE]
  }
  out
}


#' @export
diagnostic_posterior.blavaan <- function(
  posterior,
  diagnostic = "all",
  centrality = "median",
  parameters = NULL,
  ...
) {
  insight::check_if_installed("blavaan")

  params <- suppressWarnings(insight::find_parameters(posterior, flatten = TRUE))
  draws <- blavaan::blavInspect(posterior, what = "mcmc")

  .diagnostic_posterior_draws(
    draws,
    diagnostic = diagnostic,
    centrality = centrality,
    pars = params,
    parameters = parameters
  )
}
