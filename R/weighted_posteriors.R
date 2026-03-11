#' Generate posterior distributions weighted across models
#'
#' Extract posterior samples of parameters, weighted across models. Weighting is
#' done by comparing posterior model probabilities, via [bayesfactor_models()].
#'
#' @param ... Fitted models (see details), all fit on the same data, or a single
#'   `BFBayesFactor` object.
#' @param missing An optional numeric value to use if a model does not contain a
#'   parameter that appears in other models. Defaults to 0.
#' @param prior_odds Optional vector of prior odds for the models compared to
#'   the first model (or the denominator, for `BFBayesFactor` objects). For
#'   `data.frame`s, this will be used as the basis of weighting.
#' @param iterations For `BayesFactor` models, how many posterior samples to draw.
#' @inheritParams bayesfactor_models
#' @inheritParams bayesfactor_parameters
#'
#' @details
#' Note that across models some parameters might play different roles. For
#' example, the parameter `A` plays a different role in the model `Y ~ A + B`
#' (where it is a main effect) than it does in the model `Y ~ A + B + A:B`
#' (where it is a simple effect). In many cases centering of predictors (mean
#' subtracting for continuous variables, and effects coding via `contr.sum` or
#' orthonormal coding via [`contr.equalprior_pairs`] for factors) can reduce this
#' issue. In any case you should be mindful of this issue.
#'
#' See [bayesfactor_models()] details for more info on passed models.
#'
#' Note that for `BayesFactor` models, posterior samples cannot be generated
#' from intercept only models.
#'
#' This function is similar in function to `brms::posterior_average`.
#'
#' @note For `BayesFactor < 0.9.12-4.3`, in some instances there might be
#'   some problems of duplicate columns of random effects in the resulting data
#'   frame.
#'
#' @return A data frame with posterior distributions (weighted across models) .
#'
#' @seealso [`bayesfactor_inclusion()`] for Bayesian model averaging.
#'
#' @examples
#' \donttest{
#' if (require("rstanarm") && require("see") && interactive()) {
#'   stan_m0 <- suppressWarnings(stan_glm(extra ~ 1,
#'     data = sleep,
#'     family = gaussian(),
#'     refresh = 0,
#'     diagnostic_file = file.path(tempdir(), "df0.csv")
#'   ))
#'
#'   stan_m1 <- suppressWarnings(stan_glm(extra ~ group,
#'     data = sleep,
#'     family = gaussian(),
#'     refresh = 0,
#'     diagnostic_file = file.path(tempdir(), "df1.csv")
#'   ))
#'
#'   res <- weighted_posteriors(stan_m0, stan_m1, verbose = FALSE)
#'
#'   plot(eti(res))
#' }
#'
#' ## With BayesFactor
#' if (require("BayesFactor")) {
#'   extra_sleep <- ttestBF(formula = extra ~ group, data = sleep)
#'
#'   wp <- weighted_posteriors(extra_sleep, verbose = FALSE)
#'
#'   describe_posterior(extra_sleep, test = NULL, verbose = FALSE)
#'   # also considers the null
#'   describe_posterior(wp$delta, test = NULL, verbose = FALSE)
#' }
#'
#'
#' ## weighted prediction distributions via data.frames
#' if (require("rstanarm") && interactive()) {
#'   m0 <- suppressWarnings(stan_glm(
#'     mpg ~ 1,
#'     data = mtcars,
#'     family = gaussian(),
#'     diagnostic_file = file.path(tempdir(), "df0.csv"),
#'     refresh = 0
#'   ))
#'
#'   m1 <- suppressWarnings(stan_glm(
#'     mpg ~ carb,
#'     data = mtcars,
#'     family = gaussian(),
#'     diagnostic_file = file.path(tempdir(), "df1.csv"),
#'     refresh = 0
#'   ))
#'
#'   # Predictions:
#'   pred_m0 <- data.frame(posterior_predict(m0))
#'   pred_m1 <- data.frame(posterior_predict(m1))
#'
#'   BFmods <- bayesfactor_models(m0, m1, verbose = FALSE)
#'
#'   wp <- weighted_posteriors(
#'     pred_m0, pred_m1,
#'     prior_odds = as.numeric(BFmods)[2],
#'     verbose = FALSE
#'   )
#'
#'   # look at first 5 prediction intervals
#'   hdi(pred_m0[1:5])
#'   hdi(pred_m1[1:5])
#'   hdi(wp[1:5]) # between, but closer to pred_m1
#' }
#' }
#'
#' @references
#'
#' - Clyde, M., Desimone, H., & Parmigiani, G. (1996). Prediction via
#'   orthogonalized model mixing. Journal of the American Statistical
#'   Association, 91(435), 1197-1208.
#'
#' - Hinne, M., Gronau, Q. F., van den Bergh, D., and Wagenmakers, E.
#'   (2019, March 25). A conceptual introduction to Bayesian Model Averaging.
#'   \doi{10.31234/osf.io/wgb64}
#'
#' - Rouder, J. N., Haaf, J. M., & Vandekerckhove, J. (2018). Bayesian
#'   inference for psychology, part IV: Parameter estimation and Bayes factors.
#'   Psychonomic bulletin & review, 25(1), 102-113.
#'
#' - van den Bergh, D., Haaf, J. M., Ly, A., Rouder, J. N., & Wagenmakers,
#'   E. J. (2019). A cautionary note on estimating effect size.
#'
#' @export
weighted_posteriors <- function(..., prior_odds = NULL, missing = 0, verbose = TRUE) {
  UseMethod("weighted_posteriors")
}

#' @export
#' @rdname weighted_posteriors
weighted_posteriors.data.frame <- function(..., prior_odds = NULL, missing = 0, verbose = TRUE) {
  Mods <- list(...)
  mnames <- sapply(match.call(expand.dots = FALSE)$`...`, insight::safe_deparse)

  # find min nrow
  iterations <- min(vapply(Mods, nrow, numeric(1)))

  # make weights from prior_odds
  if (!is.null(prior_odds)) {
    prior_odds <- c(1, prior_odds)
  } else {
    if (verbose) {
      insight::format_warning(
        "'prior_odds = NULL'; Using uniform priors odds.\n",
        "For weighted data frame, 'prior_odds' should be specified as a numeric vector."
      )
    }
    prior_odds <- rep(1, length(Mods))
  }

  Probs <- prior_odds / sum(prior_odds)
  weighted_samps <- round(iterations * Probs)

  # pass to .weighted_posteriors
  .weighted_posteriors(Mods, weighted_samps, missing, mnames)
}


#' @export
#' @rdname weighted_posteriors
weighted_posteriors.stanreg <- function(...,
                                        prior_odds = NULL,
                                        missing = 0,
                                        verbose = TRUE,
                                        effects = "fixed",
                                        component = "conditional",
                                        parameters = NULL) {
  Mods <- list(...)
  mnames <- sapply(match.call(expand.dots = FALSE)$`...`, insight::safe_deparse)

  # Get Bayes factors
  BFMods <- bayesfactor_models(..., denominator = 1, verbose = verbose)

  # Compute posterior model probabilities
  model_tab <- .get_model_table(BFMods, priorOdds = prior_odds)
  postProbs <- model_tab$postProbs

  # Compute weighted number of samples
  iterations <- min(sapply(Mods, .total_samps))
  weighted_samps <- round(iterations * postProbs)

  # extract parameters
  params <- lapply(Mods, insight::get_parameters,
    effects = effects,
    component = component,
    parameters = parameters
  )

  .weighted_posteriors(params, weighted_samps, missing, mnames)
}

#' @export
weighted_posteriors.brmsfit <- weighted_posteriors.stanreg

#' @export
weighted_posteriors.blavaan <- weighted_posteriors.stanreg

#' @rdname weighted_posteriors
#' @export
weighted_posteriors.BFBayesFactor <- function(...,
                                              prior_odds = NULL,
                                              missing = 0,
                                              verbose = TRUE,
                                              iterations = 4000) {
  Mods <- c(...)

  # Get Bayes factors
  BFMods <- bayesfactor_models(Mods, verbose = verbose)

  # Compute posterior model probabilities
  model_tab <- .get_model_table(BFMods, priorOdds = prior_odds, add_effects_table = FALSE)
  postProbs <- model_tab$postProbs

  # Compute weighted number of samples
  weighted_samps <- round(iterations * postProbs)

  # extract parameters
  intercept_only <- which(BFMods$Model == "1")
  params <- vector(mode = "list", length = nrow(BFMods))
  for (m in seq_along(params)) {
    if (length(intercept_only) && m == intercept_only) {
      # warning(
      #   "Cannot sample from BFBayesFactor model with intercept only (model prob = ",
      #   round(postProbs[m], 3) * 100, "%).\n",
      #   "Omitting the intercept model.",
      #   call. = FALSE
      # )
      params[[m]] <- data.frame(
        mu = rep(NA, iterations),
        sig2 = rep(NA, iterations),
        g = rep(NA, iterations)
      )
    } else if (m == 1) {
      # If the model is the "den" model
      params[[m]] <- BayesFactor::posterior(1 / Mods[1], iterations = iterations, progress = FALSE)
    } else {
      params[[m]] <- BayesFactor::posterior(
        Mods[m - 1],
        iterations = iterations, progress = FALSE
      )
    }
  }

  params <- lapply(params, data.frame)

  .weighted_posteriors(params, weighted_samps, missing, BFMods$Model)
}

.weighted_posteriors <- function(params, weighted_samps, missing, mnames) {
  par_names <- unique(unlist(sapply(params, colnames), recursive = TRUE))

  # Table of weights
  weights <- data.frame(
    Model = mnames,
    weights = weighted_samps,
    pweights = weighted_samps / sum(weighted_samps)
  )

  # remove empty (0 sample) models
  params <- params[weighted_samps != 0]
  weighted_samps <- weighted_samps[weighted_samps != 0]

  for (m in seq_along(weighted_samps)) {
    temp_params <- params[[m]]
    i <- sample(nrow(temp_params), size = weighted_samps[m])
    temp_params <- temp_params[i, , drop = FALSE]

    # If any parameters not estimated in the model, they are assumed to be 0 (the default value of `missing`)
    missing_pars <- setdiff(par_names, colnames(temp_params))
    temp_params[, missing_pars] <- missing

    params[[m]] <- temp_params
  }

  # combine all
  res <- do.call("rbind", params)
  attr(res, "weights") <- weights
  return(res)
}

#' @keywords internal
.total_samps <- function(mod) {
  x <- insight::find_algorithm(mod)
  if (is.null(x$iterations)) x$iterations <- x$sample
  x$chains * (x$iterations - x$warmup)
}
