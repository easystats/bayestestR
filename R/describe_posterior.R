#' Describe Posterior Distributions
#'
#' Compute indices relevant to describe and characterise the posterior distributions.
#'
#' @param posteriors A vector, dataframe or model of posterior draws.
#' @param ci_method The type of index used for Credible Interval. Can be
#'   \code{"HDI"} (default, see \code{\link{hdi}}) or \code{"ETI"}
#'   (see \code{\link{eti}}).
#' @param test The indices of effect existence to compute. Character (vector) or
#'   list with one or more of these options: \code{"p_direction"} (or \code{"pd"}),
#'   \code{"rope"}, \code{"p_map"}, \code{"equivalence_test"} (or \code{"equitest"}),
#'   \code{"bayesfactor"} (or \code{"bf"}) or \code{"all"} to compute all tests.
#'   For each "test", the corresponding \pkg{bayestestR} function is called
#'   (e.g. \code{\link{rope}} or \code{\link{p_direction}}) and its results
#'   included in the summary output.
#' @param rope_range ROPE's lower and higher bounds. Should be a list of two
#'   values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"},
#'   the bounds are set to \code{x +- 0.1*SD(response)}.
#' @param rope_ci The Credible Interval (CI) probability, corresponding to the
#'   proportion of HDI, to use for the percentage in ROPE.
#'
#' @inheritParams point_estimate
#' @inheritParams ci
#'
#' @details One or more components of point estimates (like posterior mean or median),
#'   intervals and tests can be ommitted from the summary output by setting the
#'   related argument to \code{NULL}. For example, \code{test = NULL} and
#'   \code{centrality = NULL} would only return the HDI (or CI).
#'
#' @references \itemize{
#'   \item \href{https://easystats.github.io/bayestestR/articles/indicesEstimationComparison.html}{Comparison of Point-Estimates}
#'   \item \href{https://easystats.github.io/bayestestR/articles/region_of_practical_equivalence.html}{Region of Practical Equivalence (ROPE)}
#'   \item \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{Bayes factors}
#' }
#'
#' @examples
#' library(bayestestR)
#'
#' x <- rnorm(1000)
#' describe_posterior(x)
#' describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all")
#' describe_posterior(x, ci = c(0.80, 0.90))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' describe_posterior(df)
#' describe_posterior(df, centrality = "all", dispersion = TRUE, test = "all")
#' describe_posterior(df, ci = c(0.80, 0.90))
#'
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' describe_posterior(model)
#' describe_posterior(model, centrality = "all", dispersion = TRUE, test = "all")
#' describe_posterior(model, ci = c(0.80, 0.90))
#'
#' # emmeans estimates
#' # -----------------------------------------------
#' library(emmeans)
#' describe_posterior(emtrends(model, ~1, "wt"))
#' \dontrun{
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' describe_posterior(model)
#' describe_posterior(model, centrality = "all", dispersion = TRUE, test = "all")
#' describe_posterior(model, ci = c(0.80, 0.90))
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' describe_posterior(bf)
#' describe_posterior(bf, centrality = "all", dispersion = TRUE, test = "all")
#' describe_posterior(bf, ci = c(0.80, 0.90))
#' }
#'
#' @importFrom stats mad median sd setNames
#'
#' @export
describe_posterior <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, ...) {
  UseMethod("describe_posterior")
}




#' @keywords internal
.describe_posterior <- function(x, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, ...) {

  # Point-estimates

  if (!is.null(centrality)) {
    estimates <- point_estimate(x, centrality = centrality, dispersion = dispersion, ...)
    if (!"Parameter" %in% names(estimates)) {
      estimates <- cbind(data.frame("Parameter" = "Posterior"), estimates)
    }
  } else {
    estimates <- data.frame("Parameter" = NA)
  }


  # Uncertainty

  if (!is.null(ci)) {
    ci_method <- match.arg(tolower(ci_method), c("hdi", "quantile", "ci", "eti"))
    uncertainty <- ci(x, ci = ci, method = ci_method, ...)
    if (!"Parameter" %in% names(uncertainty)) {
      uncertainty <- cbind(data.frame("Parameter" = "Posterior"), uncertainty)
    }
  } else {
    uncertainty <- data.frame("Parameter" = NA)
  }


  # Effect Existence

  if (!is.null(test)) {
    test <- .check_test_values(test)
    if ("all" %in% test) {
      test <- c("p_map", "pd", "rope", "equivalence", "bf")
    }

    ## TODO no BF for arm::sim
    if (inherits(x, c("sim", "sim.merMod"))) test <- setdiff(test, "bf")

    # MAP-based p-value

    if ("p_map" %in% test) {
      test_pmap <- p_map(x, ...)
      if (!is.data.frame(test_pmap)) test_pmap <- data.frame("Parameter" = "Posterior", "p_map" = test_pmap)
    } else {
      test_pmap <- data.frame("Parameter" = NA)
    }


    # Probability of direction

    if (any(c("pd", "p_direction", "pdir", "mpe") %in% test)) {
      test_pd <- p_direction(x, ...)
      if (!is.data.frame(test_pd)) test_pd <- data.frame("Parameter" = "Posterior", "pd" = test_pd)
    } else {
      test_pd <- data.frame("Parameter" = NA)
    }


    # ROPE

    if (any(c("rope") %in% test)) {
      test_rope <- rope(x, range = rope_range, ci = rope_ci, ...)

      if (!"Parameter" %in% names(test_rope)) {
        test_rope <- cbind(data.frame("Parameter" = "Posterior"), test_rope)
      }
      names(test_rope)[names(test_rope) == "CI"] <- "ROPE_CI"
    } else {
      test_rope <- data.frame("Parameter" = NA)
    }


    # Equivalence test

    if (any(c("equivalence", "equivalence_test", "equitest") %in% test)) {
      if (any(c("rope") %in% test)) {
        equi_warnings <- FALSE
      } else {
        equi_warnings <- TRUE
      }

      test_equi <- equivalence_test(x, range = rope_range, ci = rope_ci, verbose = equi_warnings, ...)

      if (!"Parameter" %in% names(test_equi)) {
        test_equi <- cbind(data.frame("Parameter" = "Posterior"), test_equi)
      }
      names(test_equi)[names(test_equi) == "CI"] <- "ROPE_CI"

      test_rope <- merge(test_rope, test_equi, all = TRUE)
      test_rope <- test_rope[!names(test_rope) %in% c("HDI_low", "HDI_high")]
    }


    # Bayes Factors

    if (any(c("bf", "bayesfactor", "bayes_factor") %in% test)) {
      test_bf <- bayesfactor_parameters(x, prior = bf_prior, ...)
      if (!"Parameter" %in% names(test_bf)) {
        test_bf <- cbind(data.frame("Parameter" = "Posterior"), test_bf)
      }
    } else {
      test_bf <- data.frame("Parameter" = NA)
    }
  } else {
    test_pd <- data.frame("Parameter" = NA, "Effects" = NA, "Component" = NA)
    test_rope <- data.frame("Parameter" = NA, "Effects" = NA, "Component" = NA)
    test_bf <- data.frame("Parameter" = NA, "Effects" = NA, "Component" = NA)
    test_pmap <- data.frame("Parameter" = NA, "Effects" = NA, "Component" = NA)
  }


  # for data frames or numeric, and even for some models, we don't
  # have the "Effects" or "Component" column for all data frames.
  # To make "merge()" work, we add those columns to all data frames,
  # filled with NA, and remove the columns later if necessary

  estimates <- .add_effects_component_column(estimates)
  uncertainty <- .add_effects_component_column(uncertainty)
  test_pmap <- .add_effects_component_column(test_pmap)
  test_pd <- .add_effects_component_column(test_pd)
  test_rope <- .add_effects_component_column(test_rope)
  test_bf <- .add_effects_component_column(test_bf)

  merge_by <- c("Parameter", "Effects", "Component")


  # at least one "valid" data frame needs a row id, to restore
  # row-order after merging

  if (!all(is.na(estimates$Parameter))) {
    estimates$.rowid <- 1:nrow(estimates)
  } else if (!all(is.na(test_pmap$Parameter))) {
    test_pmap$.rowid <- 1:nrow(test_pmap)
  } else if (!all(is.na(test_pd$Parameter))) {
    test_pd$.rowid <- 1:nrow(test_pd)
  } else if (!all(is.na(test_rope$Parameter))) {
    test_rope$.rowid <- 1:nrow(test_rope)
  } else if (!all(is.na(test_bf$Parameter))) {
    test_bf$.rowid <- 1:nrow(test_bf)
  } else {
    estimates$.rowid <- 1:nrow(estimates)
  }


  # merge all data frames

  out <- merge(estimates, uncertainty, by = merge_by, all = TRUE)
  out <- merge(out, test_pmap, by = merge_by, all = TRUE)
  out <- merge(out, test_pd, by = merge_by, all = TRUE)
  out <- merge(out, test_rope, by = merge_by, all = TRUE)
  out <- merge(out, test_bf, by = merge_by, all = TRUE)
  out <- out[!is.na(out$Parameter), ]


  # check which columns can be removed at the end. In any case, we don't
  # need .rowid in the returned data frame, and when the Effects or Component
  # column consist only of missing values, we remove those columns as well

  remove_columns <- ".rowid"
  if (all(is.na(out$Effects)) || length(unique(out$Effects)) < 2) remove_columns <- c(remove_columns, "Effects")
  if (all(is.na(out$Component)) || length(unique(out$Component)) < 2) remove_columns <- c(remove_columns, "Component")

  # Restore columns order
  .remove_column(out[order(out$.rowid), ], remove_columns)
}


.add_effects_component_column <- function(x) {
  if (!"Effects" %in% names(x)) x <- cbind(x, data.frame("Effects" = NA))
  if (!"Component" %in% names(x)) x <- cbind(x, data.frame("Component" = NA))
  x
}




#' @rdname describe_posterior
#' @param bf_prior Distribution representing a prior for the computation of Bayes factors. Used if the input is a posterior, otherwise (in the case of models) ignored.
#' @export
describe_posterior.numeric <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, ...) {
  .describe_posterior(posteriors, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, ...)
}


#' @export
describe_posterior.double <- describe_posterior.numeric


#' @export
describe_posterior.data.frame <- describe_posterior.numeric


#' @export
describe_posterior.sim.merMod <- describe_posterior.numeric


#' @export
describe_posterior.sim <- describe_posterior.numeric


#' @export
describe_posterior.emmGrid <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package 'emmeans' required for this function to work. Please install it by running `install.packages('emmeans')`.")
  }

  if (any(c("all", "bf", "bayesfactor", "bayes_factor") %in% tolower(test))) {
    if (is.null(bf_prior)) {
      bf_prior <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(posteriors, names = FALSE)))
      warning(
        "Prior not specified! ",
        "Please provide the original model to get meaningful results."
      )
    } else {
      bf_prior <- .update_to_priors(bf_prior)
      bf_prior <- insight::get_parameters(bf_prior, effects = "fixed")
      bf_prior <- update(posteriors, post.beta = as.matrix(bf_prior))
      bf_prior <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(bf_prior, names = FALSE)))
    }
  }

  posteriors <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(posteriors, names = FALSE)))

  .describe_posterior(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    bf_prior = bf_prior,
    ...
  )
}



#' @inheritParams insight::get_parameters
#' @inheritParams diagnostic_posterior
#' @param priors Add the prior used for each parameter.
#' @rdname describe_posterior
#' @export
describe_posterior.stanreg <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = FALSE, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  out <- .describe_posterior(posteriors, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, effects = effects, parameters = parameters, ...)

  if (!is.null(diagnostic)) {
    diagnostic <-
      diagnostic_posterior(
        posteriors,
        diagnostic,
        effects = effects,
        parameters = parameters,
        ...
      )
    out <- .merge_and_sort(out, diagnostic, by = "Parameter", all = TRUE)
  }

  if (isTRUE(priors)) {
    priors_data <- describe_prior(posteriors, ...)
    out <- .merge_and_sort(out, priors_data, by = "Parameter", all = TRUE)
  }

  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}


#' @inheritParams describe_posterior.stanreg
#' @rdname describe_posterior
#' @export
describe_posterior.MCMCglmm <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, diagnostic = "ESS", parameters = NULL, ...) {
  out <- .describe_posterior(posteriors, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = NULL, effects = "fixed", parameters = parameters, ...)

  if (!is.null(diagnostic) && diagnostic == "ESS") {
    diagnostic <- effective_sample(posteriors, effects = "fixed", parameters = parameters, ...)
    out <- .merge_and_sort(out, diagnostic, by = "Parameter", all = TRUE)
  }

  out
}



#' @inheritParams describe_posterior.stanreg
#' @rdname describe_posterior
#' @export
describe_posterior.brmsfit <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  out <- .describe_posterior(posteriors, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, effects = effects, component = component, parameters = parameters, ...)

  if (!is.null(diagnostic)) {
    diagnostic <-
      diagnostic_posterior(
        posteriors,
        diagnostic,
        effects = effects,
        component = component,
        parameters = parameters,
        ...
      )
    out <- .merge_and_sort(out, diagnostic, by = "Parameter", all = TRUE)
  }

  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}



#' @rdname describe_posterior
#' @export
describe_posterior.BFBayesFactor <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope", "bf"), rope_range = "default", rope_ci = 0.89, priors = TRUE, ...) {

  # Match test args  to catch BFs
  if (!is.null(test)) {
    test <- .check_test_values(test)
    if ("all" %in% test) {
      test <- c("pd", "rope", "equivalence", "bf")
    }
  }

  # Remove BF from list
  if (any(c("bf", "bayesfactor", "bayes_factor") %in% test)) {
    test <- test[!test %in% c("bf", "bayesfactor", "bayes_factor")]
    compute_bf <- TRUE
  } else {
    compute_bf <- FALSE
  }

  # Describe posterior
  out <-
    .describe_posterior(
      posteriors,
      centrality = centrality,
      dispersion = dispersion,
      ci = ci,
      ci_method = ci_method,
      test = test,
      rope_range = rope_range,
      rope_ci = rope_ci,
      ...
    )


  # Compute and readd BF a posteriori
  if (compute_bf) {
    out$BF <- as.data.frame(bayesfactor_models(posteriors, ...))[-1, ]$BF
  }


  # Add priors
  if (priors) {
    priors_data <- describe_prior(posteriors, ...)
    out <- .merge_and_sort(out, priors_data, by = intersect(names(out), names(priors_data)), all = TRUE)
  }

  out
}




.check_test_values <- function(test) {
  match.arg(tolower(test), c(
    "pd", "p_direction", "pdir", "mpe",
    "rope", "equivalence", "equivalence_test", "equitest",
    "bf", "bayesfactor", "bayes_factor", "p_map", "all"
  ), several.ok = TRUE)
}
