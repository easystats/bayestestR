#' Describe Posterior Distributions
#'
#' Compute indices relevant to describe and characterize the posterior distributions.
#'
#' @param posteriors A vector, data frame or model of posterior draws.
#' @param ci_method The type of index used for Credible Interval. Can be
#'   \code{"HDI"} (default, see \code{\link[bayestestR:hdi]{hdi}}), \code{"ETI"}
#'   (see \code{\link[bayestestR:eti]{eti}}) or \code{"SI"}
#'   (see \code{\link[bayestestR:si]{si}}).
#' @param test The indices of effect existence to compute. Character (vector) or
#'   list with one or more of these options: \code{"p_direction"} (or \code{"pd"}),
#'   \code{"rope"}, \code{"p_map"}, \code{"equivalence_test"} (or \code{"equitest"}),
#'   \code{"bayesfactor"} (or \code{"bf"}) or \code{"all"} to compute all tests.
#'   For each "test", the corresponding \pkg{bayestestR} function is called
#'   (e.g. \code{\link[bayestestR:rope]{rope}} or \code{\link[bayestestR:p_direction]{p_direction}}) and its results
#'   included in the summary output.
#' @param rope_range ROPE's lower and higher bounds. Should be a list of two
#'   values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"},
#'   the bounds are set to \code{x +- 0.1*SD(response)}.
#' @param rope_ci The Credible Interval (CI) probability, corresponding to the
#'   proportion of HDI, to use for the percentage in ROPE.
#'
#' @inheritParams point_estimate
#' @inheritParams ci
#' @inheritParams si
#'
#' @details One or more components of point estimates (like posterior mean or median),
#'   intervals and tests can be omitted from the summary output by setting the
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
#' \dontrun{
#' # rstanarm models
#' # -----------------------------------------------
#' if (require("rstanarm") && require("emmeans")) {
#'   model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#'   describe_posterior(model)
#'   describe_posterior(model, centrality = "all", dispersion = TRUE, test = "all")
#'   describe_posterior(model, ci = c(0.80, 0.90))
#'
#'   # emmeans estimates
#'   # -----------------------------------------------
#'   describe_posterior(emtrends(model, ~1, "wt"))
#' }
#'
#' # brms models
#' # -----------------------------------------------
#' if (require("brms")) {
#'   model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#'   describe_posterior(model)
#'   describe_posterior(model, centrality = "all", dispersion = TRUE, test = "all")
#'   describe_posterior(model, ci = c(0.80, 0.90))
#' }
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' if (require("BayesFactor")) {
#'   bf <- ttestBF(x = rnorm(100, 1, 1))
#'   describe_posterior(bf)
#'   describe_posterior(bf, centrality = "all", dispersion = TRUE, test = "all")
#'   describe_posterior(bf, ci = c(0.80, 0.90))
#' }
#' }
#' @importFrom stats mad median sd setNames
#'
#' @export
describe_posterior <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, ...) {
  UseMethod("describe_posterior")
}




#' @keywords internal
.describe_posterior <- function(x, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, BF = 1, ...) {

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
    ci_method <- match.arg(tolower(ci_method), c("hdi", "quantile", "ci", "eti", "si"))
    if (ci_method == "si") {
      uncertainty <- ci(x, BF = BF, method = ci_method, prior = bf_prior, ...)
    } else {
      uncertainty <- ci(x, ci = ci, method = ci_method, ...)
    }

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
      test <- c("pd", "p_map", "p_rope", "p_significance", "rope", "equivalence", "bf")
    }

    ## TODO no BF for arm::sim
    if (inherits(x, c("sim", "sim.merMod", "mcmc", "stanfit"))) {
      test <- setdiff(test, "bf")
    }

    ## TODO enable once "rope()" works for multi-response models

    # no ROPE for multi-response models
    if (insight::is_multivariate(x)) {
      test <- setdiff(test, c("rope", "p_rope"))
      warning("Multivariate response models are not yet supported for tests 'rope' and 'p_rope'.", call. = FALSE)
    }

    # MAP-based p-value

    if (any(c("p_map", "p_pointnull") %in% test)) {
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

    # Probability of rope

    if (any(c("p_rope") %in% test)) {
      test_prope <- p_rope(x, range = rope_range, ...)
      if (!"Parameter" %in% names(test_prope)) {
        test_prope <- cbind(data.frame("Parameter" = "Posterior"), test_prope)
      }
    } else {
      test_prope <- data.frame("Parameter" = NA)
    }

    # Probability of significance

    if (any(c("ps", "p_sig", "p_significance") %in% test)) {
      test_psig <- p_significance(x, threshold  = rope_range, ...)
      if (!is.data.frame(test_psig)) test_psig <- data.frame("Parameter" = "Posterior", "ps" = test_psig)
    } else {
      test_psig <- data.frame("Parameter" = NA)
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
      test_equi$Cleaned_Parameter <- NULL

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
    test_pd <- data.frame("Parameter" = NA, "Effects" = NA, "Component" = NA, "Response" = NA)
    test_prope <- data.frame("Parameter" = NA, "Effects" = NA, "Component" = NA, "Response" = NA)
    test_psig <- data.frame("Parameter" = NA, "Effects" = NA, "Component" = NA, "Response" = NA)
    test_rope <- data.frame("Parameter" = NA, "Effects" = NA, "Component" = NA, "Response" = NA)
    test_bf <- data.frame("Parameter" = NA, "Effects" = NA, "Component" = NA, "Response" = NA)
    test_pmap <- data.frame("Parameter" = NA, "Effects" = NA, "Component" = NA, "Response" = NA)
  }


  # for data frames or numeric, and even for some models, we don't
  # have the "Effects" or "Component" column for all data frames.
  # To make "merge()" work, we add those columns to all data frames,
  # filled with NA, and remove the columns later if necessary

  estimates <- .add_effects_component_column(estimates)
  uncertainty <- .add_effects_component_column(uncertainty)
  test_pmap <- .add_effects_component_column(test_pmap)
  test_pd <- .add_effects_component_column(test_pd)
  test_prope <- .add_effects_component_column(test_prope)
  test_psig <- .add_effects_component_column(test_psig)
  test_rope <- .add_effects_component_column(test_rope)
  test_bf <- .add_effects_component_column(test_bf)


  # at least one "valid" data frame needs a row id, to restore
  # row-order after merging

  if (!all(is.na(estimates$Parameter))) {
    estimates$.rowid <- 1:nrow(estimates)
  } else if (!all(is.na(test_pmap$Parameter))) {
    test_pmap$.rowid <- 1:nrow(test_pmap)
  } else if (!all(is.na(test_pd$Parameter))) {
    test_pd$.rowid <- 1:nrow(test_pd)
  } else if (!all(is.na(test_prope$Parameter))) {
    test_prope$.rowid <- 1:nrow(test_prope)
  } else if (!all(is.na(test_psig$Parameter))) {
    test_psig$.rowid <- 1:nrow(test_psig)
  } else if (!all(is.na(test_rope$Parameter))) {
    test_rope$.rowid <- 1:nrow(test_rope)
  } else if (!all(is.na(test_bf$Parameter))) {
    test_bf$.rowid <- 1:nrow(test_bf)
  } else {
    estimates$.rowid <- 1:nrow(estimates)
  }


  # remove duplicated columns
  if (all(c("rope", "p_rope") %in% test)) {
    test_prope$ROPE_low <- NULL
    test_prope$ROPE_high <- NULL
  }

  # merge all data frames
  merge_by <- c("Parameter", "Effects", "Component", "Response")
  # merge_by <- intersect(merge_by, colnames(estimates))

  out <- merge(estimates, uncertainty, by = merge_by, all = TRUE)
  out <- merge(out, test_pmap, by = merge_by, all = TRUE)
  out <- merge(out, test_pd, by = merge_by, all = TRUE)
  out <- merge(out, test_prope, by = merge_by, all = TRUE)
  out <- merge(out, test_psig, by = merge_by, all = TRUE)
  out <- merge(out, test_rope, by = merge_by, all = TRUE)
  out <- merge(out, test_bf, by = merge_by, all = TRUE)
  out <- out[!is.na(out$Parameter), ]


  # check which columns can be removed at the end. In any case, we don't
  # need .rowid in the returned data frame, and when the Effects or Component
  # column consist only of missing values, we remove those columns as well

  remove_columns <- ".rowid"
  if (all(is.na(out$Effects)) || length(unique(out$Effects)) < 2) remove_columns <- c(remove_columns, "Effects")
  if (all(is.na(out$Component)) || length(unique(out$Component)) < 2) remove_columns <- c(remove_columns, "Component")
  if (all(is.na(out$Response)) || length(unique(out$Response)) < 2) remove_columns <- c(remove_columns, "Response")

  attr(out, "ci_method") <- ci_method
  # Restore columns order
  .remove_column(out[order(out$.rowid), ], remove_columns)
}


#' @keywords internal
.add_effects_component_column <- function(x) {
  if (!"Effects" %in% names(x)) x <- cbind(x, data.frame("Effects" = NA))
  if (!"Component" %in% names(x)) x <- cbind(x, data.frame("Component" = NA))
  if (!"Response" %in% names(x)) x <- cbind(x, data.frame("Response" = NA))
  x
}




#' @rdname describe_posterior
#' @param bf_prior Distribution representing a prior for the computation of Bayes factors / SI. Used if the input is a posterior, otherwise (in the case of models) ignored.
#' @export
describe_posterior.numeric <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, BF = 1, ...) {
  out <- .describe_posterior(posteriors, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, BF = BF, ...)
  class(out) <- unique(c("describe_posterior", "see_describe_posterior", class(out)))
  out
}


#' @export
describe_posterior.double <- describe_posterior.numeric


#' @export
describe_posterior.data.frame <- describe_posterior.numeric

#' @export
describe_posterior.effectsize_std_params <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, BF = 1, ...){

  class(posteriors) <- "data.frame"

  no_unique <- sapply(posteriors, function(col) {
    length(unique(col)) == 1
  })

  if (any(no_unique)) {
    no_unique <- which(no_unique)

    out <- describe_posterior.data.frame(
      posteriors[,-no_unique],
      centrality = centrality,
      dispersion = dispersion,
      ci = ci,
      ci_method = ci_method,
      test = test,
      rope_range = rope_range,
      rope_ci = rope_ci,
      bf_prior = bf_prior,
      BF = BF,
      ...
    )

    out_int <- data.frame(Parameter = colnames(posteriors)[no_unique])
    col_diff <- setdiff(colnames(out), colnames(out_int))
    out_int[, col_diff] <- NA
    out <- rbind(out_int, out)

    out <- out[order(match(out$Parameter,colnames(posteriors))),]

    return(out)
  }

  describe_posterior.data.frame(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    bf_prior = bf_prior,
    BF = BF,
    ...
  )

}


#' @export
describe_posterior.sim.merMod <- describe_posterior.numeric


#' @export
describe_posterior.sim <- describe_posterior.numeric


#' @export
describe_posterior.emmGrid <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, BF = 1, ...) {
  if (any(c("all", "bf", "bayesfactor", "bayes_factor") %in% tolower(test)) ||
      "si" %in% tolower(ci_method)) {
    samps <- .clean_priors_and_posteriors(posteriors, bf_prior)
    bf_prior <- samps$prior
    posteriors <- samps$posterior
  } else {
    posteriors <- .clean_emmeans_draws(posteriors)
  }


  out <- .describe_posterior(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    bf_prior = bf_prior,
    BF = BF,
    ...
  )

  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  attr(out, "ci_method") <- ci_method

  out
}



#' @inheritParams insight::get_parameters
#' @inheritParams diagnostic_posterior
#' @importFrom insight find_algorithm
#' @param priors Add the prior used for each parameter.
#' @rdname describe_posterior
#' @export
describe_posterior.stanreg <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = FALSE, effects = c("fixed", "random", "all"), parameters = NULL, BF = 1, ...) {

  if ((any(c("all", "bf", "bayesfactor", "bayes_factor") %in% tolower(test)) | "si" %in% tolower(ci_method)) & is.null(bf_prior)) {
    bf_prior <- unupdate(posteriors)
  }

  out <- .describe_posterior(posteriors, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, BF = BF, effects = effects, parameters = parameters, ...)

  diagnostic <-
    diagnostic_posterior(
      posteriors,
      diagnostic,
      effects = effects,
      parameters = parameters,
      ...
    )
  out <- .merge_and_sort(out, diagnostic, by = "Parameter", all = TRUE)

  if (isTRUE(priors)) {
    priors_data <- describe_prior(posteriors, ...)
    out <- .merge_and_sort(out, priors_data, by = "Parameter", all = TRUE)
  }

  attr(out, "ci_method") <- ci_method
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}


#' @inheritParams insight::get_parameters
#' @inheritParams diagnostic_posterior
#' @importFrom insight find_algorithm
#' @param priors Add the prior used for each parameter.
#' @rdname describe_posterior
#' @export
describe_posterior.stanmvreg <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = "p_direction", rope_range = "default", rope_ci = 0.89, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = FALSE, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  out <- .describe_posterior(posteriors, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, effects = effects, parameters = parameters, ...)
  if (is.null(out$Response)) {
    out$Response <- gsub("(b\\[)*(.*)\\|(.*)", "\\2", out$Parameter)
  }

  diagnostic <-
    diagnostic_posterior(
      posteriors,
      diagnostic,
      effects = effects,
      parameters = parameters,
      ...
    )
  out <- .merge_and_sort(out, diagnostic, by = c("Parameter", "Response"), all = TRUE)

  if (isTRUE(priors)) {
    priors_data <- describe_prior(posteriors, ...)
    priors_data$Parameter <- gsub("^(.*)\\|(.*)", replacement = "\\2", priors_data$Parameter)
    out <- .merge_and_sort(out, priors_data, by = c("Parameter", "Response"), all = TRUE)
  }

  attr(out, "ci_method") <- ci_method
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}



#' @inheritParams insight::get_parameters
#' @inheritParams diagnostic_posterior
#' @export
describe_posterior.stanfit <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, diagnostic = c("ESS", "Rhat"), effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  out <- .describe_posterior(posteriors, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, effects = effects, parameters = parameters, ...)

  diagnostic <-
    diagnostic_posterior(
      posteriors,
      diagnostic,
      effects = effects,
      parameters = parameters,
      ...
    )
  out <- .merge_and_sort(out, diagnostic, by = "Parameter", all = TRUE)

  attr(out, "ci_method") <- ci_method
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}



#' @inheritParams describe_posterior.stanreg
#' @rdname describe_posterior
#' @export
describe_posterior.MCMCglmm <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, diagnostic = "ESS", parameters = NULL, ...) {
  out <- .describe_posterior(posteriors, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, effects = "fixed", parameters = parameters, ...)

  if (!is.null(diagnostic) && diagnostic == "ESS") {
    diagnostic <- effective_sample(posteriors, effects = "fixed", parameters = parameters, ...)
    out <- .merge_and_sort(out, diagnostic, by = "Parameter", all = TRUE)
  }

  out
}



#' @export
describe_posterior.mcmc <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, parameters = NULL, ...) {
  .describe_posterior(as.data.frame(posteriors), centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, effects = "fixed", parameters = parameters, ...)
}



#' @export
describe_posterior.bcplm <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, priors = TRUE, parameters = NULL, ...) {
  out <- .describe_posterior(insight::get_parameters(posteriors), centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, effects = "fixed", parameters = parameters, ...)
  if (isTRUE(priors)) {
    priors_data <- describe_prior(posteriors, ...)
    out <- .merge_and_sort(out, priors_data, by = "Parameter", all = TRUE)
  }

  attr(out, "ci_method") <- ci_method
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}

#' @export
describe_posterior.bayesQR <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, parameters = NULL, ...) {
  out <- .describe_posterior(insight::get_parameters(posteriors), centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, effects = "fixed", parameters = parameters, ...)
  attr(out, "ci_method") <- ci_method
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}



#' @inheritParams describe_posterior.stanreg
#' @rdname describe_posterior
#' @export
describe_posterior.brmsfit <- function(posteriors, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("p_direction", "rope"), rope_range = "default", rope_ci = 0.89, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, BF = 1, ...) {

  if ((any(c("all", "bf", "bayesfactor", "bayes_factor") %in% tolower(test)) | "si" %in% tolower(ci_method)) & is.null(bf_prior)) {
    bf_prior <- unupdate(posteriors)
  }

  out <- .describe_posterior(posteriors, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, BF = BF, effects = effects, component = component, parameters = parameters, ...)

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

  attr(out, "ci_method") <- ci_method
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
      test <- c("pd", "p_map", "p_rope", "p_significance", "rope", "equivalence", "bf")
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
    tryCatch({
      out$BF <- as.data.frame(bayesfactor_models(posteriors, ...))[-1, ]$BF
    },
    error = function(e) { NULL }
    )
  }


  # Add priors
  if (priors) {
    priors_data <- describe_prior(posteriors, ...)
    out <- .merge_and_sort(out, priors_data, by = intersect(names(out), names(priors_data)), all = TRUE)
  }

  out
}



#' @keywords internal
.check_test_values <- function(test) {
  match.arg(tolower(test), c(
    "pd", "p_direction", "pdir", "mpe", "ps", "psig", "p_significance",
    "p_rope", "rope", "equivalence", "equivalence_test", "equitest",
    "bf", "bayesfactor", "bayes_factor", "p_map", "all"
  ), several.ok = TRUE)
}
