#' Describe Posterior Distributions
#'
#' Compute indices relevant to describe and characterise the posterior distributions.
#'
#' @param posteriors A vector, dataframe or model of posterior draws.
#' @param ci_method The type of index used for Credible Interval. Can be \link{hdi} (default) or "quantile" (see \link{ci}).
#' @param estimate The \href{https://easystats.github.io/bayestestR/articles/indicesEstimationComparison.html}{point-estimate(s)} to compute. Can be a character or a list with "median", "mean" or "MAP".
#' @param test The \href{https://easystats.github.io/bayestestR/articles/indicesEstimationComparison.html}{indices of effect existence} to compute. Can be a character or a list with "p_direction", "rope", "p_map" or "bayesfactor".
#' @param rope_range \href{https://easystats.github.io/bayestestR/rope}{ROPE's} lower and higher bounds. Should be a list of two values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the bounds are set to \code{x +- 0.1*SD(response)}.
#' @param rope_full If TRUE, use the proportion of the entire posterior distribution for the equivalence test. Otherwise, use the proportion of HDI as indicated by the \code{ci} argument.
#' @param bf_prior Distribution representing a prior for the computation of \href{https://easystats.github.io/bayestestR/bayesfactor_savagedickey}{Bayes factors}. Used if the input is a posterior, otherwise (in the case of models) ignored.
#'
#' @inheritParams point_estimate
#' @inheritParams ci
#'
#'
#' @examples
#' x <- rnorm(1000)
#' describe_posterior(x)
#' describe_posterior(x, estimate="all", dispersion = TRUE, test="all")
#' describe_posterior(x, ci=c(0.80, 0.90))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' describe_posterior(df)
#' describe_posterior(df, estimate="all", dispersion = TRUE, test="all")
#' describe_posterior(df, ci=c(0.80, 0.90))
#'
#' \dontrun{
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' describe_posterior(model)
#' describe_posterior(model, estimate="all", dispersion = TRUE, test="all")
#' describe_posterior(model, ci=c(0.80, 0.90))
#'
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' describe_posterior(model)
#' describe_posterior(model, estimate="all", dispersion = TRUE, test="all")
#' describe_posterior(model, ci=c(0.80, 0.90))
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' describe_posterior(bf)
#' describe_posterior(bf, estimate="all", dispersion = TRUE, test="all")
#' describe_posterior(bbf, ci=c(0.80, 0.90))
#' }
#'
#' @importFrom stats mad median sd setNames
#'
#' @export
describe_posterior <- function(posteriors, estimate = "median", dispersion = TRUE, ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, bf_prior = NULL, ...) {
  UseMethod("describe_posterior")
}




#' @keywords internal
.describe_posterior <- function(x, estimate = "median", dispersion = FALSE, ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, bf_prior = NULL, ...) {

  # Point-estimates
  if (!is.null(estimate)) {
    estimates <- point_estimate(x, estimate = estimate, dispersion = dispersion, ...)
    if(!"Parameter" %in% names(estimates)){
      estimates <- cbind(data.frame("Parameter" = "Posterior"), estimates)
    }
  } else{
    estimates <- data.frame("Parameter" = NA)
  }

  # Uncertainty
  if (!is.null(estimate)) {
    ci_method <- match.arg(ci_method, c("hdi", "quantile", "ci", "eti"))
    if(ci_method == "hdi"){
      uncertainty <- hdi(x, ci=ci)
    } else{
      uncertainty <- ci(x, ci=ci)
    }
    if(!"Parameter" %in% names(uncertainty)){
      uncertainty <- cbind(data.frame("Parameter" = "Posterior"), uncertainty)
    }
  } else{
    uncertainty <- data.frame("Parameter" = NA)
  }

  # Effect Existence
  test <- match.arg(test, c("pd", "p_direction", "pdir", "mpe",
                            "rope", "equivalence", "equivalence_test", "equitest",
                            "bf", "bayesfactor", "bayes_factor", "all"), several.ok = TRUE)
  if("all" %in% test){
    test <- c("pd", "rope", "equivalence", "bf")
  }

  # Probability of direction
  if(any(c("pd", "p_direction", "pdir", "mpe") %in% test)){
    test_pd <- p_direction(x, ...)
    if(!is.data.frame(test_pd)) test_pd <- data.frame("Parameter" = "Posterior", "pd" = test_pd)

  } else{
    test_pd <- data.frame("Parameter" = NA)
  }

  # ROPE
  if(any(c("rope") %in% test)){
    if(rope_full){
      test_rope <- rope(x, range = rope_range, ci = 1, ...)
    } else{
      test_rope <- rope(x, range = rope_range, ci = ci, ...)
    }
    if(!"Parameter" %in% names(test_rope)){
      test_rope <- cbind(data.frame("Parameter" = "Posterior"), test_rope)
    }
    names(test_rope)[names(test_rope) == "CI"] <- "ROPE_CI"
  } else{
    test_rope <- data.frame("Parameter" = NA)
  }

  # Equivalence test
  if(any(c("equivalence", "equivalence_test", "equitest") %in% test)){

    if(any(c("rope") %in% test)){
      equi_warnings = FALSE
    } else{
      equi_warnings = TRUE
    }

    if(rope_full){
      test_equi <- equivalence_test(x, range = rope_range, ci = 1, verbose = equi_warnings, ...)
    } else{
      test_equi <- equivalence_test(x, range = rope_range, ci = ci, verbose = equi_warnings, ...)
    }
    if(!"Parameter" %in% names(test_equi)){
      test_equi <- cbind(data.frame("Parameter" = "Posterior"), test_equi)
    }
    names(test_equi)[names(test_equi) == "CI"] <- "ROPE_CI"
  } else{
    test_equi <- data.frame("Parameter" = NA)
  }
  test_rope <- merge(test_rope, test_equi, all = TRUE)
  test_rope <- test_rope[!names(test_rope) %in% c("HDI_low", "HDI_high")]


  # Bayes Factors
  if(any(c("bf", "bayesfactor", "bayes_factor") %in% test)){
    test_bf <- bayesfactor_savagedickey(x, prior = bf_prior, ...)
    if(!"Parameter" %in% names(test_bf)){
      test_bf <- cbind(data.frame("Parameter" = "Posterior"), test_bf)
    }

  } else{
    test_bf <- data.frame("Parameter" = NA)
  }

  out <- merge(estimates, uncertainty, all=TRUE)
  out <- merge(out, test_pd, all=TRUE)
  out <- merge(out, test_rope, all=TRUE)
  out <- merge(out, test_bf, all=TRUE)
  out <- out[!is.na(out$Parameter), ]
  out
}













#' @export
describe_posterior.numeric <- function(posteriors, estimate = "median", dispersion = TRUE, ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, bf_prior = NULL, ...) {
  .describe_posterior(posteriors, estimate = estimate, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_full = rope_full, bf_prior = bf_prior, ...)
}

#' @export
describe_posterior.double <- describe_posterior.numeric

#' @export
describe_posterior.data.frame <- describe_posterior.numeric




#' @inheritParams insight::get_parameters
#' @rdname describe_posterior
#' @export
describe_posterior.stanreg <- function(posteriors, estimate = "median", dispersion = FALSE, ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, bf_prior = NULL, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  .describe_posterior(posteriors, estimate = estimate, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_full = rope_full, bf_prior = bf_prior, effects = effects, parameters = parameters, ...)
}

#' @inheritParams insight::get_parameters
#' @rdname describe_posterior
#' @export
describe_posterior.brmsfit <- function(posteriors, estimate = "median", dispersion = FALSE, ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, bf_prior = NULL, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  .describe_posterior(posteriors, estimate = estimate, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_full = rope_full, bf_prior = bf_prior, effects = effects, component = component, parameters = parameters, ...)
}




