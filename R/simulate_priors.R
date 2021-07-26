#' Returns Priors of a Model as Empirical Distributions
#'
#' Transforms priors information to actual distributions.
#'
#' @inheritParams effective_sample
#' @param n Size of the simulated prior distributions.
#'
#' @seealso [unupdate()] for directly sampling from the prior
#'   distribution (useful for complex priors and designs).
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ wt + am, data = mtcars, chains = 1, refresh = 0)
#'   simulate_prior(model)
#' }
#' }
#' @export
simulate_prior <- function(model, n = 1000, ...) {
  UseMethod("simulate_prior")
}



#' @export
simulate_prior.stanreg <- function(model,
                                   n = 1000,
                                   effects = c("fixed", "random", "all"),
                                   component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"),
                                   parameters = NULL,
                                   verbose = TRUE,
                                   ...) {

  # check arguments
  effects <- match.arg(effects)
  component <- match.arg(component)

  priors <- insight::get_priors(
    model,
    effects = effects,
    component = component,
    parameters = parameters,
    verbose = verbose
  )

  .simulate_prior(priors, n = n, verbose = verbose)
}

#' @export
simulate_prior.blavaan <- simulate_prior.stanreg

#' @export
simulate_prior.brmsfit <- function(model,
                                   n = 1000,
                                   effects = c("fixed", "random", "all"),
                                   component = c("conditional", "zi", "zero_inflated", "all"),
                                   parameters = NULL,
                                   verbose = TRUE,
                                   ...) {

  # check arguments
  effects <- match.arg(effects)
  component <- match.arg(component)

  priors <- insight::get_priors(
    model,
    effects = effects,
    component = component,
    parameters = parameters,
    verbose = verbose,
  )

  .simulate_prior(priors, n = n, verbose = verbose)
}



#' @export
simulate_prior.bcplm <- function(model, n = 1000, verbose = TRUE, ...) {
  .simulate_prior(insight::get_priors(model, verbose = verbose), n = n, verbose = verbose)
}






#' @keywords internal
.simulate_prior <- function(priors, n = 1000, verbose = TRUE) {
  simulated <- data.frame(.bamboozled = 1:n)
  sim_error_msg <- FALSE

  # iterate over parameters
  for (param in priors$Parameter) {
    prior <- priors[priors$Parameter == param, ]

    # edge cases
    if (nrow(prior) > 1) {
      prior <- prior[1, ]
    }

    # Get actual scale
    if ("Adjusted_Scale" %in% names(prior)) {
      scale <- prior$Adjusted_Scale
      # is autoscale = FALSE, scale contains NA values - replace
      # with non-adjusted then.
      if (anyNA(scale)) scale[is.na(scale)] <- prior$Scale[is.na(scale)]
    } else {
      scale <- prior$Scale
    }

    # Simulate prior
    prior <- tryCatch(
      {
        if (prior$Distribution %in% c("t", "student_t", "Student's t")) {
          distribution(prior$Distribution, n, prior$df, prior$Location)
        } else {
          distribution(prior$Distribution, n, prior$Location, scale)
        }
      },
      error = function(e) {
        sim_error_msg <- TRUE
        NA
      }
    )

    simulated[param] <- prior
  }

  if (sim_error_msg && verbose) {
    warning(paste0("Can't simulate priors from a ", prior$Distribution, " distribution."), call. = FALSE)
  }

  simulated$.bamboozled <- NULL
  simulated
}
