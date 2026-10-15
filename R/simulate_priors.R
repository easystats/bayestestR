#' Returns Priors of a Model as Empirical Distributions
#'
#' Transforms priors information to actual distributions.
#'
#' @inheritParams effective_sample
#' @param n Size of the simulated prior distributions.
#' @inheritParams hdi
#'
#' @seealso [`unupdate()`] for directly sampling from the prior
#'   distribution (useful for complex priors and designs).
#'
#' @examples
#' \donttest{
#' library(bayestestR)
#' if (require("rstanarm")) {
#'   model <- suppressWarnings(
#'     stan_glm(mpg ~ wt + am, data = mtcars, chains = 1, refresh = 0)
#'   )
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
                                   effects = "fixed",
                                   component = "location",
                                   parameters = NULL,
                                   verbose = TRUE,
                                   ...) {
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


#' @rdname simulate_prior
#' @export
simulate_prior.brmsfit <- function(model,
                                   n = 1000,
                                   effects = "fixed",
                                   component = "conditional",
                                   parameters = NULL,
                                   verbose = TRUE,
                                   ...) {
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
simulate_prior.bcplm <- function(model, n = 1000, verbose = TRUE, ...) {
  .simulate_prior(insight::get_priors(model, verbose = verbose), n = n, verbose = verbose)
}


#' @keywords internal
.simulate_prior <- function(priors, n = 1000, verbose = TRUE) {
  simulated <- data.frame(.bamboozled = 1:n)
  # distributions of the priors we could not simulate, named by parameter
  failed <- character(0)

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
    sim <- tryCatch(
      {
        if (prior$Distribution %in% c("t", "student_t", "Student's t")) {
          distribution(prior$Distribution, n, prior$df, prior$Location)
        } else {
          distribution(prior$Distribution, n, prior$Location, scale)
        }
      },
      error = function(e) NA
    )

    # unsupported distributions fail, and missing parameters (such as
    # the bounds of flat priors) give only NA values
    if (all(is.na(sim))) {
      failed[param] <- ifelse(is.na(prior$Distribution), "unknown", prior$Distribution)
    }

    simulated[param] <- sim
  }

  if (length(failed) && verbose) {
    distributions <- vapply(
      unique(failed),
      function(d) {
        paste0(d, " (", toString(names(failed)[failed == d]), ")")
      },
      character(1)
    )
    insight::format_warning(paste0(
      "Can't simulate priors from these distributions: ",
      toString(distributions),
      "."
    ))
  }

  simulated$.bamboozled <- NULL
  simulated
}
