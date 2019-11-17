#' Returns Priors of a Model as Empirical Distributions
#'
#' Transforms priors information to actual distributions.
#'
#' @inheritParams effective_sample
#' @param n Size of the simulated prior distributions.
#'
#' @examples
#' library(bayestestR)
#' library(rstanarm)
#'
#' model <- stan_glm(mpg ~ wt + am, data = mtcars, chains = 1, refresh = 0)
#' simulate_prior(model)
#' @export
simulate_prior <- function(model, n = 1000, ...) {
  UseMethod("simulate_prior")
}



#' @export
simulate_prior.stanreg <- function(model, n = 1000, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)


  priors <-
    insight::get_priors(
      model,
      effects = effects,
      parameters = parameters
    )

  .simulate_prior(priors, n = n)
}


#' @export
simulate_prior.brmsfit <- function(model, n = 1000, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)
  component <- match.arg(component)

  priors <-
    insight::get_priors(
      model,
      effects = effects,
      component = component,
      parameters = parameters
    )

  .simulate_prior(priors, n = n)
}





#' @keywords internal
.simulate_prior <- function(priors, n = 1000) {
  simulated <- data.frame(.bamboozled = 1:n)

  # iterate over parameters
  for (param in priors[[1]]) {
    prior <- priors[priors[[1]] == param, ]

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
    prior <- distribution(prior$Distribution, n, prior$Location, scale)

    simulated[param] <- prior
  }

  simulated$.bamboozled <- NULL
  simulated
}
