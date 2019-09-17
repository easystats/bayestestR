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
#' model <- stan_glm(mpg ~ wt + am, data = mtcars, chains = 1)
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
    if ("adjusted_scale" %in% names(prior)) {
      scale <- prior$adjusted_scale
    } else {
      scale <- prior$scale
    }

    # Simulate prior
    prior <- distribution(prior$distribution, n, prior$location, scale)

    simulated[param] <- prior
  }

  simulated$.bamboozled <- NULL
  simulated
}
