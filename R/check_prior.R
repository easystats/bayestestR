#' Check if Prior is Informative
#'
#' Performs a simple test to check whether the prior is informative to the posterior. This idea, and the accompanying heuristics, were discussed in \href{https://statmodeling.stat.columbia.edu/2019/08/10/}{this blogpost}.
#'
#' @inheritParams effective_sample
#' @param method Can be "gelman" or "lakeland". For the "gelman" method, if the SD of the posterior is more than 0.1 times the SD of the prior, then the prior is considered as informative. For the "lakeland" method, the prior is considered as informative if the posterior falls within the 95\% HDI of the prior.
#' @examples
#' library(bayestestR)
#' library(rstanarm)
#'
#' model <- stan_glm(mpg ~ wt + am, data = mtcars, chains = 1)
#' check_prior(model, method = "gelman")
#' check_prior(model, method = "lakeland")
#'
#' @references https://statmodeling.stat.columbia.edu/2019/08/10/
#' @export
check_prior <- function(model, method = "gelman", ...){
  UseMethod("check_prior")
}





#' @export
check_prior.brmsfit <- function(model, method = "gelman", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)

  posteriors <-
    insight::get_parameters(
      model,
      effects = effects,
      component = component,
      parameters = parameters
    )

  priors <-
    simulate_prior(
      model,
      effects = effects,
      component = component,
      parameters = parameters
    )

  .check_prior(priors, posteriors, method)
}

#' @export
check_prior.stanreg <- function(model, method = "gelman", effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)

  posteriors <-
    insight::get_parameters(
      model,
      effects = effects,
      parameters = parameters
    )

  priors <-
    simulate_prior(
      model,
      effects = effects,
      parameters = parameters
    )

  .check_prior(priors, posteriors, method)
}








#' @importFrom stats sd
#' @keywords internal
.check_prior <- function(priors, posteriors, method = "gelman"){

  result <- c()

  # iterate over parameters
  for (param in names(posteriors)) {

    posterior <- posteriors[[param]]
    prior <- priors[[param]]

    # Gelman
    if (method == "gelman") {
      if (stats::sd(posterior) > 0.1 * stats::sd(prior)) {
        result <- c(result, "informative")
      } else{
        result <- c(result, "uninformative")
      }
    } else if (method == "lakeland") {
      hdi <- hdi(prior, ci = .95)
      r <- rope(posterior, ci = 1, range = c(hdi$CI_low, hdi$CI_high))
      if (as.numeric(r) > 0.99) {
        result <- c(result, "informative")
      } else{
        result <- c(result, "misinformative")
      }
    } else{
      stop("method should be 'gelman' or 'lakeland'.")
    }
  }

  data.frame(
    Parameter = names(posteriors),
    Prior_Quality = result,
    stringsAsFactors = FALSE
  )
}