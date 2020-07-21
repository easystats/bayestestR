#' Check if Prior is Informative
#'
#' Performs a simple test to check whether the prior is informative to the posterior. This idea, and the accompanying heuristics, were discussed in \href{https://statmodeling.stat.columbia.edu/2019/08/10/}{this blogpost}.
#'
#' @inheritParams effective_sample
#' @param method Can be "gelman" or "lakeland". For the "gelman" method, if the SD of the posterior is more than 0.1 times the SD of the prior, then the prior is considered as informative. For the "lakeland" method, the prior is considered as informative if the posterior falls within the 95\% HDI of the prior.
#' @param simulate_priors Should prior distributions be simulated using \code{simulate_prior} (default; faster) or sampled (slower, more accurate).
#' @examples
#' \dontrun{
#' library(bayestestR)
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ wt + am, data = mtcars, chains = 1, refresh = 0)
#'   check_prior(model, method = "gelman")
#'   check_prior(model, method = "lakeland")
#'
#'   # An extreme example where both methods diverge:
#'   model <- stan_glm(mpg ~ wt, data = mtcars[1:3,],
#'                     prior = normal(-3.3, 1, FALSE),
#'                     prior_intercept = normal(0, 1000, FALSE),
#'                     refresh = 0)
#'   check_prior(model, method = "gelman")
#'   check_prior(model, method = "lakeland")
#'   plot(si(model)) # can provide visual confirmation to the Lakeland method
#' }
#' }
#' @references https://statmodeling.stat.columbia.edu/2019/08/10/
#'
#' @export
check_prior <- function(model, method = "gelman", simulate_priors = TRUE, ...) {
  UseMethod("check_prior")
}





#' @export
check_prior.brmsfit <- function(model, method = "gelman", simulate_priors = TRUE, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)
  component <- match.arg(component)

  posteriors <-
    insight::get_parameters(
      model,
      effects = effects,
      component = component,
      parameters = parameters
    )

  if (isTRUE(simulate_priors)) {
    priors <-
      simulate_prior(
        model,
        effects = effects,
        component = component,
        parameters = parameters
      )
  } else {
    priors <- unupdate(model, verbose = FALSE)
    priors <-
      insight::get_parameters(
        priors,
        effects = effects,
        component = component,
        parameters = parameters
      )
  }

  .check_prior(priors, posteriors, method)
}

#' @export
check_prior.stanreg <- check_prior.brmsfit


#' @importFrom stats sd
#' @keywords internal
.check_prior <- function(priors, posteriors, method = "gelman") {

  .gelman <- function(prior, posterior){
    if (stats::sd(posterior) > 0.1 * stats::sd(prior)) {
      "informative"
    } else {
      "uninformative"
    }
  }

  .lakeland <- function(prior, posterior){
    hdi <- hdi(prior, ci = .95)
    r <- rope(posterior, ci = 1, range = c(hdi$CI_low, hdi$CI_high))
    if (as.numeric(r) > 0.99) {
      "informative"
    } else {
      "misinformative"
    }
  }

  if (method == "gelman") {
    result <- mapply(.gelman, priors, posteriors)
  } else if (method == "lakeland") {
    result <- mapply(.lakeland, priors, posteriors)
  } else {
    stop("method should be 'gelman' or 'lakeland'.")
  }

  data.frame(
    Parameter = names(posteriors),
    Prior_Quality = unname(result),
    stringsAsFactors = FALSE
  )
}
