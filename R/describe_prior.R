#' Describe Priors
#'
#' Returns a summary of the priors used in the model.
#'
#' @param model A Bayesian model.
#' @param ... Currently not used.
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#'
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' describe_prior(model)
#'
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' describe_prior(model)
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' describe_prior(bf)
#' }
#'
#' @importFrom insight get_priors
#'
#' @export
describe_prior <- function(model, ...) {
  UseMethod("describe_prior")
}



#' @importFrom tools toTitleCase
#' @keywords internal
.describe_prior <- function(model, ...) {
  priors <- insight::get_priors(model, ...)

  # Format names
  names(priors) <- tolower(names(priors))
  names(priors)[-1] <- paste0("prior_", names(priors)[-1])

  # If the prior scale has been adjusted, it is the actual scale that was used.
  if ("prior_adjusted_scale" %in% names(priors)) {
    priors$prior_scale[!is.na(priors$prior_adjusted_scale)] <- priors$prior_adjusted_scale[!is.na(priors$prior_adjusted_scale)]
    priors$prior_adjusted_scale <- NULL
  }

  string <- strsplit(names(priors), "_", fixed = TRUE)
  string <- lapply(string, tools::toTitleCase)
  names(priors) <- unlist(lapply(string, paste0, collapse = "_"))

  priors
}


#' @export
describe_prior.stanreg <- .describe_prior

#' @export
describe_prior.brmsfit <- .describe_prior





#' @export
describe_prior.BFBayesFactor <- function(model, ...) {
  priors <- insight::get_priors(model)

  # Format names
  names(priors) <- tools::toTitleCase(names(priors))
  names(priors)[-1] <- paste0("Prior_", names(priors)[-1])

  priors
}
