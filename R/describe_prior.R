#' Describe Priors
#'
#' Returns a summary of the priors used in the model.
#'
#' @param model A Bayesian model.
#'
#' @examples
#' \dontrun{
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
#' # library(BayesFactor)
#' # bf <- ttestBF(x = rnorm(100, 1, 1))
#' # describe_prior(bf)
#' }
#'
#' @importFrom insight get_priors
#'
#' @export
describe_prior <- function(model, ...) {
  UseMethod("describe_prior")
}



#' @keywords internal
.describe_prior <- function(model, ...) {
  priors <- insight::get_priors(model)

  # Format names
  names(priors) <- tools::toTitleCase(names(priors))
  names(priors)[-1] <- paste0("Prior_", names(priors)[-1])
  names(priors) <- gsub("Prior_Adjusted_scale", "Prior_Scale_adjusted", names(priors))

  # If the prior scale has been adjusted, it is the actual scale.
  if ("Prior_Scale_adjusted" %in% names(priors)) {
    priors$Prior_Scale[!is.na(priors$Prior_Scale_adjusted)] <- priors$Prior_Scale_adjusted[!is.na(priors$Prior_Scale_adjusted)]
    priors$Prior_Scale_adjusted <- NULL
  }

  priors
}


#' @export
describe_prior.stanreg <- .describe_prior

# #' @export
# describe_prior.brmsfit <- .describe_prior
