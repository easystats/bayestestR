#' Sensitivity to Prior
#'
#' Computes the sensitivity to priors specification. This represents the proportion of change in some indices when the model is fitted with an antagonistic prior (a prior of same shape located on the opposite of the effect).
#'
#' @param model A Bayesian model (\code{stanreg} or \code{brmsfit}).
#' @param index The indices from which to compute the sensitivity. Can be one or multiple names of the columns returned by \code{describe_posterior}. The case is important here (e.g., write 'Median' instead of 'median').
#' @param magnitude This represent the magnitude by which to shift the antagonistic prior (to test the sensitivity). For instance, a magnitude of 10 (default) means that the mode wil be updated with a prior located at 10 standard deviations from its original location.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#'
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt, data = mtcars)
#' sensitivity_to_prior(model)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' sensitivity_to_prior(model, index = c("Median", "MAP"))
#'
#'
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' # sensitivity_to_prior(model)
#'
#' }
#' @importFrom stats update
#' @seealso DescTools
#' @export
sensitivity_to_prior <- function(model, index = "Median", magnitude = 10, ...) {
  UseMethod("sensitivity_to_prior")
}


#' @export
sensitivity_to_prior.stanreg <- function(model, index = "Median", magnitude = 10, ...) {
  # Original
  params <- .extract_parameters(model, index = index, ...)

  # Priors
  priors <- .extract_priors_rstanarm(model)
  new_priors <- .prior_new_location(prior = priors$prior, sign = sign(params$Median), magnitude = magnitude)
  model_updated <- stats::update(model, data = insight::get_data(model), prior = new_priors, refresh = 0)

  # New model
  params_updated <- .extract_parameters(model_updated, index = index, ...)

  # Compute index
  sensitivity <- abs(as.matrix(params_updated[-1]) - as.matrix(params[-1])) / abs(as.matrix(params[-1]))

  # Clean up
  sensitivity <- as.data.frame(sensitivity)
  names(sensitivity) <- paste0("Sensitivity_", names(params_updated)[-1])
  sensitivity <- cbind(params_updated[1], sensitivity)
  row.names(sensitivity) <- NULL
  sensitivity
}














#' @keywords internal
.extract_parameters <- function(model, index = "Median", ...) {
  # Handle BF
  test = c("pd", "rope", "p_map")
  if(any(c("bf", "bayesfactor", "bayes_factor") %in% c(index))){
    test <- c(test, "bf")
  }
  params <- suppressMessages(describe_posterior(model, centrality = "all", dispersion = TRUE, test = test, ...))

  params <- params[params$Parameter!="(Intercept)",]
  params[unique(c("Parameter", "Median", index))]
}




#' Set a new location for a prior
#' @keywords internal
.prior_new_location <- function(prior, sign, magnitude = 10){
  prior$location <- -1 * sign * magnitude * prior$scale
  prior
}





#' Extract and Returns the priors formatted for rstanarm
#' @keywords internal
.extract_priors_rstanarm <- function(model, ...){
  priors <- rstanarm::prior_summary(model)

  # Deal with adjusted scale
  if(!is.null(priors$prior$adjusted_scale)){
    priors$prior$scale <- priors$prior$adjusted_scale
    priors$prior$adjusted_scale <- NULL
  }
  priors$prior$autoscale <- FALSE


  priors
}