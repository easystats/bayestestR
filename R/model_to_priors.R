#' Convert model's posteriors to priors
#'
#' Convert model's posteriors to priors.
#'
#' @param model A Bayesian model.
#' @param scale_multiply The SD of the posterior will be multiplied by this amount before being set as a prior to avoid overly narrow priors.
#' @param ... Other arguments for \code{insight::get_prior()} or \code{\link{describe_posterior}}.
#'
#' @examples
#' \dontrun{
#' # brms models
#' # -----------------------------------------------
#' if (require("brms")) {
#'   formula <- brms::brmsformula(mpg ~ wt + cyl, center = FALSE)
#'
#'   model <- brms::brm(formula, data = mtcars, refresh = 0)
#'   priors <- model_to_priors(model)
#'   priors <- brms::validate_prior(priors, formula, data = mtcars)
#'   priors
#'
#'   model2 <- brms::brm(formula, data = mtcars, prior = priors, refresh = 0)
#' }
#' }
#' @export
model_to_priors <- function(model, scale_multiply = 3, ...) {
  UseMethod("model_to_priors")
}


#' @export
model_to_priors.brmsfit <- function(model, scale_multiply = 3, ...) {
  params <- describe_posterior(model, centrality = "mean", dispersion = TRUE, ci = NULL, test = NULL, ...)
  priors_params <- attributes(insight::get_priors(model, ...))$priors
  priors <- brms::prior_summary(model)

  for(p in priors_params$Parameter) {
    if(p %in% params$Parameter) {
      subset <- params[params$Parameter == p, ]
      priors$prior[priors_params$Parameter == p] <- paste0(
        "normal(",
        insight::format_value(subset$Mean),
        ", ",
        insight::format_value(subset$SD * scale_multiply),
        ")")
    }
  }
  priors
}

