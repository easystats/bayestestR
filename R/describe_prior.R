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
#' if (require("rstanarm")) {
#'   model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#'   describe_prior(model)
#' }
#'
#' # brms models
#' # -----------------------------------------------
#' if (require("brms")) {
#'   model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#'   describe_prior(model)
#' }
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' if (require("BayesFactor")) {
#'   bf <- ttestBF(x = rnorm(100, 1, 1))
#'   describe_prior(bf)
#' }
#' }
#' @importFrom insight get_priors
#' @export
describe_prior <- function(model, ...) {
  UseMethod("describe_prior")
}



#' @export
describe_prior.brmsfit <- function(model,
                                   effects = c("fixed", "random", "all"),
                                   component = c(
                                     "conditional", "zi", "zero_inflated",
                                     "all", "location", "distributional", "auxiliary"
                                   ),
                                   parameters = NULL, ...) {
  .describe_prior(model, parameters = parameters)
}


# Internal ----------------------------------------------------------------



#' @keywords internal
.describe_prior <- function(model, parameters = NULL, ...) {
  priors <- insight::get_priors(model, ...)

  # Format names
  names(priors)[-1] <- paste0("Prior_", names(priors)[-1])

  # If the prior scale has been adjusted, it is the actual scale that was used.
  if ("Prior_Adjusted_Scale" %in% names(priors)) {
    priors$Prior_Scale[!is.na(priors$Prior_Adjusted_Scale)] <- priors$Prior_Adjusted_Scale[!is.na(priors$Prior_Adjusted_Scale)]
    priors$Prior_Adjusted_Scale <- NULL
  }

  if ("Prior_Response" %in% names(priors)) {
    names(priors)[names(priors) == "Prior_Response"] <- "Response"
  }

  # make sure parameter names match between prior output and model
  cp <- insight::clean_parameters(model)

  ## TODO for now, only fixed effects
  if ("Effects" %in% names(cp)) {
    cp <- cp[cp$Effects == "fixed", ]
  }

  if (!is.null(parameters) && !all(priors$Parameter %in% parameters)) {
    cp$Cleaned_Parameter <- gsub("(.*)(\\.|\\[)\\d+(\\.|\\])", "\\1", cp$Cleaned_Parameter)
    cp$Cleaned_Parameter[cp$Cleaned_Parameter == "Intercept"] <- "(Intercept)"
    colnames(priors)[1] <- "Cleaned_Parameter"
    out <- merge(cp, priors, by = "Cleaned_Parameter", all = TRUE)
    out <- out[!duplicated(out$Parameter), ]
    priors <- out[intersect(colnames(out), c("Parameter", "Prior_Distribution", "Prior_df", "Prior_Location", "Prior_Scale", "Response"))]
  }

  priors
}


#' @export
describe_prior.stanreg <- .describe_prior



#' @export
describe_prior.bcplm <- .describe_prior

#' @export
describe_prior.blavaan <- .describe_prior

#' @export
describe_prior.mcmc.list <- function(model, ...) {
  NULL
}

#' @export
describe_prior.BGGM <- function(model, ...) {
  NULL
}

#' @export
describe_prior.bamlss <- function(model, ...) {
  NULL
}



#' @export
describe_prior.BFBayesFactor <- function(model, ...) {
  priors <- insight::get_priors(model)

  # Format names
  names(priors)[-1] <- paste0("Prior_", names(priors)[-1])

  priors
}
