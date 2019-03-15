#' Confidence/Credible Interval
#'
#' Compute Confidence/Credible Intervals (CI) for Bayesian and frequentist models.
#' Documentation is accessible for:
#' \itemize{
#'   \item Bayesian models
#'   \item LM and GLMs
#'   \item Mixed models
#' }
#'
#'
#' @param model A model.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' library(bayestestR)
#'
#' ci(rnorm(1000))
#'
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' ci(model)
#' ci(model, ci = c(.80, .90, .95))
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' ci(model)
#' ci(model, ci = c(.80, .90, .95))
#' }
#'
#'
#' @export
ci <- function(model, ci = .90, ...) {
  UseMethod("ci")
}



#' @rdname ci
#' @export
ci.numeric <- function(model, ci = .90, verbose = TRUE, ...) {
  hdi(model, ci=ci, verbose=verbose, ...)
}

