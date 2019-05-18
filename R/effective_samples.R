#' @title Effective Sample
#' @name effective_samples
#'
#' @description \code{effective_samples()} returns the effective sample size.
#'
#' @param model A \code{stanreg}, \code{stanfit}, or \code{brmsfit} object.
#' @param ... Not used
#' @inheritParams hdi
#'
#' @return A data frame with two columns: Parameter name and effective sample size.
#'
#' @details The effective sample size divides the actual sample size by the amount
#'  of autocorrelation. The effective sample size is a measure of \dQuote{how
#'  much independent information there is in autocorrelated chains}, or:
#'  \dQuote{What would be the sample size of a completely non-autocorrelated chain
#'  that yielded the same information?} (\emph{Kruschke 2015, p182-3}).
#'  The ratio of effective number of samples and total number of samples
#'  ranges from 0 to 1, and should be close to 1. The closer this ratio comes
#'  to zero means that the chains may be inefficient, but possibly still okay.
#'
#' @references Kruschke JK. Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan. 2nd edition. Academic Press, 2015
#'
#' @examples
#' \dontrun{
#' if (require("rstanarm")) {
#'   m <- stan_glm(mpg ~ wt + am, data = mtcars, chains = 1)
#'   effective_samples(m)
#' }}
#' @export
effective_samples <- function(model, ...) {
  UseMethod("effective_samples")
}


#' @rdname effective_samples
#' @export
effective_samples.brmsfit <- function(model, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)
  component <- match.arg(component)

  pars <-
    insight::get_parameters(
      model,
      effects = effects,
      component = component,
      parameters = parameters
    )

  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' required for this function to work. Please install it.")
  }

  s <- rstan::summary(model$fit)$summary
  s <- s[rownames(s) %in% colnames(pars), ]

  data.frame(
    Parameter = rownames(s),
    ESS = round(s[, "n_eff"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}



#' @rdname effective_samples
#' @export
effective_samples.stanreg <- function(model, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)

  pars <-
    insight::get_parameters(
      model,
      effects = effects,
      parameters = parameters
    )

  s <- summary(model)
  s <- s[rownames(s) %in% colnames(pars), ]

  data.frame(
    Parameter = rownames(s),
    ESS = round(s[, "n_eff"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
