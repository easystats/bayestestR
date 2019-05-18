#' @title Compute Monte-Carlo Standard Error
#' @name mcse
#'
#' @description \code{mcse()} returns the Monte Carlo standard error.
#'
#' @inheritParams effective_samples
#'
#' @return \code{mcse()} and \code{n_eff()} return a tibble with two columns: one
#'   with the term names and one with the related statistic resp. effective
#'   sample size.
#'
#' @details The Monte Carlo Standard Error is another useful measure of accuracy of
#'  the chains. It is defined as standard deviation of the chains divided by
#'  their effective sample size (the formula for \code{mcse()} is from
#'  Kruschke 2015, p. 187). The MCSE \dQuote{provides a quantitative suggestion
#'  of how big the estimation noise is}.
#'
#' @references Kruschke JK. Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan. 2nd edition. Academic Press, 2015
#'
#' @examples
#' \dontrun{
#' if (require("rstanarm")) {
#'   m <- stan_glm(mpg ~ wt + am, data = mtcars, chains = 1)
#'   mcse(m)
#' }}
#' @importFrom insight get_parameters
#' @export
mcse <- function(model, ...) {
  UseMethod("mcse")
}


#' @export
mcse.brmsfit <- function(model, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
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

  ess <- effective_samples(model, effects, component, parameters)
  mcse_helper(pars, ess$ESS)
}


#' @rdname mcse
#' @export
mcse.stanreg <- function(model, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)

  pars <-
    insight::get_parameters(
      model,
      effects = effects,
      parameters = parameters
    )

  ess <- effective_samples(model, effects, parameters)
  mcse_helper(pars, ess$ESS)
}



#' @importFrom stats sd
mcse_helper <- function(pars, ess) {
  # get standard deviations from posterior samples
  stddev <- sapply(pars, stats::sd)

  # compute mcse
  data.frame(
    Parameter = colnames(pars),
    MCSE = stddev / sqrt(ess),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
