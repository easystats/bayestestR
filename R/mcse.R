#' Monte-Carlo Standard Error (MCSE)
#'
#' This function returns the Monte Carlo Standard Error (MCSE).
#'
#' @inheritParams effective_sample
#'
#'
#' @details **Monte Carlo Standard Error (MCSE)** is another measure of
#' accuracy of the chains. It is defined as standard deviation of the chains
#' divided by their effective sample size (the formula for `mcse()` is
#' from Kruschke 2015, p. 187). The MCSE \dQuote{provides a quantitative
#' suggestion of how big the estimation noise is}.
#'
#' @references Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press.
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#' library(rstanarm)
#'
#' model <- suppressWarnings(
#'   stan_glm(mpg ~ wt + am, data = mtcars, chains = 1, refresh = 0)
#' )
#' mcse(model)
#' }
#' @export
mcse <- function(model, ...) {
  UseMethod("mcse")
}


#' @export
mcse.brmsfit <- function(model, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)
  component <- match.arg(component)

  params <-
    insight::get_parameters(
      model,
      effects = effects,
      component = component,
      parameters = parameters
    )

  ess <-
    effective_sample(
      model,
      effects = effects,
      component = component,
      parameters = parameters
    )

  .mcse(params, stats::setNames(ess$ESS, ess$Parameter))
}


#' @rdname mcse
#' @export
mcse.stanreg <- function(model, effects = c("fixed", "random", "all"), component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)
  component <- match.arg(component)

  params <-
    insight::get_parameters(
      model,
      effects = effects,
      component = component,
      parameters = parameters
    )

  ess <-
    effective_sample(
      model,
      effects = effects,
      component = component,
      parameters = parameters
    )

  .mcse(params, stats::setNames(ess$ESS, ess$Parameter))
}



#' @export
mcse.stanfit <- mcse.stanreg


#' @export
mcse.blavaan <- mcse.stanreg


#' @keywords internal
.mcse <- function(params, ess) {
  # get standard deviations from posterior samples
  stddev <- sapply(params, stats::sd)

  # check proper length, and for unequal length, shorten all
  # objects to common parameters
  if (length(stddev) != length(ess)) {
    common <- stats::na.omit(match(names(stddev), names(ess)))
    stddev <- stddev[common]
    ess <- ess[common]
    params <- params[common]
  }

  # compute mcse
  data.frame(
    Parameter = colnames(params),
    MCSE = stddev / sqrt(ess),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
