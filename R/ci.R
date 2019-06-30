#' Confidence/Credible Interval (CI)
#'
#' Compute Confidence/Credible Intervals (CI) for Bayesian and frequentist models. The Documentation is accessible for:
#'
#' \itemize{
#'  \item \href{https://easystats.github.io/bayestestR/articles/credible_interval.html}{Bayesian models}
#'  \item \href{https://easystats.github.io/parameters/reference/ci.merMod.html}{Frequentist models}
#' }
#'
#' @param x A \code{stanreg} or \code{brmsfit} model, or a vector representing a posterior distribution.
#' @param method Can be \link[=eti]{'ETI'} (default) or \link[=hdi]{'HDI'}.
#' @param ci Value or vector of probability of the CI (between 0 and 1)
#'   to be estimated. Default to \code{.89} (89\%) for Bayesian models and \code{.95} (95\%) for frequentist models.
#' @inheritParams hdi
#'
#' @return A data frame with following columns:
#'   \itemize{
#'     \item \code{Parameter} The model parameter(s), if \code{x} is a model-object. If \code{x} is a vector, this column is missing.
#'     \item \code{CI} The probability of the credible interval.
#'     \item \code{CI_low}, \code{CI_high} The lower and upper credible interval limits for the parameters.
#'   }
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' ci(posterior, method = "ETI")
#' ci(posterior, method = "HDI")
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' ci(df, method = "ETI", ci = c(.80, .89, .95))
#' ci(df, method = "HDI", ci = c(.80, .89, .95))
#'
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200)
#' ci(model, method = "ETI", ci = c(.80, .89))
#' ci(model, method = "HDI", ci = c(.80, .89))
#'
#' library(emmeans)
#' model <- emtrends(model, ~1, "wt")
#' ci(model, method = "ETI")
#' ci(model, method = "HDI")
#' \dontrun{
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' ci(model, method = "ETI")
#' ci(model, method = "HDI")
#'
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' ci(bf, method = "ETI")
#' ci(bf, method = "HDI")
#' }
#'
#' @export
ci <- function(x, ...) {
  UseMethod("ci")
}


#' @keywords internal
.ci_bayesian <- function(x, ci = .89, method = "ETI", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, verbose = TRUE, ...) {
  if (tolower(method) %in% c("eti", "equal", "ci", "quantile")) {
    return(eti(x, ci = ci, effects = effects, component = component, parameters = parameters, verbose = verbose, ...))
  } else if (tolower(method) %in% c("hdi")) {
    return(hdi(x, ci = ci, effects = effects, component = component, parameters = parameters, verbose = verbose, ...))
  } else {
    stop("`method` should be 'ETI' (for equal-tailed interval) or 'HDI' (for highest density interval).")
  }
}

#' @rdname ci
#' @export
ci.numeric <- function(x, ci = .89, method = "ETI", verbose = TRUE, ...) {
  .ci_bayesian(x, ci = ci, method = method, verbose = verbose, ...)
}



#' @rdname ci
#' @export
ci.data.frame <- ci.numeric

#' @rdname ci
#' @export
ci.emmGrid <- ci.numeric


#' @rdname ci
#' @export
ci.stanreg <- function(x, ci = .89, method = "ETI", effects = c("fixed", "random", "all"),
                       parameters = NULL, verbose = TRUE, ...) {
  .ci_bayesian(x, ci = ci, method = method, effects = effects, parameters = parameters, verbose = verbose, ...)
}


#' @rdname ci
#' @export
ci.brmsfit <- function(x, ci = .89, method = "ETI", effects = c("fixed", "random", "all"),
                       component = c("conditional", "zi", "zero_inflated", "all"),
                       parameters = NULL, verbose = TRUE, ...) {
  .ci_bayesian(x, ci = ci, method = method, effects = effects, component = component, parameters = parameters, verbose = verbose, ...)
}


#' @rdname ci
#' @export
ci.BFBayesFactor <- ci.numeric
