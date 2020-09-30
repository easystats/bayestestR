#' Confidence/Credible/Compatibility Interval (CI)
#'
#' Compute Confidence/Credible/Compatibility Intervals (CI) or Support Intervals (SI) for Bayesian and frequentist models. The Documentation is accessible for:
#'
#' \itemize{
#'  \item \href{https://easystats.github.io/bayestestR/articles/credible_interval.html}{Bayesian models}
#'  \item \href{https://easystats.github.io/parameters/reference/ci.merMod.html}{Frequentist models}
#' }
#'
#' @param x A \code{stanreg} or \code{brmsfit} model, or a vector representing a posterior distribution.
#' @param method Can be \link[=eti]{'ETI'} (default), \link[=hdi]{'HDI'} or \link[=si]{'SI'}.
#' @param ci Value or vector of probability of the CI (between 0 and 1)
#'   to be estimated. Default to \code{.89} (89\%) for Bayesian models and \code{.95} (95\%) for frequentist models.
#' @inheritParams hdi
#' @inheritParams si
#'
#' @return A data frame with following columns:
#'   \itemize{
#'     \item \code{Parameter} The model parameter(s), if \code{x} is a model-object. If \code{x} is a vector, this column is missing.
#'     \item \code{CI} The probability of the credible interval.
#'     \item \code{CI_low}, \code{CI_high} The lower and upper credible interval limits for the parameters.
#'   }
#'
#' @note When it comes to interpretation, we recommend thinking of the CI in terms of
#'   an "uncertainty" or "compatibility" interval, the latter being defined as
#'   \dQuote{Given any value in the interval and the background assumptions,
#'   the data should not seem very surprising} (\cite{Gelman & Greenland 2019}).
#'   \cr \cr
#'   There is also a \href{https://easystats.github.io/see/articles/bayestestR.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @references Gelman A, Greenland S. Are confidence intervals better termed "uncertainty intervals"? BMJ 2019;l5381. \doi{10.1136/bmj.l5381}
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
#' \dontrun{
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ wt, data = mtcars, chains = 2, iter = 200, refresh = 0)
#'   ci(model, method = "ETI", ci = c(.80, .89))
#'   ci(model, method = "HDI", ci = c(.80, .89))
#'   ci(model, method = "SI")
#' }
#'
#' if (require("brms")) {
#'   model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#'   ci(model, method = "ETI")
#'   ci(model, method = "HDI")
#'   ci(model, method = "SI")
#' }
#'
#' if (require("BayesFactor")) {
#'   bf <- ttestBF(x = rnorm(100, 1, 1))
#'   ci(bf, method = "ETI")
#'   ci(bf, method = "HDI")
#' }
#'
#' if (require("emmeans")) {
#'   model <- emtrends(model, ~1, "wt")
#'   ci(model, method = "ETI")
#'   ci(model, method = "HDI")
#'   ci(model, method = "SI")
#' }
#' }
#' @export
ci <- function(x, ...) {
  UseMethod("ci")
}


#' @keywords internal
.ci_bayesian <- function(x, ci = .89, method = "ETI", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, verbose = TRUE, BF = 1, ...) {
  if (tolower(method) %in% c("eti", "equal", "ci", "quantile")) {
    return(eti(x, ci = ci, effects = effects, component = component, parameters = parameters, verbose = verbose, ...))
  } else if (tolower(method) %in% c("hdi")) {
    return(hdi(x, ci = ci, effects = effects, component = component, parameters = parameters, verbose = verbose, ...))
  } else if (tolower(method) %in% c("si")) {
    return(si(x, BF = BF, effects = effects, component = component, parameters = parameters, verbose = verbose, ...))
  } else {
    stop("`method` should be 'ETI' (for equal-tailed interval),'HDI' (for highest density interval) or 'SI' (for support interval).")
  }
}

#' @rdname ci
#' @export
ci.numeric <- function(x, ci = .89, method = "ETI", verbose = TRUE, BF = 1, ...) {
  .ci_bayesian(x, ci = ci, method = method, verbose = verbose, BF = BF, ...)
}



#' @rdname ci
#' @export
ci.data.frame <- ci.numeric

#' @rdname ci
#' @export
ci.emmGrid <- ci.numeric

#' @export
ci.emm_list <- ci.emmGrid



#' @rdname ci
#' @export
ci.sim.merMod <- function(x, ci = .89, method = "ETI", effects = c("fixed", "random", "all"),
                          parameters = NULL, verbose = TRUE, ...) {
  .ci_bayesian(x, ci = ci, method = method, effects = effects, parameters = parameters, verbose = verbose, ...)
}



#' @rdname ci
#' @export
ci.sim <- function(x, ci = .89, method = "ETI", parameters = NULL, verbose = TRUE, ...) {
  .ci_bayesian(x, ci = ci, method = method, parameters = parameters, verbose = verbose, ...)
}



#' @rdname ci
#' @export
ci.stanreg <- function(x, ci = .89, method = "ETI", effects = c("fixed", "random", "all"),
                       parameters = NULL, verbose = TRUE,  BF = 1, ...) {
  .ci_bayesian(x, ci = ci, method = method, effects = effects, parameters = parameters, verbose = verbose, BF = BF, ...)
}


#' @rdname ci
#' @export
ci.brmsfit <- function(x, ci = .89, method = "ETI", effects = c("fixed", "random", "all"),
                       component = c("conditional", "zi", "zero_inflated", "all"),
                       parameters = NULL, verbose = TRUE, BF = 1, ...) {
  .ci_bayesian(x, ci = ci, method = method, effects = effects, component = component, parameters = parameters, verbose = verbose, BF = BF, ...)
}


#' @export
ci.stanfit <- ci.stanreg


#' @rdname ci
#' @export
ci.BFBayesFactor <- ci.numeric




#' @rdname ci
#' @export
ci.MCMCglmm <- function(x, ci = .89, method = "ETI", verbose = TRUE, ...) {
  nF <- x$Fixed$nfl
  ci(as.data.frame(x$Sol[, 1:nF, drop = FALSE]), ci = ci, method = method, verbose = verbose, ...)
}



#' @export
ci.mcmc <- function(x, ci = .89, method = "ETI", verbose = TRUE, ...) {
  ci(as.data.frame(x), ci = ci, method = method, verbose = verbose, ...)
}


#' @export
ci.bcplm <- function(x, ci = .89, method = "ETI", verbose = TRUE, ...) {
  ci(insight::get_parameters(x), ci = ci, method = method, verbose = verbose, ...)
}
