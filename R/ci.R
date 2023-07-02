#' Confidence/Credible/Compatibility Interval (CI)
#'
#' Compute Confidence/Credible/Compatibility Intervals (CI) or Support Intervals
#' (SI) for Bayesian and frequentist models. The Documentation is accessible
#' for:
#'
#' \itemize{
#'  \item [Bayesian models](https://easystats.github.io/bayestestR/articles/credible_interval.html)
#'  \item [Frequentist models](https://easystats.github.io/parameters/reference/ci.default.html)
#' }
#'
#' @param x A `stanreg` or `brmsfit` model, or a vector representing a posterior distribution.
#' @param method Can be ['ETI'][eti] (default), ['HDI'][hdi], ['BCI'][bci], ['SPI'][spi] or ['SI'][si].
#' @param ci Value or vector of probability of the CI (between 0 and 1)
#'   to be estimated. Default to `.95` (`95%`).
#' @inheritParams hdi
#' @inheritParams si
#' @inherit hdi seealso
#' @family ci
#'
#' @return A data frame with following columns:
#'   \itemize{
#'     \item `Parameter` The model parameter(s), if `x` is a model-object. If `x` is a vector, this column is missing.
#'     \item `CI` The probability of the credible interval.
#'     \item `CI_low`, `CI_high` The lower and upper credible interval limits for the parameters.
#'   }
#'
#' @note When it comes to interpretation, we recommend thinking of the CI in terms of
#'   an "uncertainty" or "compatibility" interval, the latter being defined as
#'   \dQuote{Given any value in the interval and the background assumptions,
#'   the data should not seem very surprising} (\cite{Gelman & Greenland 2019}).
#'   \cr \cr
#'   There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @references Gelman A, Greenland S. Are confidence intervals better termed "uncertainty intervals"? BMJ 2019;l5381. 10.1136/bmj.l5381
#'
#'
#' @examplesIf require("rstanarm", quietly = TRUE)
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' ci(posterior, method = "ETI")
#' ci(posterior, method = "HDI")
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' ci(df, method = "ETI", ci = c(0.80, 0.89, 0.95))
#' ci(df, method = "HDI", ci = c(0.80, 0.89, 0.95))
#'
#' model <- suppressWarnings(
#'   stan_glm(mpg ~ wt, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' )
#' ci(model, method = "ETI", ci = c(0.80, 0.89))
#' ci(model, method = "HDI", ci = c(0.80, 0.89))
#'
#' @examplesIf require("BayesFactor", quietly = TRUE)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' ci(bf, method = "ETI")
#' ci(bf, method = "HDI")
#'
#' @examplesIf require("emmeans", quietly = TRUE) && require("rstanarm", quietly = TRUE)
#' model <- emtrends(model, ~1, "wt", data = mtcars)
#' ci(model, method = "ETI")
#' ci(model, method = "HDI")
#' @export
ci <- function(x, ...) {
  UseMethod("ci")
}


#' @keywords internal
.ci_bayesian <- function(x,
                         ci = 0.95,
                         method = "ETI",
                         effects = c("fixed", "random", "all"),
                         component = c("conditional", "zi", "zero_inflated", "all"),
                         parameters = NULL,
                         verbose = TRUE,
                         BF = 1,
                         ...) {
  if (tolower(method) %in% c("eti", "equal", "ci", "quantile")) {
    return(
      eti(
        x,
        ci = ci,
        effects = effects,
        component = component,
        parameters = parameters,
        verbose = verbose,
        ...
      )
    )
  } else if (tolower(method) %in% c("bci", "bca", "bcai")) {
    return(
      bci(
        x,
        ci = ci,
        effects = effects,
        component = component,
        parameters = parameters,
        verbose = verbose,
        ...
      )
    )
  } else if (tolower(method) == "hdi") {
    return(
      hdi(
        x,
        ci = ci,
        effects = effects,
        component = component,
        parameters = parameters,
        verbose = verbose,
        ...
      )
    )
  } else if (tolower(method) == "spi") {
    return(
      spi(
        x,
        ci = ci,
        effects = effects,
        component = component,
        parameters = parameters,
        verbose = verbose,
        ...
      )
    )
  } else if (tolower(method) == "si") {
    return(
      si(
        x,
        BF = BF,
        effects = effects,
        component = component,
        parameters = parameters,
        verbose = verbose,
        ...
      )
    )
  } else {
    insight::format_error(
      "`method` should be 'ETI' (for equal-tailed interval),'HDI' (for highest density interval), 'BCI' (for bias corrected and accelerated bootstrap intervals), 'SPI' (for shortest probability interval) or 'SI' (for support interval)."
    )
  }
}


#' @rdname ci
#' @export
ci.numeric <- function(x, ci = 0.95, method = "ETI", verbose = TRUE, BF = 1, ...) {
  .ci_bayesian(x, ci = ci, method = method, verbose = verbose, BF = BF, ...)
}


#' @rdname ci
#' @export
ci.data.frame <- ci.numeric



#' @export
ci.draws <- function(x, ci = 0.95, method = "ETI", verbose = TRUE, BF = 1, ...) {
  .ci_bayesian(.posterior_draws_to_df(x), ci = ci, method = method, verbose = verbose, BF = BF, ...)
}

#' @export
ci.rvar <- ci.draws


#' @export
ci.emmGrid <- function(x, ci = NULL, ...) {
  if (!.is_baysian_emmeans(x)) {
    insight::check_if_installed("parameters")
    if (is.null(ci)) ci <- 0.95
    return(parameters::ci(model = x, ci = ci, ...))
  }

  if (is.null(ci)) ci <- 0.95
  x <- insight::get_parameters(x)
  ci(x, ci = ci, ...)
}


#' @export
ci.emm_list <- ci.emmGrid



#' @rdname ci
#' @export
ci.sim.merMod <- function(x,
                          ci = 0.95,
                          method = "ETI",
                          effects = c("fixed", "random", "all"),
                          parameters = NULL,
                          verbose = TRUE,
                          ...) {
  .ci_bayesian(
    x,
    ci = ci,
    method = method,
    effects = effects,
    parameters = parameters,
    verbose = verbose,
    ...
  )
}



#' @rdname ci
#' @export
ci.sim <- function(x,
                   ci = 0.95,
                   method = "ETI",
                   parameters = NULL,
                   verbose = TRUE,
                   ...) {
  .ci_bayesian(
    x,
    ci = ci,
    method = method,
    parameters = parameters,
    verbose = verbose,
    ...
  )
}



#' @rdname ci
#' @export
ci.stanreg <- function(x,
                       ci = 0.95,
                       method = "ETI",
                       effects = c("fixed", "random", "all"),
                       component = c(
                         "location",
                         "all",
                         "conditional",
                         "smooth_terms",
                         "sigma",
                         "distributional",
                         "auxiliary"
                       ),
                       parameters = NULL,
                       verbose = TRUE,
                       BF = 1,
                       ...) {
  .ci_bayesian(
    x,
    ci = ci,
    method = method,
    effects = effects,
    component = component,
    parameters = parameters,
    verbose = verbose,
    BF = BF,
    ...
  )
}


#' @rdname ci
#' @export
ci.brmsfit <- function(x,
                       ci = 0.95,
                       method = "ETI",
                       effects = c("fixed", "random", "all"),
                       component = c("conditional", "zi", "zero_inflated", "all"),
                       parameters = NULL,
                       verbose = TRUE,
                       BF = 1,
                       ...) {
  .ci_bayesian(
    x,
    ci = ci,
    method = method,
    effects = effects,
    component = component,
    parameters = parameters,
    verbose = verbose,
    BF = BF,
    ...
  )
}


#' @export
ci.stanfit <- ci.stanreg


#' @export
ci.blavaan <- ci.stanreg


#' @rdname ci
#' @export
ci.BFBayesFactor <- ci.numeric




#' @rdname ci
#' @export
ci.MCMCglmm <- function(x, ci = 0.95, method = "ETI", verbose = TRUE, ...) {
  nF <- x$Fixed$nfl
  ci(
    as.data.frame(x$Sol[, 1:nF, drop = FALSE]),
    ci = ci,
    method = method,
    verbose = verbose,
    ...
  )
}


#' @export
ci.bamlss <- function(x,
                      ci = 0.95,
                      method = "ETI",
                      component = c("all", "conditional", "location"),
                      verbose = TRUE,
                      ...) {
  component <- match.arg(component)
  ci(
    insight::get_parameters(x, component = component),
    ci = ci,
    method = method,
    verbose = verbose,
    ...
  )
}


#' @export
ci.bcplm <- function(x, ci = 0.95, method = "ETI", verbose = TRUE, ...) {
  ci(insight::get_parameters(x), ci = ci, method = method, verbose = verbose, ...)
}


#' @export
ci.blrm <- ci.bcplm


#' @export
ci.mcmc <- ci.bcplm


#' @export
ci.mcmc.list <- ci.bcplm


#' @export
ci.BGGM <- ci.bcplm


#' @export
ci.get_predicted <- ci.data.frame
