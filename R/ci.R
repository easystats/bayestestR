#' Confidence/Credible/Compatibility Interval (CI)
#'
#' Compute Confidence/Credible/Compatibility Intervals (CI) or Support Intervals
#' (SI) for Bayesian and frequentist models. The Documentation is accessible
#' for:
#'
#' - [Bayesian models](https://easystats.github.io/bayestestR/articles/credible_interval.html)
#' - [Frequentist models](https://easystats.github.io/parameters/reference/ci.default.html)
#'
#' @param x A `stanreg` or `brmsfit` model, or a vector representing a posterior
#' distribution.
#' @param method Can be ["ETI"][eti] (default), ["HDI"][hdi], ["BCI"][bci],
#' ["SPI"][spi] or ["SI"][si].
#' @param ci Value or vector of probability of the CI (between 0 and 1)
#'   to be estimated. Default to `0.95` (`95%`).
#' @inheritParams hdi
#' @inheritParams si
#' @inherit hdi seealso
#' @family ci
#'
#' @inheritSection hdi Model components
#'
#' @return A data frame with following columns:
#'
#' - `Parameter` The model parameter(s), if `x` is a model-object. If `x` is a
#'   vector, this column is missing.
#' - `CI` The probability of the credible interval.
#' - `CI_low`, `CI_high` The lower and upper credible interval limits for the parameters.
#'
#' @note When it comes to interpretation, we recommend thinking of the CI in terms of
#'   an "uncertainty" or "compatibility" interval, the latter being defined as
#'   "Given any value in the interval and the background assumptions,
#'   the data should not seem very surprising" (_Gelman & Greenland 2019_).
#'
#'   There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @references
#' Gelman A, Greenland S. Are confidence intervals better termed "uncertainty
#' intervals"? BMJ 2019;l5381. 10.1136/bmj.l5381
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
#' model <- suppressWarnings(rstanarm::stan_glm(
#'   mpg ~ wt,
#'   data = mtcars, chains = 2, iter = 200, refresh = 0
#' ))
#' ci(model, method = "ETI", ci = c(0.80, 0.89))
#' ci(model, method = "HDI", ci = c(0.80, 0.89))
#'
#' @examplesIf require("BayesFactor", quietly = TRUE)
#' bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
#' ci(bf, method = "ETI")
#' ci(bf, method = "HDI")
#'
#' @examplesIf require("emmeans", quietly = TRUE) && require("rstanarm", quietly = TRUE)
#' model <- emmeans::emtrends(model, ~1, "wt", data = mtcars)
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
                         effects = "fixed",
                         component = "conditional",
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
    insight::format_error(paste0(
      "`method` should be 'ETI' (for equal-tailed interval), ",
      "'HDI' (for highest density interval), 'BCI' (for bias corrected and ",
      "accelerated bootstrap intervals), 'SPI' (for shortest probability ",
      "interval) or 'SI' (for support interval)."
    ))
  }
}


#' @rdname ci
#' @export
ci.numeric <- function(x, ci = 0.95, method = "ETI", verbose = TRUE, BF = 1, ...) {
  .ci_bayesian(x, ci = ci, method = method, verbose = verbose, BF = BF, ...)
}


#' @rdname ci
#' @inheritParams p_direction
#' @export
ci.data.frame <- function(x,
                          ci = 0.95,
                          method = "ETI",
                          BF = 1,
                          rvar_col = NULL,
                          verbose = TRUE,
                          ...) {
  x_rvar <- .possibly_extract_rvar_col(x, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::ci
    cl$x <- x_rvar
    cl$rvar_col <- NULL
    out <- eval.parent(cl)

    obj_name <- insight::safe_deparse_symbol(substitute(x))
    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    return(.append_datagrid(out, x, long = length(ci) > 1L))
  }

  .ci_bayesian(x, ci = ci, method = method, verbose = verbose, BF = BF, ...)
}


#' @export
ci.draws <- function(x, ci = 0.95, method = "ETI", verbose = TRUE, BF = 1, ...) {
  .ci_bayesian(
    .posterior_draws_to_df(x),
    ci = ci,
    method = method,
    verbose = verbose,
    BF = BF,
    ...
  )
}

#' @export
ci.rvar <- ci.draws


#' @export
ci.emmGrid <- function(x, ci = NULL, ...) {
  if (!.is_baysian_grid(x)) {
    insight::check_if_installed("parameters")
    if (is.null(ci)) ci <- 0.95
    return(parameters::ci(model = x, ci = ci, ...))
  }

  if (is.null(ci)) ci <- 0.95
  xdf <- insight::get_parameters(x)
  out <- ci(xdf, ci = ci, ...)
  out <- .append_datagrid(out, x, long = length(ci) > 1L)
  out
}

#' @export
ci.emm_list <- ci.emmGrid


#' @export
ci.slopes <- function(x, ci = NULL, ...) {
  if (!.is_baysian_grid(x)) {
    insight::check_if_installed("parameters")
    if (is.null(ci)) ci <- 0.95
    return(parameters::ci(model = x, ci = ci, ...))
  }

  if (is.null(ci)) ci <- 0.95
  xrvar <- .get_marginaleffects_draws(x)
  out <- ci(xrvar, ci = ci, ...)
  out <- .append_datagrid(out, x, long = length(ci) > 1L)
  out
}

#' @export
ci.comparisons <- ci.slopes

#' @export
ci.predictions <- ci.slopes


#' @export
ci.sim.merMod <- function(x,
                          ci = 0.95,
                          method = "ETI",
                          effects = "fixed",
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


#' @export
ci.stanreg <- function(x,
                       ci = 0.95,
                       method = "ETI",
                       effects = "fixed",
                       component = "location",
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
                       effects = "fixed",
                       component = "conditional",
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
ci.CmdStanFit <- ci.stanreg

#' @export
ci.blavaan <- ci.stanreg

#' @export
ci.BFBayesFactor <- ci.numeric


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
                      component = "all",
                      verbose = TRUE,
                      ...) {
  ci(
    insight::get_parameters(x, component = component, verbose = verbose),
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
