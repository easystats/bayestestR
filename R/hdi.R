#' Highest Density Interval (HDI)
#'
#' Compute the \strong{Highest Density Interval (HDI)} of posterior distributions. All points within this interval have a higher probability density than points outside the interval. The HDI can be used in the context of uncertainty characterisation of posterior distributions as \strong{Credible Interval (CI)}.
#'
#' @param x Vector representing a posterior distribution, or a data frame of such
#'   vectors. Can also be a Bayesian model (\code{stanreg}, \code{brmsfit},
#'   \code{MCMCglmm}, \code{mcmc} or \code{bcplm}) or a \code{BayesFactor} model.
#' @param ci Value or vector of probability of the (credible) interval - CI (between 0 and 1)
#'   to be estimated. Default to \code{.89} (89\%).
#' @param effects Should results for fixed effects, random effects or both be returned?
#'   Only applies to mixed models. May be abbreviated.
#' @param component Should results for all parameters, parameters for the conditional model
#'   or the zero-inflated part of the model be returned? May be abbreviated. Only
#'   applies to \pkg{brms}-models.
#' @param parameters Regular expression pattern that describes the parameters that
#'   should be returned. Meta-parameters (like \code{lp__} or \code{prior_}) are
#'   filtered by default, so only parameters that typically appear in the
#'   \code{summary()} are returned. Use \code{parameters} to select specific parameters
#'   for the output.
#' @param verbose Toggle off warnings.
#' @param ... Currently not used.
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/bayestestR.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details Unlike equal-tailed intervals (see \code{eti()}) that typically exclude 2.5\%
#' from each tail of the distribution and always include the median, the HDI is
#' \emph{not} equal-tailed and therefore always includes the mode(s) of posterior
#' distributions.
#' \cr \cr
#' By default, \code{hdi()} and \code{eti()} return the 89\% intervals (\code{ci = 0.89}),
#' deemed to be more stable than, for instance, 95\% intervals (\cite{Kruschke, 2014}).
#' An effective sample size of at least 10.000 is recommended if 95\% intervals
#' should be computed (\cite{Kruschke, 2014, p. 183ff}). Moreover, 89 indicates
#' the arbitrariness of interval limits - its only remarkable property is being
#' the highest prime number that does not exceed the already unstable 95\%
#' threshold (\cite{McElreath, 2015}).
#' \cr \cr
#' A 90\% equal-tailed interval (ETI) has 5\% of the distribution on either
#' side of its limits. It indicates the 5th percentile and the 95h percentile.
#' In symmetric distributions, the two methods of computing credible intervals,
#' the ETI and the \link[=hdi]{HDI}, return similar results.
#' \cr \cr
#' This is not the case for skewed distributions. Indeed, it is possible that
#' parameter values in the ETI have lower credibility (are less probable) than
#' parameter values outside the ETI. This property seems undesirable as a summary
#' of the credible values in a distribution.
#' \cr \cr
#' On the other hand, the ETI range does change when transformations are applied
#' to the distribution (for instance, for a log odds scale to probabilities):
#' the lower and higher bounds of the transformed distribution will correspond
#' to the transformed lower and higher bounds of the original distribution.
#' On the contrary, applying transformations to the distribution will change
#' the resulting HDI.
#'
#' @inherit ci return
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' hdi(posterior, ci = .89)
#' hdi(posterior, ci = c(.80, .90, .95))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' hdi(df)
#' hdi(df, ci = c(.80, .90, .95))
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' hdi(model)
#' hdi(model, ci = c(.80, .90, .95))
#'
#' library(emmeans)
#' hdi(emtrends(model, ~1, "wt"))
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' hdi(model)
#' hdi(model, ci = c(.80, .90, .95))
#'
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' hdi(bf)
#' hdi(bf, ci = c(.80, .90, .95))
#' }
#' @author Credits go to \href{https://rdrr.io/cran/ggdistribute/src/R/stats.R}{ggdistribute} and \href{https://github.com/mikemeredith/HDInterval}{HDInterval}.
#'
#' @references \itemize{
#'   \item Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press.
#'   \item McElreath, R. (2015). Statistical rethinking: A Bayesian course with examples in R and Stan. Chapman and Hall/CRC.
#' }
#'
#' @export
hdi <- function(x, ...) {
  UseMethod("hdi")
}



#' @rdname hdi
#' @export
hdi.numeric <- function(x, ci = .89, verbose = TRUE, ...) {
  out <- do.call(rbind, lapply(ci, function(i) {
    .hdi(x, ci = i, verbose = verbose)
  }))
  class(out) <- unique(c("bayestestR_hdi", "see_hdi", "bayestestR_ci", "see_ci", class(out)))
  attr(out, "data") <- x
  out
}



#' @rdname hdi
#' @export
hdi.data.frame <- function(x, ci = .89, verbose = TRUE, ...) {
  dat <- .compute_interval_dataframe(x = x, ci = ci, verbose = verbose, fun = "hdi")
  attr(dat, "object_name") <- .safe_deparse(substitute(x))
  dat
}






#' @rdname hdi
#' @export
hdi.MCMCglmm <- function(x, ci = .89, verbose = TRUE, ...) {
  nF <- x$Fixed$nfl
  d <- as.data.frame(x$Sol[, 1:nF, drop = FALSE])
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "hdi")
  attr(dat, "data") <- deparse(substitute(x), width.cutoff = 500)
  dat
}



#' @export
hdi.bamlss <- function(x, ci = .89, component = c("conditional", "location", "all"), verbose = TRUE, ...) {
  component <- match.arg(component)
  d <- insight::get_parameters(x, component = component)
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "hdi")
  attr(dat, "data") <- .safe_deparse(substitute(x))
  dat
}



#' @export
hdi.mcmc <- function(x, ci = .89, verbose = TRUE, ...) {
  d <- as.data.frame(x)
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "hdi")
  attr(dat, "data") <- .safe_deparse(substitute(x))
  dat
}



#' @export
hdi.bcplm <- function(x, ci = .89, verbose = TRUE, ...) {
  d <- insight::get_parameters(x)
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "hdi")
  attr(dat, "data") <- .safe_deparse(substitute(x))
  dat
}


#' @rdname hdi
#' @export
hdi.bayesQR <- hdi.bcplm

#' @export
hdi.mcmc.list <- hdi.bcplm


#' @rdname hdi
#' @export
hdi.sim.merMod <- function(x, ci = .89, effects = c("fixed", "random", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  dat <- .compute_interval_simMerMod(x = x, ci = ci, effects = effects, parameters = parameters, verbose = verbose, fun = "hdi")
  out <- dat$result
  attr(out, "data") <- dat$data
  out
}



#' @rdname hdi
#' @export
hdi.sim <- function(x, ci = .89, parameters = NULL, verbose = TRUE, ...) {
  dat <- .compute_interval_sim(x = x, ci = ci, parameters = parameters, verbose = verbose, fun = "hdi")
  out <- dat$result
  attr(out, "data") <- dat$data
  out
}



#' @rdname hdi
#' @export
hdi.emmGrid <- function(x, ci = .89, verbose = TRUE, ...) {
  xdf <- insight::get_parameters(x)

  out <- hdi(xdf, ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
hdi.emm_list <- hdi.emmGrid



#' @importFrom insight get_parameters
#' @rdname hdi
#' @export
hdi.stanreg <- function(x, ci = .89, effects = c("fixed", "random", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)

  out <- .prepare_output(
    hdi(insight::get_parameters(x, effects = effects, parameters = parameters), ci = ci, verbose = verbose, ...),
    insight::clean_parameters(x),
    inherits(x, "stanmvreg")
  )

  attr(out, "object_name") <- .safe_deparse(substitute(x))
  class(out) <- unique(c("bayestestR_hdi", "see_hdi", class(out)))
  out
}

#' @export
hdi.stanfit <- hdi.stanreg



#' @rdname hdi
#' @export
hdi.brmsfit <- function(x, ci = .89, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .prepare_output(
    hdi(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), ci = ci, verbose = verbose, ...),
    insight::clean_parameters(x)
  )

  attr(out, "object_name") <- .safe_deparse(substitute(x))
  class(out) <- unique(c("bayestestR_hdi", "see_hdi", class(out)))
  out
}



#' @rdname hdi
#' @export
hdi.BFBayesFactor <- function(x, ci = .89, verbose = TRUE, ...) {
  out <- hdi(insight::get_parameters(x), ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}


#' @keywords internal
.hdi <- function(x, ci = .89, verbose = TRUE) {
  check_ci <- .check_ci_argument(x, ci, verbose)

  if (!is.null(check_ci)) {
    return(check_ci)
  }

  x_sorted <- unname(sort.int(x, method = "quick")) # removes NA/NaN, but not Inf
  window_size <- ceiling(ci * length(x_sorted)) # See https://github.com/easystats/bayestestR/issues/39

  if (window_size < 2) {
    if (verbose) {
      warning("`ci` is too small or x does not contain enough data points, returning NAs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  nCIs <- length(x_sorted) - window_size

  if (nCIs < 1) {
    if (verbose) {
      warning("`ci` is too large or x does not contain enough data points, returning NAs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  ci.width <- sapply(1:nCIs, function(.x) x_sorted[.x + window_size] - x_sorted[.x])

  # find minimum of width differences, check for multiple minima
  min_i <- which(ci.width == min(ci.width))
  n_candies <- length(min_i)

  if (n_candies > 1) {
    if (any(diff(sort(min_i)) != 1)) {
      if (verbose) {
        warning("Identical densities found along different segments of the distribution, choosing rightmost.", call. = FALSE)
      }
      min_i <- max(min_i)
    } else {
      min_i <- floor(mean(min_i))
    }
  }

  data.frame(
    "CI" = ci * 100,
    "CI_low" = x_sorted[min_i],
    "CI_high" = x_sorted[min_i + window_size]
  )
}
