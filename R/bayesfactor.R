#' Bayes Factor (BF)
#'
#' Compute the Bayes Factor, i.e., the likelihood ratio between two competing hypotheses.
#'
#' @param x Vector representing a posterior distribution.
#' @param prior Vector representing a prior distribution. If a prior is not provided, will sample from \code{~Cauchy(location = null, scale = sd(posterior))} (but this should be avoided).
#' @param method The method to be used. Currently only \code{"savage-dickey"} is supported.
#' @param h0 Value to be tested against (usually \code{0} in the context of null hypothesis testing).
#' @param direction Test type. One of \code{-1} (left tailed), \code{0} (defult; two tailed) or \code{1} (right tailed).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(bayestestR)
#'
#' prior <- rnorm_perfect(1000, mean = 0, sd = 1)
#' posterior <- rnorm_perfect(1000, mean = .5, sd = .3)
#'
#' bayesfactor(posterior, prior = prior)
#'
#' @references
#' Wagenmakers, E. J., Lodewyckx, T., Kuriyal, H., & Grasman, R. (2010). Bayesian
#' hypothesis testing for psychologists: A tutorial on the Savage-Dickey method.
#' Cognitive psychology, 60(3), 158-189.
#'
#' @author Mattan S. Ben-Shachar
#'
#' @export
bayesfactor <- function(x, prior, h0 = 0, direction = 0, method = "savage-dickey", ...){
  UseMethod("bayesfactor")
}

#' @rdname bayesfactor
#' @export
bayesfactor.numeric <- function(x, prior, h0 = 0, direction = 0, method = "savage-dickey", ...) {
  method <- match.arg(method)
  if (method == "savage-dickey") {
    bf_val <- bayesfactor_savagedickey(x, prior = prior, h0 = h0, direction = direction)
  } else {
    stop("Other methods not supported yet.")
  }

  bf_val
}


#' @importFrom stats rcauchy sd
#' @rdname bayesfactor
#' @export
bayesfactor_savagedickey <- function(x, prior, h0 = 0, direction = 0){
  if (missing(prior)) {
    prior <- stats::rcauchy(
      n        = length(x),
      location = h0,
      scale    = stats::sd(x)
    )
    warning("Prior not specified!\n",
            "Used Cauchy prior with location = ", h0, " and scale = ", round(stats::sd(x)), ".\n",
            "Please specify your own priors appropriate prior!")
  }

  if (requireNamespace("logspline")) {
    f_post <- suppressWarnings(logspline::logspline(x))
    f_prior <- suppressWarnings(logspline::logspline(prior))

    d_post <- logspline::dlogspline(h0, f_post)
    d_prior <- logspline::dlogspline(h0, f_prior)

    norm_post <- norm_prior <- 1
    if (direction < 0) {
      norm_post <- logspline::plogspline(h0, f_post)
      norm_prior <- logspline::plogspline(h0, f_prior)
    } else if (direction > 0) {
      norm_post <- 1 - logspline::plogspline(h0, f_post)
      norm_prior <- 1 - logspline::plogspline(h0, f_prior)
    }
  } else {
    insight::print_color("Consider installing the `logspline` package for a more robust estimate.\n", "red")
    d_post <- density_at(x, h0)
    d_prior <- density_at(prior, h0)

    norm_post <- norm_prior <- 1
    if (direction < 0) {
      norm_post <- mean(x < h0)
      norm_prior <- mean(prior < h0)
    } else if (direction > 0) {
      norm_post <- 1 - mean(x < 0)
      norm_prior <- 1 - mean(prior < h0)
    }
  }

  bf_val <- (d_prior / norm_prior) / (d_post / norm_post)
  class(bf_val) <- c("bayesfactor", class(bf_val))
  attr(bf_val, "method") <- "savage-dickey"
  attr(bf_val, "H0") <- h0

  bf_val
}
