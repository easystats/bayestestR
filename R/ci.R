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
#' ci(posterior)
#' ci(posterior, ci = c(.80, .89, .95))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' ci(df)
#' ci(df, ci = c(.80, .89, .95))
#'
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200)
#' ci(model)
#' ci(model, ci = c(.80, .89, .95))
#'
#' library(emmeans)
#' ci(emtrends(model, ~1, "wt"))
#' \dontrun{
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' ci(model)
#' ci(model, ci = c(.80, .89, .95))
#'
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' ci(bf)
#' ci(bf, ci = c(.80, .89, .95))
#' }
#'
#' @export
ci <- function(x, ...) {
  UseMethod("ci")
}



#' @rdname ci
#' @export
ci.numeric <- function(x, ci = .89, verbose = TRUE, ...) {
  out <- do.call(rbind, lapply(ci, function(i) {
    .credible_interval(x = x, ci = i, verbose = verbose)
  }))
  class(out) <- unique(c("bayestestR_ci", "see_ci", class(out)))
  attr(out, "data") <- x
  out
}



#' @rdname ci
#' @export
ci.data.frame <- function(x, ci = .89, verbose = TRUE, ...) {
  dat <- .compute_interval_dataframe(x = x, ci = ci, verbose = verbose, fun = "ci")
  attr(dat, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  dat
}

#' @rdname ci
#' @export
ci.emmGrid <- function(x, ci = .89, verbose = TRUE, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package 'emmeans' required for this function to work. Please install it by running `install.packages('emmeans')`.")
  }
  xdf <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(x, names = FALSE)))

  dat <- .compute_interval_dataframe(x = xdf, ci = ci, verbose = verbose, fun = "ci")
  attr(dat, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  dat
}


#' @rdname ci
#' @export
ci.stanreg <- function(x, ci = .89, effects = c("fixed", "random", "all"),
                       parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  out <- .compute_interval_stanreg(x, ci, effects, parameters, verbose, fun = "ci")
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @rdname ci
#' @export
ci.brmsfit <- function(x, ci = .89, effects = c("fixed", "random", "all"),
                       component = c("conditional", "zi", "zero_inflated", "all"),
                       parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  out <- .compute_interval_brmsfit(x, ci, effects, component, parameters, verbose, fun = "ci")
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @rdname ci
#' @export
ci.BFBayesFactor <- function(x, ci = .89, verbose = TRUE, ...) {
  out <- ci(insight::get_parameters(x), ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @importFrom stats quantile
.credible_interval <- function(x, ci, verbose = TRUE) {
  check_ci <- .check_ci_argument(x, ci, verbose)

  if (!is.null(check_ci)) {
    return(check_ci)
  }

  .ci <- as.vector(stats::quantile(
    x,
    probs = c((1 - ci) / 2, (1 + ci) / 2),
    names = FALSE
  ))

  data.frame(
    "CI" = ci * 100,
    "CI_low" = .ci[1],
    "CI_high" = .ci[2]
  )
}
