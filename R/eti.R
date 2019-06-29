#' Equal-Tailed Interval (ETI)
#'
#' Compute the \strong{Equal-Tailed Interval (ETI)} of posterior distributions using the quantiles method. The probability of being below this interval is equal to the probability of being above it. The ETI can be used in the context of uncertainty characterisation of posterior distributions as \strong{Credible Interval (CI)}.
#'
#' @inheritParams hdi
#' @inherit ci return
#' @inherit hdi details
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' eti(posterior)
#' eti(posterior, ci = c(.80, .89, .95))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' eti(df)
#' eti(df, ci = c(.80, .89, .95))
#'
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200)
#' eti(model)
#' eti(model, ci = c(.80, .89, .95))
#'
#' library(emmeans)
#' eti(emtrends(model, ~1, "wt"))
#' \dontrun{
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' eti(model)
#' eti(model, ci = c(.80, .89, .95))
#'
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' eti(bf)
#' eti(bf, ci = c(.80, .89, .95))
#' }
#'
#' @export
eti <- function(x, ...) {
  UseMethod("eti")
}



#' @rdname eti
#' @export
eti.numeric <- function(x, ci = .89, verbose = TRUE, ...) {
  out <- do.call(rbind, lapply(ci, function(i) {
    .eti(x = x, ci = i, verbose = verbose)
  }))
  class(out) <- unique(c("bayestestR_ci", "see_ci", class(out)))
  attr(out, "data") <- x
  out
}



#' @rdname eti
#' @export
eti.data.frame <- function(x, ci = .89, verbose = TRUE, ...) {
  dat <- .compute_interval_dataframe(x = x, ci = ci, verbose = verbose, fun = "eti")
  attr(dat, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  dat
}

#' @rdname eti
#' @export
eti.emmGrid <- function(x, ci = .89, verbose = TRUE, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package 'emmeans' required for this function to work. Please install it by running `install.packages('emmeans')`.")
  }
  xdf <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(x, names = FALSE)))

  dat <- .compute_interval_dataframe(x = xdf, ci = ci, verbose = verbose, fun = "eti")
  attr(dat, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  dat
}


#' @rdname eti
#' @export
eti.stanreg <- function(x, ci = .89, effects = c("fixed", "random", "all"),
                       parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  out <- .compute_interval_stanreg(x, ci, effects, parameters, verbose, fun = "eti")
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @rdname eti
#' @export
eti.brmsfit <- function(x, ci = .89, effects = c("fixed", "random", "all"),
                       component = c("conditional", "zi", "zero_inflated", "all"),
                       parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  out <- .compute_interval_brmsfit(x, ci, effects, component, parameters, verbose, fun = "eti")
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @rdname eti
#' @export
eti.BFBayesFactor <- function(x, ci = .89, verbose = TRUE, ...) {
  out <- eti(insight::get_parameters(x), ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @importFrom stats quantile
.eti <- function(x, ci, verbose = TRUE) {
  check_ci <- .check_ci_argument(x, ci, verbose)

  if (!is.null(check_ci)) {
    return(check_ci)
  }

  results <- as.vector(stats::quantile(
    x,
    probs = c((1 - ci) / 2, (1 + ci) / 2),
    names = FALSE
  ))

  data.frame(
    "CI" = ci * 100,
    "CI_low" = results[1],
    "CI_high" = results[2]
  )
}
