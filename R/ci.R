#' Confidence/Credible Interval
#'
#' Compute Confidence/Credible Intervals (CI) for Bayesian and frequentist models using quantiles.
#'
#' Documentation is accessible for:
#' \itemize{
#'   \item \href{https://easystats.github.io/bayestestR/reference/ci.html}{Bayesian models}
#' }
#'
#' @param x A \code{stanreg} or \code{brmsfit} model , or a vector representing a posterior distribution.
#' @inheritParams hdi
#'
#' @return A data frame with following columns:
#'   \itemize{
#'     \item \code{Parameter} The model parameter(s), if \code{x} is a model-object. If \code{x} is a vector, this column is missing.
#'     \item \code{CI} The probability of the credible interval.
#'     \item \code{CI_low} , \code{CI_high} The lower and upper credible interval limits for the parameters.
#'   }
#'
#' @examples
#' library(bayestestR)
#'
#' ci(rnorm(1000))
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' ci(model)
#' ci(model, ci = c(.80, .90, .95))
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' ci(model)
#' ci(model, ci = c(.80, .90, .95))
#' }
#'
#' @export
ci <- function(x, ...) {
  UseMethod("ci")
}



#' @rdname ci
#' @export
ci.numeric <- function(x, ci = .90, verbose = TRUE, ...) {
  do.call(rbind, lapply(ci, function(i) {
    .credible_interval(x = x, ci = i, verbose = verbose)
  }))
}


#' @rdname ci
#' @export
ci.stanreg <- function(x, ci = .90, effects = c("fixed", "random", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  .compute_interval_stanreg(x, ci, effects, parameters, verbose, fun = "ci")
}


#' @rdname ci
#' @export
ci.brmsfit <- function(x, ci = .90, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  .compute_interval_brmsfit(x, ci, effects, component, parameters, verbose, fun = "ci")
}



#' @importFrom stats quantile
.credible_interval <- function(x, ci, verbose = TRUE) {
  check_ci <- .check_ci_argument(x, ci, verbose)

  if (!is.null(check_ci)) {
    return(check_ci)
  }

  .ci <- as.vector(stats::quantile(x, probs = c((1 - ci) / 2, (1 + ci) / 2), names = FALSE))

  data.frame(
    "CI" = ci * 100,
    "CI_low" = .ci[1],
    "CI_high" = .ci[2]
  )
}
