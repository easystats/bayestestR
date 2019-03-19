#' Confidence/Credible Interval
#'
#' Compute Confidence/Credible Intervals (CI) for Bayesian and frequentist models.
#' Documentation is accessible for:
#' \itemize{
#'   \item Bayesian models
#'   \item LM and GLMs
#'   \item Mixed models
#' }
#'
#' @inheritParams hdi
#'
#' @examples
#' library(bayestestR)
#'
#' ci(rnorm(1000))
#'
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' ci(model)
#' ci(model, ci = c(.80, .90, .95))
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' ci(model)
#' ci(model, ci = c(.80, .90, .95))}
#'
#' @export
ci <- function(posterior, ci = .90, ...) {
  UseMethod("ci")
}



#' @rdname ci
#' @export
ci.numeric <- function(posterior, ci = .90, verbose = TRUE, ...) {
  do.call(rbind, lapply(ci, function(i) {
    .credible_interval(x = posterior, ci = i, verbose = verbose)
  }))
}


#' @rdname ci
#' @export
ci.stanreg <- function(posterior, ci = .90, effects = c("fixed", "random", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  .compute_interval_stanreg(posterior, ci, effects, parameters, verbose, fun = "ci")
}


#' @rdname ci
#' @export
ci.brmsfit <- function(posterior, ci = .90, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  .compute_interval_brmsfit(posterior, ci, effects, component, parameters, verbose, fun = "ci")
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
