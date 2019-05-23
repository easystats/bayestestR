#' @title Find Default Equivalence (ROPE) Region Bounds
#'
#' @description This function attempts at automatically finding suitable "default"
#'   values for the Region Of Practical Equivalence (ROPE).
#'
#' @details \cite{Kruschke (2018)} suggests that the region of practical
#'   equivalence could be set, by default, to a range from \code{-0.1} to
#'   \code{0.1} of a standardized parameter (negligible effect size
#'   according to Cohen, 1988), which can be generalised for linear models
#'   to \ifelse{html}{\out{-0.1 * SD<sub>y</sub>, 0.1 * SD<sub>y</sub>}}{\eqn{[-0.1*SD_{y}, 0.1*SD_{y}]}}.
#'   \cr \cr
#'   For logistic models, the parameters expressed in log odds ratio can be
#'   converted to standardized difference through the formula
#'   \ifelse{html}{\out{sqrt(3)/pi}}{\eqn{\sqrt{3}/\pi}}, resulting in a range
#'   of \code{-0.055} to \code{-0.055}.
#'   \cr \cr
#'   For other models with binary outcome, it is strongly recommended to
#'   manually specify the rope argument. Currently, the same default is applied
#'   that for logistic models.
#'   \cr \cr
#'   For t-tests, the standard deviation of the response is used,
#'   similarly to linear models (see above).
#'   \cr \cr
#'   For correlations, \code{-0.05, 0.05} is used, i.e., half the value of a
#'   negligible correlation as suggested by Cohen's (1988) rules of thumb.
#'   \cr \cr
#'   For all other models, \code{-0.1, 0.1} is used to determine the ROPE limitss,
#'   but it is strongly advised to specify it manually.
#'
#' @param x A \code{stanreg}, \code{brmsfit} or \code{BFBayesFactor} object.
#' @inheritParams rope
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(vs ~ mpg, data = mtcars, family = "binomial")
#' rope_range(model)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' rope_range(model)
#'
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' rope_range(bf)
#' }
#'
#' @references Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. Advances in Methods and Practices in Psychological Science, 1(2), 270-280. \doi{10.1177/2515245918771304}.
#'
#' @importFrom insight get_response model_info is_multivariate
#' @importFrom stats sd
#' @export
rope_range <- function(x, ...) {
  UseMethod("rope_range")
}


#' @export
rope_range.brmsfit <- function(x, ...) {
  response <- insight::get_response(x)
  information <- insight::model_info(x)

  if (insight::is_multivariate(x)) {
    mapply(function(i, j) .rope_range(i, j), x, information, response)
  } else {
    .rope_range(x, information, response)
  }
}


#' @export
rope_range.stanreg <- rope_range.brmsfit

#' @export
rope_range.BFBayesFactor <- rope_range.brmsfit



.rope_range <- function(x, information, response) {

  # Linear Models
  if (information$is_linear) {
    negligible_value <- 0.1 * stats::sd(response)

    # General Linear Models
  } else if (information$is_binomial) {
    negligible_value <- 0.1 * sqrt(3) / pi

    # T-tests
  } else if (information$is_ttest) {
    if ("BFBayesFactor" %in% class(x)) {
      negligible_value <- 0.1 * stats::sd(x@data[, 1])
    } else {
      warning("Could not estimate a good default ROPE range. Using 'c(-0.1, 0.1)'.")
      negligible_value <- 0.1
    }

    # Correlations
  } else if (information$is_correlation) {
    # https://github.com/easystats/bayestestR/issues/121
    negligible_value <- 0.05

    # Default
  } else {
    negligible_value <- 0.1
  }

  c(-1, 1) * negligible_value
}
