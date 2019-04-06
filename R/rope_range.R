#' @title Find Default Equivalence (ROPE) Region Bounds
#'
#' @description This function attempts at automatically finding suitable "default"
#'   values for the Region Of Practical Equivalence (ROPE). Kruschke (2018) suggests
#'   that such null value could be set, by default, to a range from \code{-0.1} to
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
#'   For all other models, \code{-0.1, 0.1} is used to determine the ROPE limits.
#'
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
#' }
#'
#' @references Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. Advances in Methods and Practices in Psychological Science, 1(2), 270-280. \doi{10.1177/2515245918771304}.
#'
#' @importFrom insight get_response model_info is_multivariate
#' @importFrom stats qlogis sd
#' @export
rope_range <- function(x) {
  response <- insight::get_response(x)
  information <- insight::model_info(x)

  if (insight::is_multivariate(x)) {
    mapply(function(i, j) .rope_range(i, j), information, response)
  } else {
    .rope_range(information, response)
  }
}

.rope_range <- function(information, response) {
  if (information$is_linear) {
    negligible_value <- 0.1 * stats::sd(response)
  } else if (information$is_binomial) {
    # https://github.com/easystats/bayestestR/issues/20
    # numeric_response <- as.numeric(as.factor(response))
    # prob_resp <- mean(numeric_response - min(numeric_response))
    # eff_size <- prob_resp / pi
    # negligible_value <- (stats::qlogis(prob_resp + eff_size) - stats::qlogis(prob_resp - eff_size)) / 4
    negligible_value <- 0.1 * sqrt(3) / pi
  } else {
    negligible_value <- 0.1
  }
  c(-1, 1) * negligible_value
}
