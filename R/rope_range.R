#' @title Find Default Equivalence (ROPE) Region Bounds
#'
#' @description This function attempts at finding suitable "default" values for the Region Of Practical Equivalence (ROPE). Kruschke (2018) suggests that such null value could be set, by default, to the \code{-0.1} to \code{0.1} range of a standardized parameter (negligible effect size according to Cohen, 1988), which can be generalised for linear models to \eqn{[-0.1*SD_{y}, 0.1*SD_{y}]}.
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
    effect_size_d <- 0.1 * stats::sd(response)
  } else if (information$is_binomial) {
    numeric_response <- as.numeric(as.factor(response))
    prob_resp <- mean(numeric_response - min(numeric_response))
    eff_size <- prob_resp / pi
    effect_size_d <- (stats::qlogis(prob_resp + eff_size) - stats::qlogis(prob_resp - eff_size)) / 4
  } else {
    effect_size_d <- 0.1
  }
  c(-1, 1) * effect_size_d
}
