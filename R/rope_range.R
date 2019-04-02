#' @title Find Default Equivalence (ROPE) Region Bounds
#'
#' @description This function attempts at finding suitable "default" values
#'   for the Region Of Practical Equivalence (ROPE). Kruschke (2018) suggests
#'   that such null value could be set, by default, to a range from \code{-0.1} to
#'   \code{0.1} times of a standardized parameter (negligible effect size
#'   according to Cohen, 1988), which can be generalised for linear models
#'   to \ifelse{html}{\out{-0.1 * SD<sub>y</sub>, 0.1 * SD<sub>y</sub>}}{\eqn{[-0.1*SD_{y}, 0.1*SD_{y}]}}.
#'   \cr \cr
#'   For models with binary outcome, there is no direct way to specify the
#'   effect size that defines the ROPE limits. Two examples from Kruschke suggest
#'   that a negligible change is about .05 on the logit-scale. In these cases,
#'   it is recommended to specify the rope argument, however, if not specified,
#'   the ROPE limits are caluclated as suggested by Kruschke, i.e. the effect
#'   size is the probability of "success" for the outcome, divided by \code{pi}.
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
