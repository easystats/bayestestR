#' @title Find Default Equivalence (ROPE) Region Bounds
#'
#' @description Kruschke (2018) suggests that such null value could be set, by default, to the -0.1 to 0.1 range of a standardized parameter (negligible effect size according to Cohen, 1988).
#'
#' @param model A Bayesian model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(vs ~ mpg, data = mtcars, family="binomial")
#' rope_range(model)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' rope_range(model)
#' }
#'
#' @importFrom insight get_response model_info is_multivariate
#' @importFrom stats qlogis sd
#' @export
rope_range <- function(model){
  response <- insight::get_response(model)
  information <- insight::model_info(model)

  if (insight::is_multivariate(model)) {
    mapply(function(x, y) .rope_range(x, y), information, response)
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
