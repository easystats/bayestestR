#' @title Find Effect Size Thresholds
#'
#' @description This function attempts at automatically finding suitable default values for a "significant" (i.e., non-negligible) and "large" effect. This is to be used with care, and the chosen threshold should always be explicitly reported and justified. It is set, by default, to \code{0.05} and \code{0.3} of the standard deviation of the outcome variable (tiny and large effect sizes for correlations according to Funder \& Ozer, 2019).
#'
#' @details
#'   \itemize{
#'     \item For \strong{linear models (lm)}, this can be generalised to \ifelse{html}{\out{0.05 * SD<sub>y</sub>}}{\eqn{[0.05*SD_{y}]}} and \ifelse{html}{\out{0.3 * SD<sub>y</sub>}}{\eqn{[0.3*SD_{y}]}} for significant and large effects, respectively.
#'     \item For \strong{logistic models}, the parameters expressed in log odds ratio can be converted to standardized difference through the formula \ifelse{html}{\out{&pi;/&radic;(3)}}{\eqn{\pi/\sqrt{3}}}, resulting a threshold of \code{0.09} and \code{0.54}.
#'     \item For other models with \strong{binary outcome}, it is strongly recommended to manually specify the rope argument. Currently, the same default is applied that for logistic models.
#'     \item For models from \strong{count data}, the residual variance is used. This is a rather experimental threshold and is probably often similar to \code{0.05} and \code{0.3}, but should be used with care!
#'     \item For \strong{t-tests}, the standard deviation of the response is used, similarly to linear models (see above).
#'     \item For \strong{correlations},\code{0.05} and \code{0.3} are used.
#'     \item For all other models, \code{0.05} and \code{0.3} are used, but it is strongly advised to specify it manually.
#'   }
#'
#' @inheritParams rope
#'
#' @examples
#' sexit_thresholds(rnorm(1000))
#'
#' \dontrun{
#' if (require("rstanarm")) {
#'   model <- stan_glm(
#'     mpg ~ wt + gear,
#'     data = mtcars,
#'     chains = 2,
#'     iter = 200,
#'     refresh = 0
#'   )
#'   sexit_thresholds(model)
#'
#'   model <- stan_glm(vs ~ mpg, data = mtcars, family = "binomial", refresh=0)
#'   sexit_thresholds(model)
#' }
#'
#' if (require("brms")) {
#'   model <- brm(mpg ~ wt + cyl, data = mtcars)
#'   sexit_thresholds(model)
#' }
#'
#' if (require("BayesFactor")) {
#'   bf <- ttestBF(x = rnorm(100, 1, 1))
#'   sexit_thresholds(bf)
#' }
#' }
#' @references Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. Advances in Methods and Practices in Psychological Science, 1(2), 270-280. \doi{10.1177/2515245918771304}.
#'
#' @importFrom insight get_response model_info is_multivariate
#' @importFrom stats sd
#' @export
sexit_thresholds <- function(x, ...) {
  UseMethod("sexit_thresholds")
}


#' @export
sexit_thresholds.brmsfit <- function(x, ...) {
  response <- insight::get_response(x)
  information <- insight::model_info(x)

  if (insight::is_multivariate(x)) {
    mapply(function(i, j) .sexit_thresholds(i, j), x, information, response)
  } else {
    .sexit_thresholds(x, information, response)
  }
}


#' @export
sexit_thresholds.stanreg <- sexit_thresholds.brmsfit

#' @export
#' @importFrom stats sd
sexit_thresholds.BFBayesFactor <- function(x, ...){
  fac <- 1
  if (inherits(x@numerator[[1]], "BFlinearModel")) {

    response <- tryCatch(
      {
        insight::get_response(x)
      },
      error = function(e) {
        NULL
      }
    )

    if (!is.null(response)) {
      fac <- stats::sd(response, na.rm = TRUE)
    }
  }

  fac * .sexit_thresholds(x)
}

#' @export
sexit_thresholds.lm <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.glm <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.merMod <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.glmmTMB <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.mixed <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.MixMod <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.wbm <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.feis <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.gee <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.geeglm <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.lme <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.felm <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.fixest <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.gls <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.hurdle <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.zeroinfl <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.bayesQR <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.default <- function(x, ...) {
  .sexit_thresholds(x)
}

#' @export
sexit_thresholds.mlm <- function(x, ...) {
  response <- insight::get_response(x)
  information <- insight::model_info(x)

  lapply(response, function(i) .sexit_thresholds(x, information, i))
}




# helper ------------------


#' @importFrom stats sigma sd
#' @importFrom insight n_obs find_parameters
.sexit_thresholds <- function(x, information=NULL, response=NULL) {
  if(is.null(information) && is.null(response)){
    norm <- 1
  } else{
    norm <- tryCatch(
      {
        # Linear Models
        if (information$is_linear) {
          stats::sd(response, na.rm = TRUE)

          # Logistic Regression Models
        } else if (information$is_binomial) {
          pi / sqrt(3)

          # Count Models
        } else if (information$is_count) {
          sig <- stats::sigma(x)
          if (!is.null(sig) && length(sig) > 0 && !is.na(sig)) {
            sig
          } else {
            1
          }

          # T-tests
        } else if (information$is_ttest) {
          if ("BFBayesFactor" %in% class(x)) {
            stats::sd(x@data[, 1])
          } else {
            warning("Could not estimate good thresholds, using default values.", call. = FALSE)
            1
          }

          # Correlations
        } else if (information$is_correlation) {
          # https://github.com/easystats/bayestestR/issues/121
          1

          # Default
        } else {
          1
        }
      },
      error = function(e) {
        warning("Could not estimate good thresholds, using default values.", call. = FALSE)
        1
      }
    )
  }

  c(0.05, 0.3) * norm
}
