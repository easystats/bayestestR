#' @title Find Effect Size Thresholds
#'
#' @description This function attempts at automatically finding suitable default
#' values for a "significant" (i.e., non-negligible) and "large" effect. This is
#' to be used with care, and the chosen threshold should always be explicitly
#' reported and justified. See the detail section in [`sexit()`][sexit] for more
#' information.
#'
#' @inheritParams rope
#'
#' @examples
#' sexit_thresholds(rnorm(1000))
#' \dontrun{
#' if (require("rstanarm")) {
#'   model <- suppressWarnings(stan_glm(
#'     mpg ~ wt + gear,
#'     data = mtcars,
#'     chains = 2,
#'     iter = 200,
#'     refresh = 0
#'   ))
#'   sexit_thresholds(model)
#'
#'   model <- suppressWarnings(
#'     stan_glm(vs ~ mpg, data = mtcars, family = "binomial", refresh = 0)
#'   )
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
#' @export
sexit_thresholds <- function(x, ...) {
  UseMethod("sexit_thresholds")
}


#' @export
sexit_thresholds.brmsfit <- function(x, verbose = TRUE, ...) {
  response <- insight::get_response(x, source = "mf")
  information <- insight::model_info(x, verbose = FALSE)

  if (insight::is_multivariate(x)) {
    mapply(function(i, j) .sexit_thresholds(i, j), x, information, response, verbose)
  } else {
    .sexit_thresholds(x, information, response, verbose)
  }
}


#' @export
sexit_thresholds.stanreg <- sexit_thresholds.brmsfit

#' @export
sexit_thresholds.BFBayesFactor <- function(x, verbose = TRUE, ...) {
  fac <- 1
  if (inherits(x@numerator[[1]], "BFlinearModel")) {
    response <- .safe(insight::get_response(x, source = "mf"))
    if (!is.null(response)) {
      fac <- stats::sd(response, na.rm = TRUE)
    }
  }

  fac * .sexit_thresholds(x, verbose = verbose)
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
sexit_thresholds.default <- function(x, verbose = TRUE, ...) {
  .sexit_thresholds(x, verbose = verbose)
}

#' @export
sexit_thresholds.mlm <- function(x, verbose = TRUE, ...) {
  response <- insight::get_response(x, type = "mf")
  information <- insight::model_info(x, verbose = FALSE)

  lapply(response, function(i) .sexit_thresholds(x, information, i, verbose = verbose))
}




# helper ------------------


.sexit_thresholds <- function(x, information = NULL, response = NULL, verbose = TRUE) {
  if (is.null(information) && is.null(response)) {
    norm <- 1
  } else {
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
          if (inherits(x, "BFBayesFactor")) {
            stats::sd(x@data[, 1])
          } else {
            if (verbose) {
              insight::format_warning("Could not estimate good thresholds, using default values.")
            }
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
        if (verbose) {
          insight::format_warning("Could not estimate good thresholds, using default values.")
        }
        1
      }
    )
  }

  c(0.05, 0.3) * norm
}
