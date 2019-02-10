#' Region of Practical Equivalence (ROPE)
#'
#' Compute the proportion (in percentage) of the HDI (default to the 90\% HDI) of a posterior distribution that lies within a region of practical equivalence.
#'
#' @param posterior vector representing a posterior distribution. Can also be a \code{stanreg} or \code{brmsfit} model.
#' @param bounds ROPE's lower and higher bounds. Should be a list of two values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the bounds are set to \code{c(0.1, 0.1)} if input is a vector and \code{x +- 0.1*SD(response)} if a Bayesian model is provided.
#' @param ci The Credible Interval (CI) probability, corresponding to the proportion of HDI, to use.
#' @param verbose Toggle off warnings.
#'
#' @details Statistically, the probability of a posterior distribution of being different from 0 does not make much sense (the probability of it being different from a single point being infinite). Therefore, the idea underlining ROPE is to let the user define an area around the null value enclosing values that are equivalent to the null value for practical purposes (2010, 2011, 2014). Kruschke (2018) suggests that such null value could be set, by default, to the -0.1 to 0.1 range of a standardized parameter (negligible effect size according to Cohen). This could be generalized: For instance, for linear models, the ROPE could be set as 0 +/- .1 * sd(y). Kruschke (2010, 2011, 2014) suggest using the proportion of the 95\% (or 90\%, considered more stable) \link[=hdi]{HDI} that falls within the ROPE as an index for "null-hypothesis" testing (as understood under the Bayesian framework, see \link[=equivalence_test]{equivalence_test}). Besides the ROPE-based decision criteria, the proportion of the 95\% CI that falls in the ROPE can be used as a continuous index.
#'
#' @examples
#' library(bayestestR)
#'
#' rope(posterior = rnorm(1000, 0, 0.01), bounds = c(-0.1, 0.1))
#' rope(posterior = rnorm(1000, 0, 1), bounds = c(-0.1, 0.1))
#' rope(posterior = rnorm(1000, 1, 0.01), bounds = c(-0.1, 0.1))
#' rope(posterior = rnorm(1000, 1, 1), ci = c(.90, .95))
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' rope(model)
#' rope(model, ci = c(.90, .95))
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' rope(model)
#' rope(model, ci = c(.90, .95))
#' }
#'
#' @export
rope <- function(posterior, bounds = "default", ci = .90, verbose = TRUE) {
  UseMethod("rope")
}


#' @method as.double rope
#' @export
as.double.rope <- function(x, ...) {
  x$ROPE_Percentage
}


#' @export
print.rope <- function(x, ...) {
  cat(sprintf(
    "%.2f%% of the %s%% CI is in ROPE [%.2f, %.2f]",
    x$ROPE_Percentage,
    x$CI,
    x$ROPE_low,
    x$ROPE_high
  ))
}




#' @export
rope.numeric <- function(posterior, bounds = "default", ci = .90, verbose = TRUE) {
  if (all(bounds == "default")) {
    bounds <- c(-0.1, 0.1)
  } else if (!all(is.numeric(bounds)) | length(bounds) != 2) {
    stop("`bounds` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  rope_values <- lapply(ci, function(i) {
    .rope(posterior, bounds = bounds, ci = i, verbose = verbose)
  })

  out <- flatten_list(rope_values)
  if (nrow(out) > 1) {
    out$ROPE_Percentage <- as.numeric(out$ROPE_Percentage)
  }
  return(out)
}




.rope <- function(posterior, bounds = c(-0.1, 0.1), ci = .90, verbose = TRUE) {
  HDI_area <- hdi(posterior, ci, verbose)

  if (anyNA(HDI_area)) {
    rope_percentage <- NA
  } else {
    HDI_area <- posterior[posterior >= HDI_area$CI_low & posterior <= HDI_area$CI_high]
    area_within <- HDI_area[HDI_area >= min(bounds) & HDI_area <= max(bounds)]
    rope_percentage <- length(area_within) / length(HDI_area) * 100
  }


  rope <- data.frame(
    "CI" = ci*100,
    "ROPE_low" = bounds[1],
    "ROPE_high" = bounds[2],
    "ROPE_Percentage" = rope_percentage
  )

  class(rope) <- c("rope", class(rope))

  rope
}



#' @importFrom insight get_response get_parameters
#' @importFrom stats sd
#' @keywords internal
.rope_models <- function(posterior, bounds = "default", ci = .90, verbose = TRUE) {
  if (all(bounds == "default")) {
    bounds <- c(-0.1 * sd(insight::get_response(posterior)), 0.1 * sd(insight::get_response(posterior)))
  } else if (!all(is.numeric(bounds)) | length(bounds) != 2) {
    stop("`bounds` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }
  list <- sapply(insight::get_parameters(posterior), rope, bounds = bounds, ci = ci, verbose = verbose, simplify = FALSE)
  return(flatten_list(list, name = "Parameter"))
}

#' @export
rope.stanreg <- .rope_models

#' @export
rope.brmsfit <- .rope_models
