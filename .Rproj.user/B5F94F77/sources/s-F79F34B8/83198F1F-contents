#' Region of Practical Equivalence (ROPE)
#'
#' Compute the proportion (in percentage) of the HDI (default to the 90\% HDI) of a posterior distribution that lies within a region of practical equivalence.
#'
#' @param posterior vector representing a posterior distribution. Can also be a `stanreg` or `brmsfit` model.
#' @param bounds ROPE's lower and higher bounds.
#' @param CI The credible interval to use.
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
#' rope(posterior = rnorm(1000, 1, 1), CI = c(90, 95))
#'
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' rope(model)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' rope(model)
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
rope <- function(posterior, bounds = c(-0.1, 0.1), CI = 90, verbose = TRUE) {
  UseMethod("rope")
}


#' @export
print.rope <- function(x, ...){
  cat(sprintf("%.2f%% in ROPE", x))
}


#' @export
rope.numeric <- function(posterior, bounds = c(-0.1, 0.1), CI = 90, verbose = TRUE) {
  if (length(CI) > 1) {
    rope_values <- list()
    for (CI_value in CI) {
      rope_values[[paste0("CI_", CI_value)]] <- .rope(posterior, bounds = bounds, CI = CI_value, verbose = verbose)
    }
    return(rope_values)
  } else {
    return(.rope(posterior, bounds = bounds, CI = CI, verbose = verbose))
  }
}



.rope <- function(posterior, bounds = c(-0.1, 0.1), CI = 90, verbose = TRUE) {
  HDI_area <- hdi(posterior, CI, verbose)

  if (anyNA(HDI_area)) {
    return(NA)
  }

  HDI_area <- posterior[posterior >= HDI_area[1] & posterior <= HDI_area[2]]
  area_within <- HDI_area[HDI_area >= min(bounds) & HDI_area <= max(bounds)]

  rope <- length(area_within) / length(HDI_area) * 100
  class(rope) <- c("rope", class(rope))

  return(rope)
}


#' @export
rope.stanreg <- function(posterior, bounds = c(-0.1, 0.1), CI = 90, verbose = TRUE) {
  return(sapply(as.data.frame(posterior), rope, bounds=bounds, CI=CI, verbose=verbose, simplify = FALSE))
}

#' @export
rope.brmsfit <- function(posterior, bounds = c(-0.1, 0.1), CI = 90, verbose = TRUE) {
  return(sapply(as.data.frame(posterior), rope, bounds=bounds, CI=CI, verbose=verbose, simplify = FALSE))
}
