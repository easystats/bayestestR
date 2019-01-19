#' Region of Practical Equivalence (ROPE)
#'
#' Compute the proportion of the HDI of a posterior distribution that lies within a region of practical equivalence.
#'
#' @param posterior vector representing a posterior distribution.
#' @param bounds ROPE's lower and higher bounds.
#' @param CI the credible interval to use.
#' @param verbose toggle off warnings.
#'
#' @details Statistically, the probability of a posterior distribution of being different from 0 does not make much sense (the probability of it being different from a single point being infinite). Therefore, the idea underlining ROPE is to let the user define an area around the null value enclosing values that are equivalent to the null value for practical purposes (2010, 2011, 2014). Kruschke (2018) suggests that such null value could be set, by default, to the -0.1 to 0.1 range of a standardized parameter (negligible effect size according to Cohen). This could be generalized: For instance, for linear models, the ROPE could be set as 0 +/- .1 * sd(y). Kruschke (2010, 2011, 2014) suggest using the proportion of the 95\% (or 90\%, considered more stable) \link[=hdi]{HDI} that falls within the ROPE as an index for "null-hypothesis" testing (as understood under the Bayesian framework, see \link[=rope_test]{rope_test}). Besides the ROPE-based decisions criteria, the proportion of the 95\% CI that falls in the ROPE can be used as a continuous index, which low values are desirable.
#'
#' @examples
#' library(bayestestR)
#'
#' rope(posterior = rnorm(1000, 0, 0.01), bounds = c(-0.1, 0.1))
#' rope(posterior = rnorm(1000, 0, 1), bounds = c(-0.1, 0.1))
#' rope(posterior = rnorm(1000, 1, 0.01), bounds = c(-0.1, 0.1))
#' rope(posterior = rnorm(1000, 1, 1), CI = c(90, 95))
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
rope <- function(posterior, bounds = c(-0.1, 0.1), CI = 90, verbose=TRUE) {
  if (length(CI) > 1) {
    rope_values <- list()
    for (CI_value in CI) {
      rope_values[[paste0("CI_", CI_value)]] <- .rope(posterior, bounds=bounds, CI=CI_value, verbose=verbose)
    }
    return(rope_values)
  } else {
    return(.rope(posterior, bounds=bounds, CI=CI, verbose=verbose))
  }
}



.rope <- function(posterior, bounds = c(-0.1, 0.1), CI = 90, verbose=TRUE) {
  HDI_area <- hdi(posterior, CI, verbose)

  if(anyNA(HDI_area)){
    return(NA)
  }

  HDI_area <- posterior[posterior >= HDI_area[1] & posterior <= HDI_area[2]]
  area_within <- HDI_area[HDI_area >= min(bounds) & HDI_area <= max(bounds)]

  return(length(area_within) / length(HDI_area))
}




#' Test for Practical Equivalence
#'
#' Perform a Test for Practical Equivalence based on the "HDI+ROPE decision rule" (Kruschke 2018) to check whether parameter values should be accepted or rejected against an explicitely formulated "null hypothesis".
#'
#' @inheritParams rope
#'
#' @details Using the \link[=rope]{ROPE} and the \link[=hdi]{HDI}, Kruschke (2010, 2011, 2014, 2018) suggest using the percentage of the 95\% (or 90\%, considered more stable) \link[=hdi]{HDI} that falls within the ROPE as a decision rule. If the HDI is completely outside the ROPE, the "null hypothesis" for this parameter is "rejected". If the ROPE completely covers the HDI, i.e. all most credible values of a parameter are inside the region of practical equivalence, the null hypothesis is accepted. Else, it’s undecided whether to accept or reject the null hypothesis.
#'
#' @references \href{https://strengejacke.wordpress.com/2018/06/06/r-functions-for-bayesian-model-statistics-and-summaries-rstats-stan-brms/}{sjstats}
#'
#'
#' @examples
#' library(bayestestR)
#'
#' rope_test(posterior = rnorm(1000, 0, 0.01), bounds = c(-0.1, 0.1))
#' rope_test(posterior = rnorm(1000, 0, 1), bounds = c(-0.1, 0.1))
#' rope_test(posterior = rnorm(1000, 1, 0.01), bounds = c(-0.1, 0.1))
#' rope_test(posterior = rnorm(1000, 1, 1), CI = c(50, 99))
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
rope_test <- function(posterior, bounds = c(-0.1, 0.1), CI = 90, verbose=TRUE) {
  rope_value <- rope(posterior, bounds=bounds, CI=CI)
  decision <- ifelse(rope_value == 0, "rejected",
    ifelse(rope_value == 1, "accepted", "undecided")
  )

  if(length(CI) > 1){
    decision <- split(unname(decision),names(decision))
  }

  return(decision)
}

