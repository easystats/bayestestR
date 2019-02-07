#' Test for Practical Equivalence
#'
#' Perform a Test for Practical Equivalence based on the "HDI+ROPE decision rule" (Kruschke, 2018) to check whether parameter values should be accepted or rejected against an explicitly formulated "null hypothesis".
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
#' equivalence_test(posterior = rnorm(1000, 0, 0.01), bounds = c(-0.1, 0.1))
#' equivalence_test(posterior = rnorm(1000, 0, 1), bounds = c(-0.1, 0.1))
#' equivalence_test(posterior = rnorm(1000, 1, 0.01), bounds = c(-0.1, 0.1))
#' equivalence_test(posterior = rnorm(1000, 1, 1), CI = c(50, 99))
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' equivalence_test(model)
#' 
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' equivalence_test(model)
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
equivalence_test <- function(posterior, bounds = "default", CI = 90, verbose = TRUE) {
  UseMethod("equivalence_test")
}


#' @export
equivalence_test.numeric <- function(posterior, bounds = "default", CI = 90, verbose = TRUE) {
  rope_value <- rope(posterior, bounds = bounds, CI = CI)
  if ("rope" %in% class(rope_value)) {
    rope_value <- as_numeric_rope(rope_value)
  } else {
    rope_value <- sapply(rope_value, as_numeric_rope)
  }
  decision <- ifelse(rope_value == 0, "rejected",
    ifelse(rope_value == 100, "accepted", "undecided")
  )

  if (length(CI) > 1) {
    decision <- split(unname(decision), names(decision))
  }

  return(decision)
}


#' @export
equivalence_test.stanreg <- function(posterior, bounds = "default", CI = 90, verbose = TRUE) {
  return(sapply(as.data.frame(posterior), equivalence_test, bounds = bounds, CI = CI, verbose = verbose, simplify = FALSE))
}

#' @export
equivalence_test.brmsfit <- function(posterior, bounds = "default", CI = 90, verbose = TRUE) {
  return(sapply(as.data.frame(posterior), equivalence_test, bounds = bounds, CI = CI, verbose = verbose, simplify = FALSE))
}
