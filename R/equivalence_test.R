#' Test for Practical Equivalence
#'
#' Perform a Test for Practical Equivalence based on the "HDI+ROPE decision rule" (Kruschke, 2018) to check whether parameter values should be accepted or rejected against an explicitly formulated "null hypothesis".
#'
#' @inheritParams rope
#'
#' @details Using the \link[=rope]{ROPE} and the \link[=hdi]{HDI}, Kruschke (2010, 2011, 2014, 2018) suggest using the percentage of the 95\% (or 90\%, considered more stable) \link[=hdi]{HDI} that falls within the ROPE as a decision rule. If the HDI is completely outside the ROPE, the "null hypothesis" for this parameter is "rejected". If the ROPE completely covers the HDI, i.e. all most credible values of a parameter are inside the region of practical equivalence, the null hypothesis is accepted. Else, it’s undecided whether to accept or reject the null hypothesis. If the full ROPE is used (i.e., 100\% of the HDI), then the null hypothesis is rejected or accepted if the percentage of the posterior within the ROPE is smaller than to 1\% or greater than 99\%.
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
#' equivalence_test(posterior = rnorm(1000, 1, 1), ci = c(.50, .99))
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' equivalence_test(model)
#' equivalence_test(model, ci = c(.50, 1))
#' 
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' equivalence_test(model)
#' equivalence_test(model, ci = c(.50, .99))
#' }
#' 
#' @export
equivalence_test <- function(posterior, bounds = "default", ci = .90, verbose = TRUE) {
  UseMethod("equivalence_test")
}


#' @export
equivalence_test.numeric <- function(posterior, bounds = "default", ci = .90, verbose = TRUE) {
  out <- as.data.frame(rope(posterior, bounds = bounds, ci = ci))

  if (ci < 1) {
    out$ROPE_Equivalence <- ifelse(out$ROPE_Percentage == 0, "rejected",
      ifelse(out$ROPE_Percentage == 100, "accepted", "undecided")
    )
  } else {
    out$ROPE_Equivalence <- ifelse(out$ROPE_Percentage < 1, "rejected",
      ifelse(out$ROPE_Percentage > 99, "accepted", "undecided")
    )
  }
  return(out)
}




#' @importFrom stats sd
#' @keywords internal
.equivalence_test_models <- function(posterior, bounds = "default", ci = .90, verbose = TRUE) {
  if (all(bounds == "default")) {
    bounds <- c(-0.1 * sd(insight::get_response(posterior)), 0.1 * sd(insight::get_response(posterior)))
  } else if (!all(is.numeric(bounds)) | length(bounds) != 2) {
    stop("`bounds` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }
  l <- sapply(get_parameters(posterior), equivalence_test, bounds = bounds, ci = ci, verbose = verbose, simplify = FALSE)
  return(flatten_list(l, name = "Parameter"))
}

#' @export
equivalence_test.stanreg <- .equivalence_test_models

#' @export
equivalence_test.brmsfit <- .equivalence_test_models
