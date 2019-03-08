#' Test for Practical Equivalence
#'
#' Perform a Test for Practical Equivalence based on the "HDI+ROPE decision rule" (Kruschke, 2018) to check whether parameter values should be accepted or rejected against an explicitly formulated "null hypothesis".
#'
#' @inheritParams rope
#'
#' @details Using the \link[=rope]{ROPE} and the \link[=hdi]{HDI}, Kruschke (2010, 2011, 2014, 2018) suggest using the percentage of the 95\% (or 90\%, considered more stable) \link[=hdi]{HDI} that falls within the ROPE as a decision rule. If the HDI is completely outside the ROPE, the "null hypothesis" for this parameter is "rejected". If the ROPE completely covers the HDI, i.e. all most credible values of a parameter are inside the region of practical equivalence, the null hypothesis is accepted. Else, it’s undecided whether to accept or reject the null hypothesis. If the full ROPE is used (i.e., 100\% of the HDI), then the null hypothesis is rejected or accepted if the percentage of the posterior within the ROPE is smaller than to 1\% or greater than 99\%.
#'
#' @references Kruschke JK. Rejecting or Accepting Parameter Values in Bayesian Estimation. Advances in Methods and Practices in Psychological Science. 2018; doi: \doi{10.1177/2515245918771304}
#'
#'
#' @examples
#' library(bayestestR)
#'
#' equivalence_test(posterior = rnorm(1000, 0, 0.01), range = c(-0.1, 0.1))
#' equivalence_test(posterior = rnorm(1000, 0, 1), range = c(-0.1, 0.1))
#' equivalence_test(posterior = rnorm(1000, 1, 0.01), range = c(-0.1, 0.1))
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
equivalence_test <- function(posterior, range = "default", ci = .95, verbose = TRUE) {
  UseMethod("equivalence_test")
}


#' @export
equivalence_test.numeric <- function(posterior, range = "default", ci = .95, verbose = TRUE) {
  # if (length(ci) > 1) {
  #   stop("'ci' needs be a single value.", call. = FALSE)
  # }

  rope_data <- rope(posterior, range = range, ci = ci)
  out <- as.data.frame(rope_data)

  if (ci < 1) {
    out$ROPE_Equivalence <- ifelse(out$ROPE_Percentage == 0, "rejected",
      ifelse(out$ROPE_Percentage == 100, "accepted", "undecided")
    )
  } else {
    out$ROPE_Equivalence <- ifelse(out$ROPE_Percentage < 1, "rejected",
      ifelse(out$ROPE_Percentage > 99, "accepted", "undecided")
    )
  }

  out$HDI_low <- attr(rope_data, "HDI_area", exact = TRUE)[1]
  out$HDI_high <- attr(rope_data, "HDI_area", exact = TRUE)[2]

  class(out) <- c("equivalence_test", class(out))
  out
}


#' @export
print.equivalence_test <- function(x, ...) {
  cat("# Test for Practical Equivalence\n\n")
  cat(sprintf("  ROPE: [%.2f %.2f]\n\n", x$ROPE_low[1], x$ROPE_high[1]))
  ci <- x$CI[1]

  x$ROPE_Percentage <- sprintf("%.2f%%", x$ROPE_Percentage)
  x$HDI <- sprintf("[%.2f %.2f]", x$HDI_low, x$HDI_high)

  x <- x[, intersect(c("Parameter", "ROPE_Equivalence", "ROPE_Percentage", "HDI"), colnames(x))]

  colnames(x)[which(colnames(x) == "ROPE_Equivalence")] <- "H0"
  colnames(x)[which(colnames(x) == "ROPE_Percentage")] <- "Inside ROPE"
  colnames(x)[ncol(x)] <- sprintf("%i%% HDI", ci)

  print.data.frame(x, digits = 3, row.names = FALSE)
}


#' @importFrom stats sd
#' @keywords internal
.equivalence_test_models <- function(posterior, range = "default", ci = .95, verbose = TRUE) {
  if (all(range == "default")) {
    range <- rope_bounds(posterior)
  } else if (!all(is.numeric(range)) | length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }
  l <- sapply(get_parameters(posterior), equivalence_test, range = range, ci = ci, verbose = verbose, simplify = FALSE)
  flatten_list(l, name = "Parameter")
}

#' @export
equivalence_test.stanreg <- .equivalence_test_models

#' @export
equivalence_test.brmsfit <- .equivalence_test_models
