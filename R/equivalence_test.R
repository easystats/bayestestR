#' Test for Practical Equivalence
#'
#' Perform a Test for Practical Equivalence based on the "HDI+ROPE decision rule" (Kruschke, 2018) to check whether parameter values should be accepted or rejected against an explicitly formulated "null hypothesis".
#'
#' @inheritParams rope
#'
#' @details Using the \link[=rope]{ROPE} and the \link[=hdi]{HDI}, Kruschke (2018)
#'   suggest using the percentage of the 95\% (or 90\%, considered more stable)
#'   \link[=hdi]{HDI} that falls within the ROPE as a decision rule. If the HDI
#'   is completely outside the ROPE, the "null hypothesis" for this parameter is
#'   "rejected". If the ROPE completely covers the HDI, i.e. all most credible
#'   values of a parameter are inside the region of practical equivalence, the
#'   null hypothesis is accepted. Else, it’s undecided whether to accept or
#'   reject the null hypothesis. If the full ROPE is used (i.e., 100\% of the
#'   HDI), then the null hypothesis is rejected or accepted if the percentage
#'   of the posterior within the ROPE is smaller than to 1\% or greater  than
#'   99\%. Desirable results are low proportions inside the ROPE  (the closer
#'   to zero the better) and the H0 should be rejected.
#'
#' @references Kruschke JK. Rejecting or Accepting Parameter Values in Bayesian Estimation. Advances in Methods and Practices in Psychological Science. 2018; \doi{10.1177/2515245918771304}
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
  rope_data <- rope(posterior, range = range, ci = ci)
  out <- as.data.frame(rope_data)

  if (all(ci < 1)) {
    out$ROPE_Equivalence <- ifelse(out$ROPE_Percentage == 0, "rejected",
      ifelse(out$ROPE_Percentage == 100, "accepted", "undecided")
    )
  } else {
    out$ROPE_Equivalence <- ifelse(out$ROPE_Percentage < 1, "rejected",
      ifelse(out$ROPE_Percentage > 99, "accepted", "undecided")
    )
  }

  out$HDI_low <- sapply(attr(rope_data, "HDI_area", exact = TRUE), function(i) i[1])
  out$HDI_high <- sapply(attr(rope_data, "HDI_area", exact = TRUE), function(i) i[2])

  # remove attribute
  attr(out, "HDI_area") <- NULL

  class(out) <- c("equivalence_test", class(out))
  out
}


#' @export
print.equivalence_test <- function(x, ...) {
  cat("# Test for Practical Equivalence\n\n")
  cat(sprintf("  ROPE: [%.2f %.2f]\n\n", x$ROPE_low[1], x$ROPE_high[1]))

  x$ROPE_Percentage <- sprintf("%.2f%%", x$ROPE_Percentage)
  x$HDI <- sprintf("[%.2f %.2f]", x$HDI_low, x$HDI_high)

  ci <- unique(x$CI)
  keep.columns <- c("CI", "Parameter", "ROPE_Equivalence", "ROPE_Percentage", "HDI")

  x <- x[, intersect(keep.columns, colnames(x))]

  colnames(x)[which(colnames(x) == "ROPE_Equivalence")] <- "H0"
  colnames(x)[which(colnames(x) == "ROPE_Percentage")] <- "% inside ROPE"

  for (i in ci) {
    xsub <- x[x$CI == i, -which(colnames(x) == "CI")]
    colnames(xsub)[ncol(xsub)] <- sprintf("%i%% HDI", i)
    print.data.frame(xsub, digits = 3, row.names = FALSE)
    cat("\n")
  }
}


#' @importFrom stats sd
#' @keywords internal
.equivalence_test_models <- function(posterior, range = "default", ci = .95, verbose = TRUE) {
  if (all(range == "default")) {
    range <- rope_range(posterior)
  } else if (!all(is.numeric(range)) | length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  l <- sapply(get_parameters(posterior), equivalence_test, range = range, ci = ci, verbose = verbose, simplify = FALSE)
  out <- flatten_list(l, name = "Parameter")

  .clean_parameters(out)
}

#' @export
equivalence_test.stanreg <- function(posterior, range = "default", ci = .95, verbose = TRUE) {
  et <- .equivalence_test_models(posterior, range, ci, verbose)
  attr(et, "model") <- deparse(substitute(posterior), width.cutoff = 500)
  et
}

#' @export
equivalence_test.brmsfit <- function(posterior, range = "default", ci = .95, verbose = TRUE) {
  et <- .equivalence_test_models(posterior, range, ci, verbose)
  attr(et, "model") <- deparse(substitute(posterior), width.cutoff = 500)
  et
}
