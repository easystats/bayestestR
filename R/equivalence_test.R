#' Test for Practical Equivalence
#'
#' Perform a \strong{Test for Practical Equivalence} for Bayesian and frequentist models.
#'
#' Documentation is accessible for:
#' \itemize{
#'   \item \href{https://easystats.github.io/bayestestR/reference/equivalence_test.html}{Bayesian models}
#'   \item \href{https://easystats.github.io/parameters/reference/equivalence_test.lm.html}{Frequentist models}
#' }
#'
#' For Bayesian models, the \strong{Test for Practical Equivalence} is based on the \emph{"HDI+ROPE decision rule"} (Kruschke, 2018) to check whether parameter values should be accepted or rejected against an explicitly formulated "null hypothesis" (\emph{i.e.}, a \link[=rope]{ROPE}).
#'
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
#'   reject the null hypothesis. If the full ROPE is used (\emph{i.e.}, 100\% of the
#'   HDI), then the null hypothesis is rejected or accepted if the percentage
#'   of the posterior within the ROPE is smaller than to 2.5\% or greater  than
#'   97.5\%. Desirable results are low proportions inside the ROPE  (the closer
#'   to zero the better) and the null hypothesis should be rejected.
#'   \cr \cr
#'   Some attention is required for finding suitable values for the ROPE limits
#'   (argument \code{range}). See 'Details' in \link{rope_range} for further
#'   information.
#'
#' @references Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. Advances in Methods and Practices in Psychological Science, 1(2), 270-280. \doi{10.1177/2515245918771304}.
#'
#' @return A data frame with following columns:
#'   \itemize{
#'     \item \code{Parameter} The model parameter(s), if \code{x} is a model-object. If \code{x} is a vector, this column is missing.
#'     \item \code{CI} The probability of the HDI.
#'     \item \code{ROPE_low}, \code{ROPE_high} The limits of the ROPE. These values are identical for all parameters.
#'     \item \code{ROPE_Percentage} The proportion of the HDI that lies inside the ROPE.
#'     \item \code{ROPE_Equivalence} The "test result", as character. Either "rejected", "accepted" or "undecided".
#'     \item \code{HDI_low} , \code{HDI_high} The lower and upper HDI limits for the parameters.
#'   }
#'
#' @note There is a \code{print()}-method with a \code{digits}-argument to control
#'   the amount of digits in the output, and there is a \code{plot()}-method
#'   to visualize the results from the equivalence-test (for models only).
#'
#' @examples
#' library(bayestestR)
#'
#' equivalence_test(x = rnorm(1000, 0, 0.01), range = c(-0.1, 0.1))
#' equivalence_test(x = rnorm(1000, 0, 1), range = c(-0.1, 0.1))
#' equivalence_test(x = rnorm(1000, 1, 0.01), range = c(-0.1, 0.1))
#' equivalence_test(x = rnorm(1000, 1, 1), ci = c(.50, .99))
#'
#' # print more digits
#' test <- equivalence_test(x = rnorm(1000, 1, 1), ci = c(.50, .99))
#' print(test, digits = 4)
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' equivalence_test(model)
#' equivalence_test(model, ci = c(.50, 1))
#'
#' # plot result
#' test <- equivalence_test(model)
#' plot(test)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' equivalence_test(model)
#' equivalence_test(model, ci = c(.50, .99))
#' }
#'
#' @importFrom insight print_color
#' @export
equivalence_test <- function(x, ...) {
  UseMethod("equivalence_test")
}


#' @rdname equivalence_test
#' @export
equivalence_test.numeric <- function(x, range = "default", ci = .95, verbose = TRUE, ...) {
  rope_data <- rope(x, range = range, ci = ci)
  out <- as.data.frame(rope_data)

  if (all(ci < 1)) {
    out$ROPE_Equivalence <- ifelse(out$ROPE_Percentage == 0, "rejected",
      ifelse(out$ROPE_Percentage == 100, "accepted", "undecided")
    )
  } else {
    # Related to guidelines for full rope (https://easystats.github.io/bayestestR/articles/4_Guidelines.html)
    out$ROPE_Equivalence <- ifelse(out$ROPE_Percentage < 2.5, "rejected",
      ifelse(out$ROPE_Percentage > 97.5, "accepted", "undecided")
    )
  }

  out$HDI_low <- attr(rope_data, "HDI_area", exact = TRUE)$CI_low
  out$HDI_high <- attr(rope_data, "HDI_area", exact = TRUE)$CI_high

  # remove attribute
  attr(out, "HDI_area") <- NULL
  attr(out, "data") <- x

  class(out) <- c("equivalence_test", "equivalence_test_see", class(out))
  out
}


#' @importFrom stats sd
#' @keywords internal
.equivalence_test_models <- function(x, range = "default", ci = .95, parameters = NULL, verbose = TRUE) {
  if (all(range == "default")) {
    range <- rope_range(x)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  if (verbose) .check_parameter_correlation(x)

  l <- sapply(
    insight::get_parameters(x, component = "conditional", parameters = parameters),
    equivalence_test,
    range = range,
    ci = ci,
    verbose = verbose,
    simplify = FALSE
  )

  dat <- do.call(rbind, l)
  out <- data.frame(
    Parameter = rep(names(l), each = nrow(dat) / length(l)),
    dat,
    stringsAsFactors = FALSE
  )

  class(out) <- c("equivalence_test", "equivalence_test_see", class(out))
  out
}


#' @rdname equivalence_test
#' @export
equivalence_test.stanreg <- function(x, range = "default", ci = .95, parameters = NULL, verbose = TRUE, ...) {
  out <- .equivalence_test_models(x, range, ci, parameters, verbose)
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @rdname equivalence_test
#' @export
equivalence_test.brmsfit <- function(x, range = "default", ci = .95, parameters = NULL, verbose = TRUE, ...) {
  out <- .equivalence_test_models(x, range, ci, parameters, verbose)
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @importFrom stats cor cor.test
#' @importFrom insight find_parameters
#' @keywords internal
.check_parameter_correlation <- function(model) {
  valid_parameters <- insight::find_parameters(model, parameters = "^(?!(r_|sd_|prior_|cor_|b\\[))", flatten = TRUE)
  dat <- as.data.frame(model)[, valid_parameters]
  dat <- dat[, -1, drop = FALSE]

  if (ncol(dat) > 1) {
    parameter_correlation <- stats::cor(dat)
    parameter <- expand.grid(colnames(dat), colnames(dat), stringsAsFactors = FALSE)

    results <- cbind(
      parameter,
      corr = abs(as.vector(expand.grid(parameter_correlation)[[1]])),
      pvalue = apply(parameter, 1, function(r) stats::cor.test(dat[[r[1]]], dat[[r[2]]])$p.value)
    )

    results <- results[results$pvalue < 0.05 & results$Var1 != results$Var2, ]

    if (nrow(results) > 0 && any(results$corr >= 0.5)) {
      warning("Some parameters show strong correlations. Note that joint parameter distributions may shift towards or away from the ROPE when covariates are not independent. Hence, a test for practical equivalence may have inappropriate results.", call. = FALSE)
    }
  }
}
