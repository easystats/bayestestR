#' Test for Practical Equivalence
#'
#' Perform a \strong{Test for Practical Equivalence} based on the \emph{"HDI+ROPE decision rule"} (Kruschke, 2018) to check whether parameter values should be accepted or rejected against an explicitly formulated "null hypothesis" (\emph{i.e.}, a \link[=rope]{ROPE}).
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
#'
#' @references Kruschke, J. K. (2018). Rejecting or Accepting Parameter Values in Bayesian Estimation. Advances in Methods and Practices in Psychological Science, 251524591877130. \doi{10.1177/2515245918771304}
#'
#' @examples
#' library(bayestestR)
#'
#' equivalence_test(x = rnorm(1000, 0, 0.01), range = c(-0.1, 0.1))
#' equivalence_test(x = rnorm(1000, 0, 1), range = c(-0.1, 0.1))
#' equivalence_test(x = rnorm(1000, 1, 0.01), range = c(-0.1, 0.1))
#' equivalence_test(x = rnorm(1000, 1, 1), ci = c(.50, .99))
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

  out$HDI_low <- sapply(attr(rope_data, "HDI_area", exact = TRUE), function(i) i[1])
  out$HDI_high <- sapply(attr(rope_data, "HDI_area", exact = TRUE), function(i) i[2])

  # remove attribute
  attr(out, "HDI_area") <- NULL

  class(out) <- c("equivalence_test", class(out))
  out
}


#' @importFrom stats sd
#' @keywords internal
.equivalence_test_models <- function(x, range = "default", ci = .95, parameters = NULL, verbose = TRUE) {
  if (all(range == "default")) {
    range <- rope_range(x)
  } else if (!all(is.numeric(range)) | length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

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

  class(out) <- c("equivalence_test", class(out))
  out
}


#' @rdname equivalence_test
#' @export
equivalence_test.stanreg <- function(x, range = "default", ci = .95, parameters = NULL, verbose = TRUE, ...) {
  et <- .equivalence_test_models(x, range, ci, parameters, verbose)
  attr(et, "model") <- deparse(substitute(x), width.cutoff = 500)
  et
}


#' @rdname equivalence_test
#' @export
equivalence_test.brmsfit <- function(x, range = "default", ci = .95, parameters = NULL, verbose = TRUE, ...) {
  et <- .equivalence_test_models(x, range, ci, parameters, verbose)
  attr(et, "model") <- deparse(substitute(x), width.cutoff = 500)
  et
}
