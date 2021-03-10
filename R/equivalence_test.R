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
#' For Bayesian models, the \strong{Test for Practical Equivalence} is based on the \emph{"HDI+ROPE decision rule"} (\cite{Kruschke, 2014, 2018}) to check whether parameter values should be accepted or rejected against an explicitly formulated "null hypothesis" (i.e., a ROPE). In other words, it checks the percentage of the 89\% \link[=hdi]{HDI} that is the null region (the ROPE). If this percentage is sufficiently low, the null hypothesis is rejected. If this percentage is sufficiently high, the null hypothesis is accepted.
#'
#'
#' @inheritParams rope
#'
#' @details Using the \link[=rope]{ROPE} and the \link[=hdi]{HDI}, \cite{Kruschke (2018)}
#'   suggests using the percentage of the 95\% (or 89\%, considered more stable)
#'   HDI that falls within the ROPE as a decision rule. If the HDI
#'   is completely outside the ROPE, the "null hypothesis" for this parameter is
#'   "rejected". If the ROPE completely covers the HDI, i.e., all most credible
#'   values of a parameter are inside the region of practical equivalence, the
#'   null hypothesis is accepted. Else, it’s undecided whether to accept or
#'   reject the null hypothesis. If the full ROPE is used (i.e., 100\% of the
#'   HDI), then the null hypothesis is rejected or accepted if the percentage
#'   of the posterior within the ROPE is smaller than to 2.5\% or greater than
#'   97.5\%. Desirable results are low proportions inside the ROPE  (the closer
#'   to zero the better).
#'   \cr \cr
#'   Some attention is required for finding suitable values for the ROPE limits
#'   (argument \code{range}). See 'Details' in \code{\link[=rope_range]{rope_range()}}
#'   for further information.
#'   \cr \cr
#'   \strong{Multicollinearity: Non-independent covariates}
#'   \cr \cr
#'   When parameters show strong correlations, i.e. when covariates are not
#'   independent, the joint parameter distributions may shift towards or
#'   away from the ROPE. In such cases, the test for practical equivalence may
#'   have inappropriate results. Collinearity invalidates ROPE and hypothesis
#'   testing based on univariate marginals, as the probabilities are conditional
#'   on independence. Most problematic are the results of the "undecided"
#'   parameters, which may either move further towards "rejection" or away
#'   from it (\cite{Kruschke 2014, 340f}).
#'   \cr \cr
#'   \code{equivalence_test()} performs a simple check for pairwise correlations
#'   between parameters, but as there can be collinearity between more than two variables,
#'   a first step to check the assumptions of this hypothesis testing is to look
#'   at different pair plots. An even more sophisticated check is the projection
#'   predictive variable selection (\cite{Piironen and Vehtari 2017}).
#'
#'
#' @references \itemize{
#'   \item Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. Advances in Methods and Practices in Psychological Science, 1(2), 270-280. \doi{10.1177/2515245918771304}
#'   \item Kruschke, J. K. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press
#'   \item Piironen, J., & Vehtari, A. (2017). Comparison of Bayesian predictive methods for model selection. Statistics and Computing, 27(3), 711–735. \doi{10.1007/s11222-016-9649-y}
#' }
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
#'   the amount of digits in the output, and there is a
#'   \href{https://easystats.github.io/see/articles/bayestestR.html}{\code{plot()}-method}
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
#' library(emmeans)
#' equivalence_test(emtrends(model, ~1, "wt"))
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' equivalence_test(model)
#' equivalence_test(model, ci = c(.50, .99))
#'
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' equivalence_test(bf)
#' equivalence_test(bf, ci = c(.50, .99))
#' }
#'
#' @importFrom insight print_color
#' @export
equivalence_test <- function(x, ...) {
  UseMethod("equivalence_test")
}



#' @rdname equivalence_test
#' @export
equivalence_test.default <- function(x, ...) {
  NULL
}




#' @rdname equivalence_test
#' @export
equivalence_test.numeric <- function(x, range = "default", ci = .89, verbose = TRUE, ...) {
  rope_data <- rope(x, range = range, ci = ci)
  out <- as.data.frame(rope_data)

  if (all(ci < 1)) {
    out$ROPE_Equivalence <- ifelse(out$ROPE_Percentage == 0, "Rejected",
      ifelse(out$ROPE_Percentage == 1, "Accepted", "Undecided")
    )
  } else {
    # Related to guidelines for full rope (https://easystats.github.io/bayestestR/articles/4_Guidelines.html)
    out$ROPE_Equivalence <- ifelse(out$ROPE_Percentage < 0.025, "Rejected",
      ifelse(out$ROPE_Percentage > 0.975, "Accepted", "Undecided")
    )
  }

  out$HDI_low <- attr(rope_data, "HDI_area", exact = TRUE)$CI_low
  out$HDI_high <- attr(rope_data, "HDI_area", exact = TRUE)$CI_high

  # remove attribute
  attr(out, "HDI_area") <- NULL
  attr(out, "data") <- x

  class(out) <- unique(c("equivalence_test", "see_equivalence_test", class(out)))
  out
}



#' @rdname equivalence_test
#' @export
equivalence_test.data.frame <- function(x, range = "default", ci = .89, verbose = TRUE, ...) {
  l <- .compact_list(lapply(
    x,
    equivalence_test,
    range = range,
    ci = ci,
    verbose = verbose
  ))

  dat <- do.call(rbind, l)
  out <- data.frame(
    Parameter = rep(names(l), each = nrow(dat) / length(l)),
    dat,
    stringsAsFactors = FALSE
  )
  row.names(out) <- NULL

  attr(out, "object_name") <- .safe_deparse(substitute(x))
  class(out) <- unique(c("equivalence_test", "see_equivalence_test_df", class(out)))

  out
}

#' @rdname equivalence_test
#' @export
equivalence_test.emmGrid <- function(x, range = "default", ci = .89, verbose = TRUE, ...) {
  xdf <- insight::get_parameters(x)

  out <- equivalence_test(xdf, range = range, ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
equivalence_test.emm_list <- equivalence_test.emmGrid


#' @rdname equivalence_test
#' @export
equivalence_test.BFBayesFactor <- function(x, range = "default", ci = .89, verbose = TRUE, ...) {
  out <- equivalence_test(insight::get_parameters(x), range = range, ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}




#' @importFrom stats sd
#' @keywords internal
.equivalence_test_models <- function(x, range = "default", ci = .89, effects = "fixed", component = "conditional", parameters = NULL, verbose = TRUE) {
  if (all(range == "default")) {
    range <- rope_range(x)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  if (verbose && !inherits(x, "blavaan")) .check_multicollinearity(x)
  params <- insight::get_parameters(x, component = component, effects = effects, parameters = parameters)

  l <- sapply(
    params,
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

  class(out) <- unique(c("equivalence_test", "see_equivalence_test", class(out)))
  out
}


#' @rdname equivalence_test
#' @export
equivalence_test.stanreg <- function(x, range = "default", ci = .89, effects = c("fixed", "random", "all"), component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .equivalence_test_models(x, range, ci, effects, component, parameters, verbose)

  out <- .prepare_output(
    out,
    insight::clean_parameters(x),
    inherits(x, "stanmvreg")
  )

  class(out) <- unique(c("equivalence_test", "see_equivalence_test", class(out)))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}


#' @export
equivalence_test.stanfit <- equivalence_test.stanreg

#' @export
equivalence_test.blavaan <- equivalence_test.stanreg


#' @rdname equivalence_test
#' @export
equivalence_test.brmsfit <- function(x, range = "default", ci = .89, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .equivalence_test_models(x, range, ci, effects, component, parameters, verbose)

  out <- .prepare_output(
    out,
    insight::clean_parameters(x),
    inherits(x, "stanmvreg")
  )

  class(out) <- unique(c("equivalence_test", "see_equivalence_test", class(out)))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}



#' @export
equivalence_test.sim.merMod <- function(x, range = "default", ci = .89, parameters = NULL, verbose = TRUE, ...) {
  out <- .equivalence_test_models(x, range, ci, effects = "fixed", component = "conditional", parameters, verbose = FALSE)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
equivalence_test.sim <- equivalence_test.sim.merMod


#' @export
equivalence_test.mcmc <- function(x, range = "default", ci = .89, parameters = NULL, verbose = TRUE, ...) {
  out <- .equivalence_test_models(as.data.frame(x), range, ci, effects = "fixed", component = "conditional", parameters, verbose = FALSE)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}


#' @export
equivalence_test.bcplm <- function(x, range = "default", ci = .89, parameters = NULL, verbose = TRUE, ...) {
  out <- .equivalence_test_models(insight::get_parameters(x), range, ci, effects = "fixed", component = "conditional", parameters, verbose = FALSE)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
equivalence_test.blrm <- equivalence_test.bcplm

#' @export
equivalence_test.mcmc.list <- equivalence_test.bcplm

#' @export
equivalence_test.bayesQR <- equivalence_test.bcplm



#' @export
equivalence_test.bamlss <- function(x, range = "default", ci = .89, component = c("all", "conditional", "location"), parameters = NULL, verbose = TRUE, ...) {
  component <- match.arg(component)
  out <- .equivalence_test_models(insight::get_parameters(x, component = component), range, ci, effects = "fixed", component = "conditional", parameters, verbose = FALSE)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}
