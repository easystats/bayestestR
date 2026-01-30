#' Bayes Factors (BF) for Order Restricted Models
#'
#' This method computes Bayes factors for comparing a model with an order restrictions on its parameters
#' with the fully unrestricted model. *Note that this method should only be used for confirmatory analyses*.
#' \cr\cr
#' The `bf_*` function is an alias of the main function.
#' \cr \cr
#' \strong{For more info, see [the Bayes factors vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).}
#'
#' @param posterior A `stanreg` / `brmsfit` object, `emmGrid` or a data frame - representing
#' a posterior distribution(s) from (see Details).
#' @param hypothesis A character vector specifying the restrictions as logical conditions (see examples below).
#' @param prior An object representing a prior distribution (see Details).
#' @inheritParams hdi
#'
#' @details This method is used to compute Bayes factors for order-restricted
#'   models vs un-restricted models by setting an order restriction on the prior
#'   and posterior distributions (\cite{Morey & Wagenmakers, 2013}).
#' \cr\cr
#' (Though it is possible to use `bayesfactor_restricted()` to test interval restrictions,
#' it is more suitable for testing order restrictions; see examples).
#'
#' ## Additional methods
#' The resulting output is supported by the following methods:
#'
#' - `as.matrix()`: Extract a full matrix of (log-)Bayes factors between all
#'   models (using the transitivity of Bayes factors).
#' - `as.logical()`: Extract boolean vectors indicating which (prior/posterior)
#'   samples are included in the hypothesized restriction.
#' - `as.numeric()`: Extract the (possibly log-)Bayes factor values.
#'
#' See examples and [bayesfactor_methods].
#'
#' @inheritSection bayesfactor_parameters Obtaining prior samples
#'
#' @inheritSection bayesfactor_methods Transitivity of Bayes factors
#'
#' @inheritSection bayesfactor_methods Interpreting Bayes Factors
#'
#' @return A data frame containing the (log) Bayes factor representing evidence
#'   *against* the un-restricted model (Use `as.numeric()` to extract the
#'   non-log Bayes factors; see examples). (A `bool_results` attribute contains
#'   the results for each sample, indicating if they are included or not in the
#'   hypothesized restriction.)
#'
#' @examples
#' set.seed(444)
#' library(bayestestR)
#' prior <- data.frame(
#'   A = rnorm(500),
#'   B = rnorm(500),
#'   C = rnorm(500)
#' )
#'
#' posterior <- data.frame(
#'   A = rnorm(500, .4, 0.7),
#'   B = rnorm(500, -.2, 0.4),
#'   C = rnorm(500, 0, 0.5)
#' )
#'
#' hyps <- c(
#'   "A > B & B > C",
#'   "A > B & A > C",
#'   "C > A"
#' )
#'
#'
#' (b <- bayesfactor_restricted(posterior, hypothesis = hyps, prior = prior))
#'
#' # See the matrix of BFs
#' as.matrix(b)
#'
#' bool <- as.logical(b, which = "posterior")
#' head(bool)
#'
#' @examplesIf require("see") && require("patchwork")
#'
#' see::plots(
#'   plot(estimate_density(posterior)),
#'   # distribution **conditional** on the restrictions
#'   plot(estimate_density(posterior[bool[, hyps[1]], ])) + ggplot2::ggtitle(hyps[1]),
#'   plot(estimate_density(posterior[bool[, hyps[2]], ])) + ggplot2::ggtitle(hyps[2]),
#'   plot(estimate_density(posterior[bool[, hyps[3]], ])) + ggplot2::ggtitle(hyps[3]),
#'   guides = "collect"
#' )
#'
#' @examplesIf require("rstanarm")
#' \donttest{
#' # rstanarm models
#' # ---------------
#' data("mtcars")
#'
#' fit_stan <- rstanarm::stan_glm(mpg ~ wt + cyl + am,
#'   data = mtcars, refresh = 0
#' )
#' hyps <- c(
#'   "am > 0 & cyl < 0",
#'   "cyl < 0",
#'   "wt - cyl > 0"
#' )
#'
#' bayesfactor_restricted(fit_stan, hypothesis = hyps)
#' }
#'
#' @examplesIf require("rstanarm") && require("emmeans")
#' \donttest{
#' # emmGrid objects
#' # ---------------
#' # replicating http://bayesfactor.blogspot.com/2015/01/multiple-comparisons-with-bayesfactor-2.html
#' data("disgust")
#' contrasts(disgust$condition) <- contr.equalprior_pairs # see vignette
#' fit_model <- rstanarm::stan_glm(score ~ condition, data = disgust, family = gaussian())
#'
#' em_condition <- emmeans::emmeans(fit_model, ~condition, data = disgust)
#' hyps <- c("lemon < control & control < sulfur")
#'
#' bayesfactor_restricted(em_condition, prior = fit_model, hypothesis = hyps)
#' # > # Bayes Factor (Order-Restriction)
#' # >
#' # >                          Hypothesis P(Prior) P(Posterior)   BF
#' # >  lemon < control & control < sulfur     0.17         0.75 4.49
#' # > ---
#' # > Bayes factors for the restricted model vs. the un-restricted model.
#' }
#'
#' @references
#' - Morey, R. D., & Wagenmakers, E. J. (2014). Simple relation between Bayesian order-restricted and
#' point-null hypothesis tests. Statistics & Probability Letters, 92, 121-124.
#' - Morey, R. D., & Rouder, J. N. (2011). Bayes factor approaches for testing interval null hypotheses.
#' Psychological methods, 16(4), 406.
#' - Morey, R. D. (Jan, 2015). Multiple Comparisons with BayesFactor, Part 2 – order restrictions.
#' Retrieved from https://richarddmorey.org/category/order-restrictions/.
#'
#' @family Bayes factors
#'
#' @export
bayesfactor_restricted <- function(posterior, ...) {
  UseMethod("bayesfactor_restricted")
}

#' @rdname bayesfactor_restricted
#' @export
bf_restricted <- bayesfactor_restricted

#' @rdname bayesfactor_restricted
#' @export
bayesfactor_restricted.stanreg <- function(posterior, hypothesis, prior = NULL,
                                           verbose = TRUE,
                                           effects = "fixed",
                                           component = "conditional",
                                           ...) {
  samps <- .clean_priors_and_posteriors(posterior, prior,
    effects = effects, component = component,
    verbose = verbose
  )

  # Get savage-dickey BFs
  bayesfactor_restricted.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    hypothesis = hypothesis
  )
}

#' @rdname bayesfactor_restricted
#' @export
bayesfactor_restricted.brmsfit <- bayesfactor_restricted.stanreg

#' @export
bayesfactor_restricted.CmdStanFit <- bayesfactor_restricted.stanreg

#' @export
bayesfactor_restricted.stanfit <- bayesfactor_restricted.stanreg

#' @rdname bayesfactor_restricted
#' @export
bayesfactor_restricted.blavaan <- function(posterior, hypothesis, prior = NULL,
                                           verbose = TRUE, ...) {
  samps <- .clean_priors_and_posteriors(posterior, prior,
    verbose = verbose
  )

  # Get savage-dickey BFs
  bayesfactor_restricted.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    hypothesis = hypothesis
  )
}


#' @rdname bayesfactor_restricted
#' @export
bayesfactor_restricted.emmGrid <- function(posterior, hypothesis, prior = NULL,
                                           verbose = TRUE,
                                           ...) {
  samps <- .clean_priors_and_posteriors(posterior, prior,
    verbose = verbose
  )

  bayesfactor_restricted.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    hypothesis = hypothesis
  )
}

#' @export
bayesfactor_restricted.emm_list <- bayesfactor_restricted.emmGrid

#' @export
bayesfactor_restricted.slopes <- bayesfactor_restricted.emmGrid

#' @export
bayesfactor_restricted.predictions <- bayesfactor_restricted.emmGrid

#' @export
bayesfactor_restricted.comparisons <- bayesfactor_restricted.emmGrid

#' @export
#' @rdname bayesfactor_restricted
#' @inheritParams p_direction
bayesfactor_restricted.data.frame <- function(posterior, hypothesis, prior = NULL, rvar_col = NULL, ...) {
  x_rvar <- .possibly_extract_rvar_col(posterior, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::bayesfactor_restricted
    cl$posterior <- x_rvar
    cl$rvar_col <- NULL
    prior_rvar <- .possibly_extract_rvar_col(posterior, prior)
    if (length(prior_rvar) > 0L) {
      cl$prior <- prior_rvar
    }
    return(eval.parent(cl))
  }


  p_hypothesis <- parse(text = hypothesis)

  if (is.null(prior)) {
    prior <- posterior
    insight::format_warning(
      "Prior not specified! ",
      "Please specify priors (with column names matching 'posterior')",
      " to get meaningful results."
    )
  }

  .test_hypothesis <- function(x, data) {
    x_logical <- try(eval(x, envir = data), silent = TRUE)
    if (inherits(x_logical, "try-error")) {
      cnames <- colnames(data)
      is_name <- make.names(cnames) == cnames
      cnames[!is_name] <- paste0("`", cnames[!is_name], "`")
      insight::format_error(
        x_logical,
        paste("Available parameters are:", toString(cnames))
      )
    } else if (!all(is.logical(x_logical))) {
      insight::format_error("Hypotheses must be logical.")
    }
    x_logical
  }


  posterior_l <- as.data.frame(lapply(p_hypothesis, .test_hypothesis, data = posterior))
  prior_l <- as.data.frame(lapply(p_hypothesis, .test_hypothesis, data = prior))
  colnames(posterior_l) <- colnames(prior_l) <- if (is.null(names(hypothesis))) hypothesis else names(hypothesis)

  posterior_p <- sapply(posterior_l, mean)
  prior_p <- sapply(prior_l, mean)
  log_BF <- log(posterior_p) - log(prior_p)

  res <- data.frame(
    Hypothesis = hypothesis,
    p_prior = prior_p,
    p_posterior = posterior_p,
    log_BF = log_BF
  )

  attr(res, "bool_results") <- list(posterior = posterior_l, prior = prior_l)
  class(res) <- unique(c(
    "bayestestRBF",
    "bayesfactor_restricted",
    class(res)
  ))

  res
}


#' @export
bayesfactor_restricted.draws <- function(posterior, hypothesis, prior = NULL, ...) {
  bayesfactor_restricted(.posterior_draws_to_df(posterior),
    hypothesis = hypothesis,
    prior = if (!is.null(prior)) .posterior_draws_to_df(prior),
    ...
  )
}

#' @export
bayesfactor_restricted.rvar <- bayesfactor_restricted.draws
