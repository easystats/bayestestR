#' Bayes Factors (BF) for a Single Parameter
#'
#' This method computes Bayes factors against the null (either a point or an interval),
#' based on prior and posterior samples of a single parameter. This Bayes factor indicates
#' the degree by which the mass of the posterior distribution has shifted further away
#' from or closer to the null value(s) (relative to the prior distribution), thus indicating
#' if the null value has become less or more likely given the observed data.
#' \cr \cr
#' When the null is an interval, the Bayes factor is computed by comparing the prior
#' and posterior odds of the parameter falling within or outside the null interval
#' (Morey & Rouder, 2011; Liao et al., 2020); When the null is a point, a Savage-Dickey
#' density ratio is computed, which is also an approximation of a Bayes factor comparing
#' the marginal likelihoods of the model against a model in which the tested parameter
#' has been restricted to the point null (Wagenmakers et al., 2010; Heck, 2019).
#' \cr \cr
#' Note that the \code{logspline} package is used for estimating densities and probabilities,
#' and must be installed for the function to work.
#' \cr \cr
#' \code{bayesfactor_pointnull()} and \code{bayesfactor_rope()} are wrappers around
#' \code{bayesfactor_parameters} with different defaults for the null to be tested against
#' (a point and a range, respectively). Aliases of the main functions are prefixed
#' with \code{bf_*}, like \code{bf_parameters()} or \code{bf_pointnull()}
#' \cr \cr
#' \strong{For more info, in particular on specifying correct priors for factors with more than 2 levels, see \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.}
#'
#' @param posterior A numerical vector, \code{stanreg} / \code{brmsfit} object, \code{emmGrid}
#' or a data frame - representing a posterior distribution(s) from (see 'Details').
#' @param prior An object representing a prior distribution (see 'Details').
#' @param direction Test type (see 'Details'). One of \code{0}, \code{"two-sided"} (default, two tailed),
#' \code{-1}, \code{"left"} (left tailed) or \code{1}, \code{"right"} (right tailed).
#' @param null Value of the null, either a scaler (for point-null) or a a range
#' (for a interval-null).
#' @inheritParams hdi
#'
#' @return A data frame containing the Bayes factor representing evidence \emph{against} the null.
#'
#' @details This method is used to compute Bayes factors based on prior and posterior distributions.
#' \cr\cr
#' For the computation of Bayes factors, the model priors must be proper priors (at the very least
#' they should be \emph{not flat}, and it is preferable that they be \emph{informative}); As the priors for
#' the alternative get wider, the likelihood of the null value(s) increases, to the extreme that for completely
#' flat priors the null is infinitely more favorable than the alternative (this is called \emph{the Jeffreys-Lindley-Bartlett
#' paradox}). Thus, you should only ever try (or want) to compute a Bayes factor when you have an informed prior.
#' \cr\cr
#' (Note that by default, \code{brms::brm()} uses flat priors for fixed-effects; See example below.)
#'
#' \subsection{Setting the correct \code{prior}}{
#' It is important to provide the correct \code{prior} for meaningful results.
#' \itemize{
#'   \item When \code{posterior} is a numerical vector, \code{prior} should also be a numerical vector.
#'   \item When \code{posterior} is a \code{data.frame}, \code{prior} should also be a \code{data.frame}, with matching column order.
#'   \item When \code{posterior} is a \code{stanreg} or \code{brmsfit} model: \itemize{
#'     \item \code{prior} can be set to \code{NULL}, in which case prior samples are drawn internally.
#'     \item \code{prior} can also be a model equvilant to \code{posterior} but with samples from the priors \emph{only}.
#'   }
#'   \item When \code{posterior} is an \code{emmGrid} object: \itemize{
#'     \item \code{prior} should be the \code{stanreg} or \code{brmsfit} model used to create the \code{emmGrid} objects.
#'     \item \code{prior} can also be an \code{emmGrid} object equvilant to \code{posterior} but created with a model of priors samples \emph{only}.
#'   }
#' }}
#' \subsection{One-sided Tests (setting an order restriction)}{
#' One sided tests (controlled by \code{direction}) are conducted by restricting the prior and
#' posterior of the non-null values (the "alternative") to one side of the null only
#' (\cite{Morey & Wagenmakers, 2014}). For example, if we have a prior hypothesis that the
#' parameter should be positive, the alternative will be restricted to the region to the right
#' of the null (point or interval).
#' }
#' \subsection{Interpreting Bayes Factors}{
#' A Bayes factor greater than 1 can be interpereted as evidence against the null,
#' at which one convention is that a Bayes factor greater than 3 can be considered
#' as "substantial" evidence against the null (and vice versa, a Bayes factor
#' smaller than 1/3 indicates substantial evidence in favor of the null-model)
#' (\cite{Wetzels et al. 2011}).
#' }
#'
#' @examples
#' library(bayestestR)
#'
#' prior <- distribution_normal(1000, mean = 0, sd = 1)
#' posterior <- distribution_normal(1000, mean = .5, sd = .3)
#'
#' bayesfactor_parameters(posterior, prior)
#' \dontrun{
#' # rstanarm models
#' # ---------------
#' if (require("rstanarm") && require("emmeans")) {
#'   contrasts(sleep$group) <- contr.bayes # see vingette
#'   stan_model <- stan_lmer(extra ~ group + (1 | ID), data = sleep)
#'   bayesfactor_parameters(stan_model)
#'   bayesfactor_parameters(stan_model, null = rope_range(stan_model))
#'
#' # emmGrid objects
#' # ---------------
#'   group_diff <- pairs(emmeans(stan_model, ~group))
#'   bayesfactor_parameters(group_diff, prior = stan_model)
#' }
#'
#' # brms models
#' # -----------
#' if (require("brms")) {
#'   contrasts(sleep$group) <- contr.bayes # see vingette
#'   my_custom_priors <-
#'     set_prior("student_t(3, 0, 1)", class = "b") +
#'     set_prior("student_t(3, 0, 1)", class = "sd", group = "ID")
#'
#'   brms_model <- brm(extra ~ group + (1 | ID),
#'     data = sleep,
#'     prior = my_custom_priors
#'   )
#'   bayesfactor_parameters(brms_model)
#' }
#' }
#' @references
#' \itemize{
#' \item Wagenmakers, E. J., Lodewyckx, T., Kuriyal, H., and Grasman, R. (2010). Bayesian hypothesis testing for psychologists: A tutorial on the Savage-Dickey method. Cognitive psychology, 60(3), 158-189.
#' \item Heck, D. W. (2019). A caveat on the Savage–Dickey density ratio: The case of computing Bayes factors for regression parameters. British Journal of Mathematical and Statistical Psychology, 72(2), 316-333.
#' \item Morey, R. D., & Wagenmakers, E. J. (2014). Simple relation between Bayesian order-restricted and point-null hypothesis tests. Statistics & Probability Letters, 92, 121-124.
#' \item Morey, R. D., & Rouder, J. N. (2011). Bayes factor approaches for testing interval null hypotheses. Psychological methods, 16(4), 406.
#' \item Liao, J. G., Midya, V., & Berg, A. (2020). Connecting and contrasting the Bayes factor and a modified ROPE procedure for testing interval null hypotheses. The American Statistician, 1-19.
#' \item Wetzels, R., Matzke, D., Lee, M. D., Rouder, J. N., Iverson, G. J., and Wagenmakers, E.-J. (2011). Statistical Evidence in Experimental Psychology: An Empirical Comparison Using 855 t Tests. Perspectives on Psychological Science, 6(3), 291–298. \doi{10.1177/1745691611406923}
#' }
#'
#' @author Mattan S. Ben-Shachar
#'
#' @export
bayesfactor_parameters <- function(posterior, prior = NULL, direction = "two-sided", null = 0, verbose = TRUE, ...) {
  UseMethod("bayesfactor_parameters")
}

#' @rdname bayesfactor_parameters
#' @export
bayesfactor_pointull <- function(posterior, prior = NULL, direction = "two-sided", null = 0, verbose = TRUE, ...) {

  if (length(null) > 1) {
    message("'null' is a range - computing a ROPE based Bayes factor.")
  }

  bayesfactor_parameters(
    posterior = posterior,
    prior = prior,
    direction = direction,
    null = null,
    verbose = verbose,
    ...
  )
}

#' @rdname bayesfactor_parameters
#' @export
bayesfactor_rope <- function(posterior, prior = NULL, direction = "two-sided", null = rope_range(posterior), verbose = TRUE, ...) {
  if (length(null) < 2) {
    message("'null' is a point - computing a Savage-Dickey (point null) Bayes factor.")
  }

  bayesfactor_parameters(
    posterior = posterior,
    prior = prior,
    direction = direction,
    null = null,
    verbose = verbose,
    ...
  )
}

#' @rdname bayesfactor_parameters
#' @export
bf_parameters <- bayesfactor_parameters

#' @rdname bayesfactor_parameters
#' @export
bf_pointull <- bayesfactor_pointull

#' @rdname bayesfactor_parameters
#' @export
bf_rope <- bayesfactor_rope

#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.numeric <- function(posterior, prior = NULL, direction = "two-sided", null = 0, verbose = TRUE, ...) {
  # nm <- .safe_deparse(substitute(posterior)

  if (is.null(prior)) {
    prior <- posterior
    if (verbose) {
      warning(
        "Prior not specified! ",
        "Please specify a prior (in the form 'prior = distribution_normal(1000, 0, 1)')",
        " to get meaningful results."
      )
    }
  }
  prior <- data.frame(X = prior)
  posterior <- data.frame(X = posterior)
  # colnames(posterior) <- colnames(prior) <- nm

  # Get BFs
  sdbf <- bayesfactor_parameters.data.frame(
    posterior = posterior, prior = prior,
    direction = direction, null = null, ...
  )
  sdbf$Parameter <- NULL
  sdbf
}


#' @importFrom insight clean_parameters
#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.stanreg <- function(posterior,
                                           prior = NULL,
                                           direction = "two-sided",
                                           null = 0,
                                           verbose = TRUE,
                                           effects = c("fixed", "random", "all"),
                                           component = c("conditional", "zi", "zero_inflated", "all"),
                                           parameters = NULL,
                                           ...) {
  cleaned_parameters <- insight::clean_parameters(posterior)
  effects <- match.arg(effects)
  component <- match.arg(component)

  samps <- .clean_priors_and_posteriors(posterior, prior,
                                        verbose = verbose,
                                        effects = effects, component = component,
                                        parameters = parameters)

  # Get BFs
  temp <- bayesfactor_parameters.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    direction = direction, null = null, ...
  )

  bf_val <- .prepare_output(temp, cleaned_parameters)

  class(bf_val) <- class(temp)
  attr(bf_val, "hypothesis") <- attr(temp, "hypothesis") # don't change the name of this attribute - it is used only internally for "see" and printing
  attr(bf_val, "direction") <- attr(temp, "direction")
  attr(bf_val, "plot_data") <- attr(temp, "plot_data")

  bf_val
}



#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.brmsfit <- bayesfactor_parameters.stanreg



#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.emmGrid <- function(posterior,
                                           prior = NULL,
                                           direction = "two-sided",
                                           null = 0,
                                           verbose = TRUE,
                                           ...) {

  samps <- .clean_priors_and_posteriors(posterior, prior,
                                        verbose = verbose)

  # Get BFs
  bayesfactor_parameters.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    direction = direction, null = null, ...
  )
}


#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.data.frame <- function(posterior,
                                              prior = NULL,
                                              direction = "two-sided",
                                              null = 0,
                                              verbose = TRUE,
                                              ...) {
  # find direction
  direction <- .get_direction(direction)

  if (is.null(prior)) {
    prior <- posterior
    warning(
      "Prior not specified! ",
      "Please specify priors (with column order matching 'posterior')",
      " to get meaningful results."
    )
  }

  sdbf <- numeric(ncol(posterior))
  for (par in seq_along(posterior)) {
    sdbf[par] <- .bayesfactor_parameters(
      posterior[[par]],
      prior[[par]],
      direction = direction,
      null = null
    )
  }

  bf_val <- data.frame(
    Parameter = colnames(posterior),
    BF = sdbf,
    stringsAsFactors = FALSE
  )

  class(bf_val) <- unique(c(
    "bayesfactor_parameters",
    "see_bayesfactor_parameters",
    class(bf_val)
  ))

  attr(bf_val, "hypothesis") <- null # don't change the name of this attribute - it is used only internally for "see" and printing
  attr(bf_val, "direction") <- direction
  attr(bf_val, "plot_data") <- .make_BF_plot_data(posterior, prior, direction, null)

  bf_val
}



#' @keywords internal
#' @importFrom insight print_color
.bayesfactor_parameters <- function(posterior, prior, direction = 0, null = 0) {
  if (isTRUE(all.equal(posterior, prior))) {
    return(1)
  }

  if (!requireNamespace("logspline")) {
    stop("Package \"logspline\" needed for this function to work. Please install it.")
  }


  if (length(null) == 1) {
    relative_density <- function(samples) {
      f_samples <- suppressWarnings(logspline::logspline(samples))
      d_samples <- logspline::dlogspline(null, f_samples)

      if (direction < 0) {
        norm_samples <- logspline::plogspline(null, f_samples)
      } else if (direction > 0) {
        norm_samples <- 1 - logspline::plogspline(null, f_samples)
      } else {
        norm_samples <- 1
      }

      d_samples / norm_samples
    }

    return(relative_density(prior) /
      relative_density(posterior))
  } else if (length(null) == 2) {
    null <- sort(null)
    null[is.infinite(null)] <- 1.797693e+308 * sign(null[is.infinite(null)])

    f_prior <- logspline::logspline(prior)
    f_posterior <- logspline::logspline(posterior)

    h0_prior <- diff(logspline::plogspline(null, f_prior))
    h0_post <- diff(logspline::plogspline(null, f_posterior))

    BF_null_full <- h0_post / h0_prior

    if (direction < 0) {
      h1_prior <- logspline::plogspline(min(null), f_prior)
      h1_post <- logspline::plogspline(min(null), f_posterior)
    } else if (direction > 0) {
      h1_prior <- 1 - logspline::plogspline(max(null), f_prior)
      h1_post <- 1 - logspline::plogspline(max(null), f_posterior)
    } else {
      h1_prior <- 1 - h0_prior
      h1_post <- 1 - h0_post
    }
    BF_alt_full <- h1_post / h1_prior

    return(BF_alt_full / BF_null_full)
  } else {
    stop("'null' must be of length 1 or 2")
  }
}

# Bad Methods -------------------------------------------------------------

#' @export
bayesfactor_parameters.bayesfactor_models <- function(...) {
  stop(
    "Oh no, 'bayesfactor_parameters()' does not know how to deal with multiple models :(\n",
    "You might want to use 'bayesfactor_inclusion()' here to test specific terms across models."
  )
}

#' @export
bayesfactor_parameters.sim <- function(...) {
  stop(
    "Bayes factors are based on the shift from a prior to a posterior. ",
    "Since simulated draws are not based on any priors, computing Bayes factors does not make sense :(\n",
    "You might want to try `rope`, `ci`, `pd` or `pmap` for posterior-based inference."
  )
}

#' @export
bayesfactor_parameters.sim.merMod <- bayesfactor_parameters.sim
