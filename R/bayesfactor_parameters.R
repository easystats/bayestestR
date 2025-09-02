#' Bayes Factors (BF) for a Single Parameter
#'
#' This method computes Bayes factors against the null (either a point or an
#' interval), based on prior and posterior samples of a single parameter. This
#' Bayes factor indicates the degree by which the mass of the posterior
#' distribution has shifted away from or closer to the null value(s)
#' (relative to the prior distribution), thus indicating if the null value has
#' become less or more likely given the observed data.
#' \cr \cr
#' When the null is an interval, the Bayes factor is computed by comparing the
#' prior and posterior odds of the parameter falling within or outside the null
#' interval (Morey & Rouder, 2011; Liao et al., 2020); When the null is a point,
#' a Savage-Dickey density ratio is computed, which is also an approximation of
#' a Bayes factor comparing the marginal likelihoods of the model against a
#' model in which the tested parameter has been restricted to the point null
#' (Wagenmakers et al., 2010; Heck, 2019).
#' \cr \cr
#' `bayesfactor_pointnull()` and `bayesfactor_rope()` are wrappers around
#' `bayesfactor_parameters()` with different defaults for the null to be tested
#' against (a point and a range, respectively; see details). The `bf_*`
#' functions are aliases of the main functions.
#' \cr \cr
#' \strong{For more info, see [the Bayes factors vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).}
#'
#' @param posterior A numerical vector, `stanreg` / `brmsfit` object,
#'   `emmGrid` or a data frame - representing a posterior distribution(s)
#'   from (see 'Details').
#' @param prior An object representing a prior distribution (see 'Details').
#' @param direction Test type (see 'Details'). One of `0`,
#'   `"two-sided"` (default, two tailed), `-1`, `"left"` (left
#'   tailed) or `1`, `"right"` (right tailed).
#' @param null Value of the null, either a scalar (for point-null) or a range
#'   (for a interval-null).
#' @param ... Arguments passed to and from other methods. (Can be used to pass
#'   arguments to internal [logspline::logspline()].)
#' @inheritParams hdi
#'
#' @return A data frame containing the (log) Bayes factor representing evidence
#'   *against* the null  (Use `as.numeric()` to extract the non-log Bayes
#'   factors; see examples).
#'
#' @note There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html)
#'   implemented in the
#'   \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details
#' This method is used to compute Bayes factors based on prior and posterior
#' distributions.
#' \cr \cr
#' Note that the `logspline` package is used for estimating densities and
#' probabilities, and must be installed for the function to work.
#'
#'
#' ## One-sided & Dividing Tests (setting an order restriction):
#' One sided tests (controlled by `direction`) are conducted by restricting
#' the prior and posterior of the non-null values (the "alternative") to one
#' side of the null only (\cite{Morey & Wagenmakers, 2014}). For example, if we
#' have a prior hypothesis that the parameter should be positive, the
#' alternative will be restricted to the region to the right of the null (point
#' or interval). For example, for a Bayes factor comparing the "null" of `0-0.1`
#' to the alternative `>0.1`, we would set
#' `bayesfactor_parameters(null = c(0, 0.1), direction = ">")`.
#' \cr\cr
#' It is also possible to compute a Bayes factor for **dividing**
#' hypotheses - that is, for a null and alternative that are complementary,
#' opposing one-sided hypotheses (\cite{Morey & Wagenmakers, 2014}). For
#' example, for a Bayes factor comparing the "null" of `<0` to the alternative
#' `>0`, we would set `bayesfactor_parameters(null = c(-Inf, 0))`.
#'
#'
#' ## Additional methods
#' The resulting output is supported by the following methods:
#'
#' - `as.numeric()`: Extract the (possibly log-)Bayes factor values.
#'
#' See [bayesfactor_methods].
#'
#' @inheritSection bayesfactor_methods Prior and posterior considerations
#'
#' @section Obtaining prior samples:
#'
#' It is important to provide the correct `prior` for meaningful results,
#' to match the `posterior`-type input:
#'
#' - **A numeric vector** - `prior` should also be a _numeric vector_, representing the prior-distribution
#' - **A data frame** - `prior` should also be a _data frame_, representing the prior-estimates, in matching column order.
#'   - If `rvar_col` is specified, `prior` should be _the name of an `rvar` column_ that represents the prior-estimates.
#' - **Supported Bayesian model (`stanreg`, `brmsfit`, etc.)**
#'   - `prior` should be _a model an equivalent model with MCMC samples from the priors **only**_. See [unupdate()].
#'   - If `prior` is set to `NULL`, [unupdate()] is called internally (not supported for `brmsfit_multiple` model).
#' - **Output from a `{marginaleffects}` function** - `prior` should also be _an equivalent output_ from a `{marginaleffects}` function based on a prior-model
#'  (See [unupdate()]).
#' - **Output from an `{emmeans}` function**
#'   - `prior` should also be _an equivalent output_ from an `{emmeans}` function based on a prior-model (See [unupdate()]).
#'   - `prior` can also be _the original (posterior) model_, in which case the function
#'      will try to "unupdate" the estimates (not supported if the estimates have undergone
#'      any transformations -- `"log"`, `"response"`, etc. -- or any `regrid`ing).
#'
#' @inheritSection bayesfactor_methods Interpreting Bayes Factors
#'
#' @inheritSection hdi Model components
#'
#' @examplesIf require("logspline")
#' library(bayestestR)
#' prior <- distribution_normal(1000, mean = 0, sd = 1)
#' posterior <- distribution_normal(1000, mean = .5, sd = .3)
#' (BF_pars <- bayesfactor_parameters(posterior, prior, verbose = FALSE))
#'
#' as.numeric(BF_pars)
#'
#' @examplesIf require("rstanarm") && require("emmeans") && require("logspline")
#' \donttest{
#' # rstanarm models
#' # ---------------
#' contrasts(sleep$group) <- contr.equalprior_pairs # see vingette
#' stan_model <- suppressWarnings(stan_lmer(
#'   extra ~ group + (1 | ID),
#'   data = sleep,
#'   refresh = 0
#' ))
#' bayesfactor_parameters(stan_model, verbose = FALSE)
#' bayesfactor_parameters(stan_model, null = rope_range(stan_model))
#'
#' # emmGrid objects
#' # ---------------
#' group_diff <- pairs(emmeans(stan_model, ~group, data = sleep))
#' bayesfactor_parameters(group_diff, prior = stan_model, verbose = FALSE)
#'
#' # Or
#' # group_diff_prior <- pairs(emmeans(unupdate(stan_model), ~group))
#' # bayesfactor_parameters(group_diff, prior = group_diff_prior, verbose = FALSE)
#' }
#' @examplesIf require("brms") && require("logspline")
#' # brms models
#' # -----------
#' \dontrun{
#' contrasts(sleep$group) <- contr.equalprior_pairs # see vingette
#' my_custom_priors <-
#'   set_prior("student_t(3, 0, 1)", class = "b") +
#'   set_prior("student_t(3, 0, 1)", class = "sd", group = "ID")
#'
#' brms_model <- suppressWarnings(brm(extra ~ group + (1 | ID),
#'   data = sleep,
#'   prior = my_custom_priors,
#'   refresh = 0
#' ))
#' bayesfactor_parameters(brms_model, verbose = FALSE)
#' }
#'
#' @references
#'
#' - Wagenmakers, E. J., Lodewyckx, T., Kuriyal, H., and Grasman, R. (2010).
#' Bayesian hypothesis testing for psychologists: A tutorial on the
#' Savage-Dickey method. Cognitive psychology, 60(3), 158-189.
#'
#' - Heck, D. W. (2019). A caveat on the Savage–Dickey density ratio: The
#' case of computing Bayes factors for regression parameters. British Journal of
#' Mathematical and Statistical Psychology, 72(2), 316-333.
#'
#' - Morey, R. D., & Wagenmakers, E. J. (2014). Simple relation between
#' Bayesian order-restricted and point-null hypothesis tests. Statistics &
#' Probability Letters, 92, 121-124.
#'
#' - Morey, R. D., & Rouder, J. N. (2011). Bayes factor approaches for
#' testing interval null hypotheses. Psychological methods, 16(4), 406.
#'
#' - Liao, J. G., Midya, V., & Berg, A. (2020). Connecting and contrasting
#' the Bayes factor and a modified ROPE procedure for testing interval null
#' hypotheses. The American Statistician, 1-19.
#'
#' - Wetzels, R., Matzke, D., Lee, M. D., Rouder, J. N., Iverson, G. J., and
#' Wagenmakers, E.-J. (2011). Statistical Evidence in Experimental Psychology:
#' An Empirical Comparison Using 855 t Tests. Perspectives on Psychological
#' Science, 6(3), 291–298. \doi{10.1177/1745691611406923}
#'
#' @author Mattan S. Ben-Shachar
#'
#' @family Bayes factors
#'
#' @export
bayesfactor_parameters <- function(posterior,
                                   prior = NULL,
                                   direction = "two-sided",
                                   null = 0,
                                   ...,
                                   verbose = TRUE) {
  UseMethod("bayesfactor_parameters")
}

#' @rdname bayesfactor_parameters
#' @export
bayesfactor_pointnull <- function(posterior,
                                  prior = NULL,
                                  direction = "two-sided",
                                  null = 0,
                                  ...,
                                  verbose = TRUE) {
  if (length(null) > 1L && verbose) {
    insight::format_alert("`null` is a range - computing a ROPE based Bayes factor.")
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
bayesfactor_rope <- function(posterior,
                             prior = NULL,
                             direction = "two-sided",
                             null = rope_range(posterior, verbose = FALSE),
                             ...,
                             verbose = TRUE) {
  if (length(null) < 2 && verbose) {
    insight::format_alert("'null' is a point - computing a Savage-Dickey (point null) Bayes factor.")
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
bf_pointnull <- bayesfactor_pointnull

#' @rdname bayesfactor_parameters
#' @export
bf_rope <- bayesfactor_rope

#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.numeric <- function(posterior,
                                           prior = NULL,
                                           direction = "two-sided",
                                           null = 0,
                                           ...,
                                           verbose = TRUE) {
  # nm <- insight::safe_deparse(substitute(posterior)

  if (is.null(prior)) {
    prior <- posterior
    if (verbose) {
      insight::format_warning(
        "Prior not specified! Please specify a prior (in the form 'prior = distribution_normal(1000, 0, 1)') to get meaningful results."
      )
    }
  }
  prior <- data.frame(X = prior)
  posterior <- data.frame(X = posterior)
  # colnames(posterior) <- colnames(prior) <- nm

  # Get BFs
  sdbf <- bayesfactor_parameters.data.frame(
    posterior = posterior, prior = prior,
    direction = direction, null = null,
    verbose = verbose, ...
  )
  sdbf$Parameter <- NULL
  sdbf
}


#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.stanreg <- function(posterior,
                                           prior = NULL,
                                           direction = "two-sided",
                                           null = 0,
                                           effects = "fixed",
                                           component = "conditional",
                                           parameters = NULL,
                                           ...,
                                           verbose = TRUE) {
  cleaned_parameters <- .get_cleaned_parameters(posterior, ...)

  samps <- .clean_priors_and_posteriors(posterior, prior,
    effects = effects, component = component,
    parameters = parameters, verbose = verbose
  )

  # Get BFs
  temp <- bayesfactor_parameters.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    direction = direction, null = null,
    verbose = verbose, ...
  )

  bf_val <- .prepare_output(temp, cleaned_parameters, inherits(posterior, "stanmvreg"))

  class(bf_val) <- class(temp)
  attr(bf_val, "clean_parameters") <- cleaned_parameters
  # don't change the name of this attribute - it is used only internally for "see" and printing
  attr(bf_val, "hypothesis") <- attr(temp, "hypothesis")
  attr(bf_val, "direction") <- attr(temp, "direction")
  attr(bf_val, "plot_data") <- attr(temp, "plot_data")

  bf_val
}


#' @export
bayesfactor_parameters.brmsfit <- bayesfactor_parameters.stanreg


#' @export
bayesfactor_parameters.blavaan <- function(posterior,
                                           prior = NULL,
                                           direction = "two-sided",
                                           null = 0,
                                           ...,
                                           verbose = TRUE) {
  cleaned_parameters <- insight::clean_parameters(posterior)

  samps <- .clean_priors_and_posteriors(posterior, prior,
    verbose = verbose
  )

  # Get BFs
  temp <- bayesfactor_parameters.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    direction = direction, null = null,
    verbose = verbose, ...
  )

  bf_val <- .prepare_output(temp, cleaned_parameters)

  class(bf_val) <- class(temp)
  attr(bf_val, "clean_parameters") <- cleaned_parameters
  # don't change the name of this attribute - it is used only internally for "see" and printing
  attr(bf_val, "hypothesis") <- attr(temp, "hypothesis")
  attr(bf_val, "direction") <- attr(temp, "direction")
  attr(bf_val, "plot_data") <- attr(temp, "plot_data")

  bf_val
}


#' @export
bayesfactor_parameters.emmGrid <- function(posterior,
                                           prior = NULL,
                                           direction = "two-sided",
                                           null = 0,
                                           ...,
                                           verbose = TRUE) {
  samps <- .clean_priors_and_posteriors(
    posterior,
    prior,
    verbose = verbose
  )

  # Get BFs
  out <- bayesfactor_parameters.data.frame(
    posterior = samps$posterior,
    prior = samps$prior,
    direction = direction,
    null = null,
    verbose = verbose,
    ...
  )
  .append_datagrid(out, posterior)
}

#' @export
bayesfactor_parameters.emm_list <- bayesfactor_parameters.emmGrid

#' @export
bayesfactor_parameters.slopes <- bayesfactor_parameters.emmGrid

#' @export
bayesfactor_parameters.predictions <- bayesfactor_parameters.emmGrid

#' @export
bayesfactor_parameters.comparisons <- bayesfactor_parameters.emmGrid


#' @rdname bayesfactor_parameters
#' @inheritParams p_direction
#' @export
bayesfactor_parameters.data.frame <- function(posterior,
                                              prior = NULL,
                                              direction = "two-sided",
                                              null = 0,
                                              rvar_col = NULL,
                                              ...,
                                              verbose = TRUE) {
  x_rvar <- .possibly_extract_rvar_col(posterior, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::bayesfactor_parameters
    cl$posterior <- x_rvar
    cl$rvar_col <- NULL
    prior_rvar <- .possibly_extract_rvar_col(posterior, prior)
    if (length(prior_rvar) > 0L) {
      cl$prior <- prior_rvar
    }
    out <- eval.parent(cl)

    obj_name <- insight::safe_deparse_symbol(substitute(posterior))
    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    return(.append_datagrid(out, posterior))
  }

  # find direction
  direction <- .get_direction(direction)

  if (is.null(prior)) {
    prior <- posterior
    if (verbose) {
      insight::format_warning(
        "Prior not specified! Please specify priors (with column order matching 'posterior') to get meaningful results."
      )
    }
  }

  if (verbose && length(null) == 1L && (nrow(posterior) < 4e4 || nrow(prior) < 4e4)) {
    insight::format_warning(
      "Bayes factors might not be precise.",
      "For precise Bayes factors, sampling at least 40,000 posterior samples is recommended."
    )
  }


  sdlogbf <- numeric(ncol(posterior))
  for (par in seq_along(posterior)) {
    sdlogbf[par] <- .logbayesfactor_parameters(
      posterior[[par]],
      prior[[par]],
      direction = direction,
      null = null,
      ...
    )
  }

  bf_val <- data.frame(
    Parameter = colnames(posterior),
    log_BF = sdlogbf,
    stringsAsFactors = FALSE
  )

  class(bf_val) <- unique(c(
    "bayestestRBF",
    "bayesfactor_parameters",
    "see_bayesfactor_parameters",
    class(bf_val)
  ))

  attr(bf_val, "hypothesis") <- null # don't change the name of this attribute - it is used only internally for "see" and printing
  attr(bf_val, "direction") <- direction
  attr(bf_val, "plot_data") <- .make_BF_plot_data(posterior, prior, direction, null, ...)

  bf_val
}


#' @export
bayesfactor_parameters.draws <- function(posterior,
                                         prior = NULL,
                                         direction = "two-sided",
                                         null = 0,
                                         ...,
                                         verbose = TRUE) {
  bayesfactor_parameters(
    .posterior_draws_to_df(posterior),
    prior = if (!is.null(prior)) .posterior_draws_to_df(prior),
    direction = direction,
    null = null,
    verbose = verbose,
    ...
  )
}

#' @export
bayesfactor_parameters.rvar <- bayesfactor_parameters.draws


#' @keywords internal
.logbayesfactor_parameters <- function(posterior,
                                       prior,
                                       direction = 0,
                                       null = 0,
                                       ...) {
  stopifnot(length(null) %in% c(1, 2))

  if (isTRUE(all.equal(posterior, prior))) {
    return(0)
  }

  insight::check_if_installed("logspline")

  if (length(null) == 1) {
    relative_loglikelihood <- function(samples) {
      f_samples <- .logspline(samples, ...)
      d_samples <- logspline::dlogspline(null, f_samples, log = TRUE)

      if (direction < 0) {
        norm_samples <- logspline::plogspline(null, f_samples)
      } else if (direction > 0) {
        norm_samples <- 1 - logspline::plogspline(null, f_samples)
      } else {
        norm_samples <- 1
      }

      d_samples - log(norm_samples)
    }
  } else if (length(null) == 2) {
    null <- sort(null)
    null[is.infinite(null)] <- 1.797693e+308 * sign(null[is.infinite(null)])

    relative_loglikelihood <- function(samples) {
      f_samples <- .logspline(samples, ...)
      p_samples <- diff(logspline::plogspline(null, f_samples))

      if (direction < 0) {
        norm_samples <- logspline::plogspline(min(null), f_samples)
      } else if (direction > 0) {
        norm_samples <- 1 - logspline::plogspline(max(null), f_samples)
      } else {
        norm_samples <- 1 - p_samples
      }

      log(p_samples) - log(norm_samples)
    }
  }

  relative_loglikelihood(prior) - relative_loglikelihood(posterior)
}

# Bad Methods -------------------------------------------------------------

#' @export
bayesfactor_parameters.bayesfactor_models <- function(...) {
  insight::format_error(
    "Oh no, 'bayesfactor_parameters()' does not know how to deal with multiple models :(",
    "You might want to use 'bayesfactor_inclusion()' here to test specific terms across models."
  )
}

#' @export
bayesfactor_parameters.sim <- function(...) {
  insight::format_error(
    "Bayes factors are based on the shift from a prior to a posterior.",
    "Since simulated draws are not based on any priors, computing Bayes factors does not make sense :(",
    "You might want to try `rope`, `ci`, `pd` or `pmap` for posterior-based inference."
  )
}

#' @export
bayesfactor_parameters.sim.merMod <- bayesfactor_parameters.sim
