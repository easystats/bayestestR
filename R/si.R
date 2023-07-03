#' Compute Support Intervals
#'
#' A support interval contains only the values of the parameter that predict the observed data better
#' than average, by some degree *k*; these are values of the parameter that are associated with an
#' updating factor greater or equal than *k*. From the perspective of the Savage-Dickey Bayes factor, testing
#' against a point null hypothesis for any value within the support interval will yield a Bayes factor smaller
#' than *1/k*.
#'
#' **For more info, in particular on specifying correct priors for factors with more than 2 levels,
#' see [the Bayes factors vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).**
#'
#' @param BF The amount of support required to be included in the support interval.
#' @inheritParams bayesfactor_parameters
#' @inheritParams hdi
#' @inherit hdi seealso
#' @family ci
#'
#' @details This method is used to compute support intervals based on prior and posterior distributions.
#' For the computation of support intervals, the model priors must be proper priors (at the very least
#' they should be *not flat*, and it is preferable that they be *informative* - note
#' that by default, `brms::brm()` uses flat priors for fixed-effects; see example below).
#'
#' @section Choosing a value of `BF`:
#' The choice of `BF` (the level of support) depends on what we want our interval
#' to represent:
#'
#' - A `BF` = 1 contains values whose credibility is not decreased by observing the data.
#' - A `BF` > 1 contains values who received more impressive support from the data.
#' - A `BF` < 1 contains values whose credibility has *not* been impressively
#'   decreased by observing the data. Testing against values outside this interval
#'   will produce a Bayes factor larger than 1/`BF` in support of the alternative.
#'   E.g., if an SI (BF = 1/3) excludes 0, the Bayes factor against the point-null
#'   will be larger than 3.
#'
#' @inheritSection bayesfactor_parameters Setting the correct `prior`
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @return
#' A data frame containing the lower and upper bounds of the SI.
#'
#' Note that if the level of requested support is higher than observed in the data, the
#' interval will be `[NA,NA]`.
#'
#' @examplesIf requireNamespace("logspline", quietly = TRUE)
#' library(bayestestR)
#'
#' prior <- distribution_normal(1000, mean = 0, sd = 1)
#' posterior <- distribution_normal(1000, mean = 0.5, sd = 0.3)
#'
#' si(posterior, prior, verbose = FALSE)
#' \dontrun{
#' # rstanarm models
#' # ---------------
#' library(rstanarm)
#' contrasts(sleep$group) <- contr.equalprior_pairs # see vignette
#' stan_model <- stan_lmer(extra ~ group + (1 | ID), data = sleep)
#' si(stan_model, verbose = FALSE)
#' si(stan_model, BF = 3, verbose = FALSE)
#'
#' # emmGrid objects
#' # ---------------
#' library(emmeans)
#' group_diff <- pairs(emmeans(stan_model, ~group))
#' si(group_diff, prior = stan_model, verbose = FALSE)
#'
#' # brms models
#' # -----------
#' library(brms)
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
#' si(brms_model, verbose = FALSE)
#' }
#' @references
#' Wagenmakers, E., Gronau, Q. F., Dablander, F., & Etz, A. (2018, November 22).
#' The Support Interval. \doi{10.31234/osf.io/zwnxb}
#'
#' @export
si <- function(posterior, prior = NULL, BF = 1, verbose = TRUE, ...) {
  UseMethod("si")
}

#' @rdname si
#' @export
si.numeric <- function(posterior, prior = NULL, BF = 1, verbose = TRUE, ...) {
  if (is.null(prior)) {
    prior <- posterior
    if (verbose) {
      insight::format_warning(
        "Prior not specified!",
        "Support intervals ('si') can only be computed for Bayesian models with proper priors.",
        "Please specify priors (with column order matching 'posterior')."
      )
    }
  }
  prior <- data.frame(X = prior)
  posterior <- data.frame(X = posterior)

  # Get SIs
  out <- si.data.frame(
    posterior = posterior, prior = prior,
    BF = BF, verbose = verbose, ...
  )
  out$Parameter <- NULL
  out
}

#' @rdname si
#' @export
si.stanreg <- function(posterior, prior = NULL,
                       BF = 1, verbose = TRUE,
                       effects = c("fixed", "random", "all"),
                       component = c("location", "conditional", "all", "smooth_terms", "sigma", "auxiliary", "distributional"),
                       parameters = NULL,
                       ...) {
  cleaned_parameters <- insight::clean_parameters(posterior)
  effects <- match.arg(effects)
  component <- match.arg(component)

  samps <- .clean_priors_and_posteriors(posterior, prior,
    verbose = verbose,
    effects = effects, component = component,
    parameters = parameters
  )

  # Get SIs
  temp <- si.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    BF = BF, verbose = verbose, ...
  )

  out <- .prepare_output(temp, cleaned_parameters, inherits(posterior, "stanmvreg"))

  attr(out, "ci_method") <- "SI"
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posterior))
  class(out) <- class(temp)
  attr(out, "plot_data") <- attr(temp, "plot_data")

  out
}


#' @rdname si
#' @export
si.brmsfit <- si.stanreg

#' @rdname si
#' @export
si.blavaan <- si.stanreg


#' @rdname si
#' @export
si.emmGrid <- function(posterior, prior = NULL,
                       BF = 1, verbose = TRUE, ...) {
  samps <- .clean_priors_and_posteriors(posterior, prior,
    verbose = verbose
  )

  # Get SIs
  out <- si.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    BF = BF, verbose = verbose, ...
  )

  attr(out, "ci_method") <- "SI"
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posterior))
  out
}

#' @export
si.emm_list <- si.emmGrid


#' @export
si.stanfit <- function(posterior, prior = NULL, BF = 1, verbose = TRUE, effects = c("fixed", "random", "all"), ...) {
  out <- si(insight::get_parameters(posterior, effects = effects),
    prior = prior, BF = BF, verbose = verbose
  )
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posterior))
  out
}

#' @export
si.get_predicted <- function(posterior, ...) {
  out <- si(as.data.frame(t(posterior)), ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posterior))
  out
}


#' @rdname si
#' @export
si.data.frame <- function(posterior, prior = NULL, BF = 1, verbose = TRUE, ...) {
  if (is.null(prior)) {
    prior <- posterior
    insight::format_warning(
      "Prior not specified!",
      "Support intervals ('si') can only be computed for Bayesian models with proper priors.",
      "Please specify priors (with column order matching 'posterior')."
    )
  }

  if (verbose && (nrow(posterior) < 4e4 || nrow(prior) < 4e4)) {
    insight::format_warning(
      "Support intervals might not be precise.",
      "For precise support intervals, sampling at least 40,000 posterior samples is recommended."
    )
  }

  out <- lapply(BF, function(BFi) {
    .si.data.frame(posterior, prior, BFi, verbose = verbose)
  })
  out <- do.call(rbind, out)

  attr(out, "ci_method") <- "SI"
  attr(out, "ci") <- BF
  attr(out, "plot_data") <- .make_BF_plot_data(posterior, prior, 0, 0, ...)$plot_data
  class(out) <- unique(c("bayestestR_si", "see_si", "bayestestR_ci", "see_ci", class(out)))

  out
}


#' @export
si.draws <- function(posterior, prior = NULL, BF = 1, verbose = TRUE, ...) {
  si(.posterior_draws_to_df(posterior), prior = prior, BF = BF, verbose = verbose, ...)
}

#' @export
si.rvar <- si.draws



# Helper ------------------------------------------------------------------

.si.data.frame <- function(posterior, prior, BF, verbose = TRUE, ...) {
  sis <- matrix(NA, nrow = ncol(posterior), ncol = 2)
  for (par in seq_along(posterior)) {
    sis[par, ] <- .si(posterior[[par]],
      prior[[par]],
      BF = BF,
      verbose = verbose,
      ...
    )
  }

  out <- data.frame(
    Parameter = colnames(posterior),
    CI = BF,
    CI_low = sis[, 1],
    CI_high = sis[, 2],
    stringsAsFactors = FALSE
  )
}



#' @keywords internal
.si <- function(posterior, prior, BF = 1, extend_scale = 0.05, precision = 2^8, verbose = TRUE, ...) {
  insight::check_if_installed("logspline")

  if (isTRUE(all.equal(prior, posterior))) {
    return(c(NA, NA))
  }

  x <- c(prior, posterior)
  x_range <- range(x)
  x_rangex <- stats::median(x) + 7 * stats::mad(x) * c(-1, 1)
  x_range <- c(
    max(c(x_range[1], x_rangex[1])),
    min(c(x_range[2], x_rangex[2]))
  )

  extension_scale <- diff(x_range) * extend_scale
  x_range <- x_range + c(-1, 1) * extension_scale

  x_axis <- seq(x_range[1], x_range[2], length.out = precision)

  f_prior <- .logspline(prior, ...)
  f_posterior <- .logspline(posterior, ...)
  d_prior <- logspline::dlogspline(x_axis, f_prior)
  d_posterior <- logspline::dlogspline(x_axis, f_posterior)

  relative_d <- d_posterior / d_prior

  crit <- relative_d >= BF

  cp <- rle(stats::na.omit(crit))
  if (length(cp$lengths) > 3 && verbose) {
    insight::format_warning("More than 1 SI detected. Plot the result to investigate.")
  }

  x_supported <- stats::na.omit(x_axis[crit])
  if (length(x_supported) < 2) {
    return(c(NA, NA))
  } else {
    range(x_supported)
  }
}
