#' Savage-Dickey density ratio Bayes Factor (BF)
#'
#' This method computes Bayes factors against the null (either a point or an interval),
#' bases on prior and posterior samples of a single parameter. This Bayes factor indicates
#' the degree by which the mass of the posterior distribution has shifted further away
#' from or closer to the null value(s) (relative to the prior distribution), thus indicating
#' if the null value has become less or more likely given the observed data.
#' \cr \cr
#' When the null is an interval, the Bayes factor is computed by comparing the prior
#' and posterior odds of the parameter falling within or outside the null;
#' When the null is a point, a Savage-Dickey density ratio is computed, which is also
#' an approximation of a Bayes factor comparing the marginal likelihoods of the model
#' against a model in which the tested parameter has been restricted to the point null.
#' \cr \cr
#' For more info, see \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.
#'
#' @param posterior A numerical vector, \code{stanreg} / \code{brmsfit} object, \code{emmGrid}
#' or a data frame - representing a posterior distribution(s) from (see Details).
#' @param prior An object representing a prior distribution (see Details).
#' @param direction Test type (see details). One of \code{0}, \code{"two-sided"} (default, two tailed),
#' \code{-1}, \code{"left"} (left tailed) or \code{1}, \code{"right"} (right tailed).
#' @param null Value of the null, either a scaler (for point-null) or a a range
#' (for a interval-null).
#' @inheritParams hdi
#'
#' @return A data frame containing the Bayes factor representing evidence \emph{against} the (point) null effect model.
#'
#' @details This method is used to compute Bayes factors based on prior and posterior distributions.
#' When \code{posterior} is a model (\code{stanreg}, \code{brmsfit}), posterior and prior samples are
#' extracted for each parameter, and Savage-Dickey Bayes factors are computed for each parameter.
#'
#' \strong{NOTE:} For \code{brmsfit} models, the model must have been fitted with \emph{custom (non-default)}
#' priors. See example below.
#'
#' \subsection{Setting the correct \code{prior}}{
#' It is important to provide the correct \code{prior} for meaningful results.
#' \itemize{
#'   \item When \code{posterior} is a numerical vector, \code{prior} should also be a numerical vector.
#'   \item When \code{posterior} is an \code{emmGrid} object based on a \code{stanreg} or \code{brmsfit} model, \code{prior} should be \emph{that model object} (see example).
#'   \item When \code{posterior} is a \code{stanreg} or \code{brmsfit} model, there is no need to specify \code{prior}, as prior samples are drawn internally.
#'   \item When \code{posterior} is a \code{data.frame}, \code{prior} should also be a \code{data.frame}, with matching column order.
#' }}
#' \subsection{One-sided Tests (setting an order restriction)}{
#' One sided tests (controlled by \code{direction}) are conducted by restricting the prior and
#' posterior of the non-null values (the "alternative") to one side of the null only
#' (\cite{Morey & Wagenmakers, 2013}). For example, if we have a prior hypothesis that the
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
#' library(rstanarm)
#' stan_model <- stan_lmer(extra ~ group + (1 | ID), data = sleep)
#' bayesfactor_parameters(stan_model)
#' bayesfactor_parameters(stan_model, null = rope_range(stan_model))
#'
#' # emmGrid objects
#' # ---------------
#' library(emmeans)
#' group_diff <- pairs(emmeans(stan_model, ~group))
#' bayesfactor_parameters(group_diff, prior = stan_model)
#'
#' # brms models
#' # -----------
#' library(brms)
#' my_custom_priors <-
#'   set_prior("student_t(3, 0, 1)", class = "b") +
#'   set_prior("student_t(3, 0, 1)", class = "sd", group = "ID")
#'
#' brms_model <- brm(extra ~ group + (1 | ID),
#'   data = sleep,
#'   prior = my_custom_priors
#' )
#' bayesfactor_parameters(brms_model)
#' }
#'
#' @references
#' \itemize{
#' \item Wagenmakers, E. J., Lodewyckx, T., Kuriyal, H., and Grasman, R. (2010). Bayesian hypothesis testing for psychologists: A tutorial on the Savage-Dickey method. Cognitive psychology, 60(3), 158-189.
#' \item Wetzels, R., Matzke, D., Lee, M. D., Rouder, J. N., Iverson, G. J., and Wagenmakers, E.-J. (2011). Statistical Evidence in Experimental Psychology: An Empirical Comparison Using 855 t Tests. Perspectives on Psychological Science, 6(3), 291–298. \doi{10.1177/1745691611406923}
#' \item Heck, D. W. (2019). A caveat on the Savage–Dickey density ratio: The case of computing Bayes factors for regression parameters. British Journal of Mathematical and Statistical Psychology, 72(2), 316-333.
#' \item Morey, R. D., & Wagenmakers, E. J. (2014). Simple relation between Bayesian order-restricted and point-null hypothesis tests. Statistics & Probability Letters, 92, 121-124.
#' \item Morey, R. D., & Rouder, J. N. (2011). Bayes factor approaches for testing interval null hypotheses. Psychological methods, 16(4), 406.
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
bayesfactor_savagedickey <- function(posterior, prior = NULL, direction = "two-sided", null = 0, verbose = TRUE, ...) {
  if (length(null) == 2) {
    warning("'null' is an interval - computing an interval Bayes factor.")
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
bayesfactor_interval <- function(posterior, prior = NULL, direction = "two-sided", null = rope_range(posterior), verbose = TRUE, ...) {
  if (length(null) == 1) {
    warning("'null' is a point - computing a Savage-Dickey Bayes factor.")
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

  # Get savage-dickey BFs
  sdbf <- bayesfactor_parameters.data.frame(
    posterior = posterior, prior = prior,
    direction = direction, null = null, ...
  )
  sdbf$Parameter <- NULL
  sdbf
}


#' @importFrom insight get_parameters
#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.stanreg <- function(posterior, prior = NULL,
                                             direction = "two-sided", null = 0,
                                             verbose = TRUE,
                                             effects = c("fixed", "random", "all"),
                                             ...) {
  effects <- match.arg(effects)

  # Get Priors
  if (is.null(prior)) {
    prior <- .update_to_priors(posterior, verbose = verbose)
  }

  prior <- insight::get_parameters(prior, effects = effects)
  posterior <- insight::get_parameters(posterior, effects = effects)

  # Get savage-dickey BFs
  bayesfactor_parameters.data.frame(
    posterior = posterior, prior = prior,
    direction = direction, null = null, ...
  )
}



#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.brmsfit <- bayesfactor_parameters.stanreg



#' @importFrom stats update
#' @importFrom insight get_parameters
#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.emmGrid <- function(posterior, prior = NULL,
                                             direction = "two-sided", null = 0,
                                             verbose = TRUE,
                                             ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package 'emmeans' required for this function to work. Please install it by running `install.packages('emmeans')`.")
  }

  if (is.null(prior)) {
    prior <- posterior
    warning(
      "Prior not specified! ",
      "Please provide the original model to get meaningful results."
    )
  } else {
    prior <- .update_to_priors(prior, verbose = verbose)
    prior <- insight::get_parameters(prior, effects = "fixed")
    prior <- stats::update(posterior, post.beta = as.matrix(prior))
  }

  prior <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(prior, names = FALSE)))
  posterior <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(posterior, names = FALSE)))

  bayesfactor_parameters.data.frame(
    posterior = posterior, prior = prior,
    direction = direction, null = null, ...
  )
}

#' @export
bayesfactor_parameters.bayesfactor_models <- function(...){
  stop(
    "Oh no, 'bayesfactor_parameters()' does not know how to deal with multiple models :(\n",
    "You want might want to use 'bayesfactor_inclusion()' here to test specific terms across models."
  )
}


#' @rdname bayesfactor_parameters
#' @export
bayesfactor_parameters.data.frame <- function(posterior, prior = NULL,
                                                direction = "two-sided", null = 0,
                                                verbose = TRUE,
                                                ...) {
  dots <- list(...)
  if (!is.null(dots$hypothesis)) {
    null <- dots$hypothesis
    warning("The 'hypothesis' argument is deprecated. Please use 'null' instead.")
  }
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
    "see_bayesfactor_savagedickey",
    class(bf_val)
  ))

  attr(bf_val, "hypothesis") <- null
  attr(bf_val, "direction") <- direction
  attr(bf_val, "plot_data") <- .make_sdBF_plot_data(posterior, prior, direction, null)

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

    h0_prior <- diff(logspline::plogspline(null,f_prior))
    h0_post <- diff(logspline::plogspline(null,f_posterior))

    BF_null_full <- h0_post/h0_prior

    if (direction < 0) {
      h1_prior <- logspline::plogspline(min(null),f_prior)
      h1_post <- logspline::plogspline(min(null),f_posterior)
    } else if (direction > 0) {
      h1_prior <- 1 - logspline::plogspline(max(null),f_prior)
      h1_post <- 1 - logspline::plogspline(max(null),f_posterior)
    } else {
      h1_prior <- 1 - h0_prior
      h1_post <- 1 - h0_post
    }
    BF_alt_full <- h1_post/h1_prior

    return(BF_alt_full / BF_null_full)
  } else {
    stop("'null' must be of length 1 or 2")
  }
}


# UTILS -------------------------------------------------------------------

#' @keywords internal
.get_direction <- function(direction) {
  if (length(direction) > 1) {
    warning("Using first 'direction' value.")
    direction <- direction[1]
  }

  String <- c("left", "right", "one-sided", "onesided", "two-sided", "twosided", "<", ">", "=", "-1", "0", "1", "+1")
  Value <- c(-1, 1, 1, 1, 0, 0, -1, 1, 0, -1, 0, 1, 1)

  ind <- String == direction
  if (length(ind) == 0) {
    stop("Unrecognized 'direction' argument.")
  }
  Value[ind]
}



#' @importFrom stats median mad approx
#' @importFrom utils stack
#' @keywords internal
.make_sdBF_plot_data <- function(posterior, prior, direction, null) {
  if (!requireNamespace("logspline")) {
    stop("Package \"logspline\" needed for this function to work. Please install it.")
  }

  estimate_samples_density <- function(samples) {
    nm <- .safe_deparse(substitute(samples))
    samples <- utils::stack(samples)
    samples <- split(samples, samples$ind)

    samples <- lapply(samples, function(data) {
      # 1. estimate density
      x <- data$values

      extend_scale <- 0.05
      precision <- 2^8

      x_range <- range(x)
      x_rangex <- stats::median(x) + 7 * stats::mad(x) * c(-1, 1)
      x_range <- c(
        max(c(x_range[1], x_rangex[1])),
        min(c(x_range[2], x_rangex[2]))
      )

      extension_scale <- diff(x_range) * extend_scale
      x_range[1] <- x_range[1] - extension_scale
      x_range[2] <- x_range[2] + extension_scale

      x_axis <- seq(x_range[1], x_range[2], length.out = precision)
      f_x <- logspline::logspline(x)
      y <- logspline::dlogspline(x_axis, f_x)
      d_points <- data.frame(x = x_axis, y = y)

      # 2. estimate points
      d_null <- stats::approx(d_points$x, d_points$y, xout = null)
      d_null$y[is.na(d_null$y)] <- 0

      # 3. direction?
      if (direction > 0) {
        d_points <- d_points[d_points$x > min(null), , drop = FALSE]
        norm_factor <- 1 - logspline::plogspline(min(null),f_x)
        d_points$y <- d_points$y / norm_factor
        d_null$y <- d_null$y / norm_factor
      } else if (direction < 0) {
        d_points <- d_points[d_points$x < max(null), , drop = FALSE]
        norm_factor <- logspline::plogspline(max(null),f_x)
        d_points$y <- d_points$y / norm_factor
        d_null$y <- d_null$y / norm_factor
      }

      d_points$ind <- d_null$ind <- data$ind[1]
      list(d_points, d_null)
    })

    # 4a. orgenize
    point0 <- lapply(samples, function(.) as.data.frame(.[[2]]))
    point0 <- do.call("rbind", point0)

    samplesX <- lapply(samples, function(.) .[[1]])
    samplesX <- do.call("rbind", samplesX)

    samplesX$Distribution <- point0$Distribution <- nm
    rownames(samplesX) <- rownames(point0) <- c()

    list(samplesX, point0)
  }

  # 4b. orgenize
  posterior <- estimate_samples_density(posterior)
  prior <- estimate_samples_density(prior)

  list(
    plot_data = rbind(posterior[[1]], prior[[1]]),
    d_points = rbind(posterior[[2]], prior[[2]])
  )
}