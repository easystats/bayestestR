#' Savage-Dickey density ratio Bayes Factor (BF)
#'
#' This method computes the ratio between the density of a single value (typically the null)
#' of two distributions. When the compared distributions are the posterior and the prior distributions,
#' this results in an approximation of a Bayes factor against the (point) null model.
#'
#' @param posterior A numerical vector, \code{stanreg} / \code{brmsfit} object, \code{emmGrid} or a data frame - representing a posterior distribution(s) from (see Details).
#' @param prior An object representing a prior distribution (see Details).
#' @param direction Test type (see details). One of \code{0}, \code{"two-sided"} (default, two tailed),
#' \code{-1}, \code{"left"} (left tailed) or \code{1}, \code{"right"} (right tailed).
#' @param hypothesis Value to be tested against (usually \code{0} in the context of null hypothesis testing).
#' @inheritParams hdi
#'
#' @return A data frame containing the Bayes factor representing evidence \emph{against} the (point) null effect model.
#'
#' @details This method is used to compute Bayes factors based on prior and posterior distributions.
#' When \code{posterior} is a model (\code{stanreg}, \code{brmsfit}), posterior and prior samples are
#' extracted for each parameter, and Savage-Dickey Bayes factors are computed for each parameter.
#'
#' \strong{NOTE:} For \code{brmsfit} models, the model must have been fitted with \emph{custom (non-default)} priors. See example below.

#'
#' \subsection{Setting the correct \code{prior}}{
#' It is important to provide the correct \code{prior} for meaningful results.
#' \itemize{
#'   \item When \code{posterior} is a numerical vector, \code{prior} should also be a numerical vector.
#'   \item When \code{posterior} is an \code{emmGrid} object based on a \code{stanreg} \ \code{brmsfit} model, \code{prior} should be \emph{that model object} (see example).
#'   \item When \code{posterior} is a \code{stanreg} \ \code{brmsfit} model, there is no need to specify \code{prior}, as prior samples are drawn internally.
#'   \item When \code{posterior} is a \code{data.frame}, \code{prior} should also be a \code{data.frame}, with matching column names.
#' }}
#' \subsection{One-sided Tests (setting an order restriction)}{
#' One sided tests (controlled by \code{direction}) are conducted by setting an order restriction on
#' the prior and posterior distributions (\cite{Morey & Wagenmakers, 2013}).
#' }
#' \subsection{Interpreting Bayes Factors}{
#' A Bayes factor greater than 1 can be interpereted as evidence against the null,
#' at which one convention is that a Bayes factor greater than 3 can be considered
#' as "substantial" evidence against the null (and vice versa, a Bayes factor
#' smaller than 1/3 indicates substantial evidence in favor of the null-hypothesis)
#' (\cite{Wetzels et al. 2011}).
#' }
#'
#' @examples
#' library(bayestestR)
#'
#' prior <- distribution_normal(1000, mean = 0, sd = 1)
#' posterior <- distribution_normal(1000, mean = .5, sd = .3)
#'
#' bayesfactor_savagedickey(posterior, prior)
#' \dontrun{
#' # rstanarm models
#' # ---------------
#' library(rstanarm)
#' stan_model <- stan_lmer(extra ~ group + (1 | ID), data = sleep)
#' bayesfactor_savagedickey(stan_model)
#'
#' # emmGrid objects
#' # ---------------
#' library(emmeans)
#' group_diff <- pairs(emmeans(stan_model, ~ group))
#' bayesfactor_savagedickey(group_diff, prior = stan_model)
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
#' bayesfactor_savagedickey(brms_model)
#' }
#'
#' @references
#' \itemize{
#' \item Wagenmakers, E. J., Lodewyckx, T., Kuriyal, H., and Grasman, R. (2010). Bayesian hypothesis testing for psychologists: A tutorial on the Savage-Dickey method. Cognitive psychology, 60(3), 158-189.
#' \item Wetzels, R., Matzke, D., Lee, M. D., Rouder, J. N., Iverson, G. J., and Wagenmakers, E.-J. (2011). Statistical Evidence in Experimental Psychology: An Empirical Comparison Using 855 t Tests. Perspectives on Psychological Science, 6(3), 291–298. \doi{10.1177/1745691611406923}
#' \item Heck, D. W. (2019). A caveat on the Savage–Dickey density ratio: The case of computing Bayes factors for regression parameters. British Journal of Mathematical and Statistical Psychology, 72(2), 316-333.
#' \item Morey, R. D., & Wagenmakers, E. J. (2014). Simple relation between Bayesian order-restricted and point-null hypothesis tests. Statistics & Probability Letters, 92, 121-124.
#' }
#'
#' @author Mattan S. Ben-Shachar
#'
#' @export
bayesfactor_savagedickey <- function(posterior, prior = NULL, direction = "two-sided", hypothesis = 0, verbose = TRUE, ...) {
  UseMethod("bayesfactor_savagedickey")
}


#' @rdname bayesfactor_savagedickey
#' @export
bayesfactor_savagedickey.numeric <- function(posterior, prior = NULL, direction = "two-sided", hypothesis = 0, verbose = TRUE, ...) {
  # nm <- deparse(substitute(posterior))

  # find direction
  direction <- .get_direction(direction)

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
  colnames(posterior) <- colnames(prior) <- "X" # nm

  # Get savage-dickey BFs
  sdbf <- bayesfactor_savagedickey.data.frame(
    posterior = posterior, prior = prior,
    direction = direction, hypothesis = hypothesis
  )
  sdbf$Parameter <- NULL
  sdbf
}

#' @rdname bayesfactor_savagedickey
#' @export
bayesfactor_savagedickey.stanreg <- function(posterior, prior = NULL,
                                             direction = "two-sided", hypothesis = 0,
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
  bayesfactor_savagedickey.data.frame(
    posterior = posterior, prior = prior,
    direction = direction, hypothesis = hypothesis
  )
}

#' @rdname bayesfactor_savagedickey
#' @export
bayesfactor_savagedickey.brmsfit <- bayesfactor_savagedickey.stanreg

#' @rdname bayesfactor_savagedickey
#' @export
bayesfactor_savagedickey.emmGrid <- function(posterior, prior = NULL,
                                             direction = "two-sided", hypothesis = 0,
                                             verbose = TRUE,
                                             ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package \"emmeans\" needed for this function to work. Please install it.")
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
    prior <- update(posterior, post.beta = as.matrix(prior))
  }

  prior <- as.data.frame(as.matrix(as.mcmc.emmGrid(prior, names = FALSE)))
  posterior <- as.data.frame(as.matrix(as.mcmc.emmGrid(posterior, names = FALSE)))

  bayesfactor_savagedickey.data.frame(
    posterior = posterior, prior = prior,
    direction = direction, hypothesis = hypothesis
  )
}

#' @rdname bayesfactor_savagedickey
#' @export
bayesfactor_savagedickey.data.frame <- function(posterior, prior = NULL,
                                                direction = "two-sided", hypothesis = 0,
                                                verbose = TRUE,
                                                ...) {
  # find direction
  direction <- .get_direction(direction)

  if (is.null(prior)) {
    prior <- posterior
    warning(
      "Prior not specified! ",
      "Please specify priors (with column names matching 'posterior')",
      " to get meaningful results."
    )
  }

  sdbf <- numeric(ncol(posterior))
  for (par in seq_along(posterior)) {
    par_name <- colnames(posterior)[par]
    sdbf[par] <- .bayesfactor_savagedickey(posterior[[par_name]],
      prior[[par_name]],
      direction = direction,
      hypothesis = hypothesis
    )
  }

  bf_val <- data.frame(
    Parameter = colnames(posterior),
    BF = sdbf,
    stringsAsFactors = FALSE
  )

  class(bf_val) <- unique(c(
    "bayesfactor_savagedickey",
    "see_bayesfactor_savagedickey",
    class(bf_val)
  ))

  attr(bf_val, "hypothesis") <- hypothesis
  attr(bf_val, "direction") <- direction
  attr(bf_val, "plot_data") <- .make_sdBF_plot_data(posterior, prior, direction, hypothesis)

  bf_val
}

#' @keywords internal
#' @importFrom insight print_color
.bayesfactor_savagedickey <- function(posterior, prior, direction = 0, hypothesis = 0) {
  if (requireNamespace("logspline", quietly = TRUE)) {
    relative_density <- function(samples) {
      f_samples <- suppressWarnings(logspline::logspline(samples))
      d_samples <- logspline::dlogspline(hypothesis, f_samples)

      if (direction < 0) {
        norm_samples <- logspline::plogspline(hypothesis, f_samples)
      } else if (direction > 0) {
        norm_samples <- 1 - logspline::plogspline(hypothesis, f_samples)
      } else {
        norm_samples <- 1
      }

      d_samples / norm_samples
    }
  } else {
    insight::print_color("Consider installing the \"logspline\" package for a more robust estimate.\n", "red")
    relative_density <- function(samples) {
      d_samples <- density_at(samples, hypothesis)

      if (direction < 0) {
        norm_samples <- mean(samples < hypothesis)
      } else if (direction > 0) {
        norm_samples <- 1 - mean(samples < 0)
      } else {
        norm_samples <- 1
      }

      d_samples / norm_samples
    }
  }

  relative_density(prior) /
    relative_density(posterior)
}


# UTILS -------------------------------------------------------------------

#' @keywords internal
.get_direction <- function(direction) {
  if (length(direction) > 1) {
    warning("Using first 'direction' value.")
    direction <- direction[1]
  }

  String <- c("left", "right", "one-sided", "onesided", "two-sided", "twosided", "<", ">", "=", "-1", "0", "1", "+1")
  Value <- c(-1, 1, 1, 1,  0, 0, -1, 1, 0, -1, 0, 1, 1)

  ind <- String == direction
  if (length(ind) == 0) {
    stop("Unrecognized 'direction' argument.")
  }
  Value[ind]
}



#' @importFrom utils stack
#' @keywords internal
.make_sdBF_plot_data <- function(posterior, prior, direction, hypothesis) {
  if (requireNamespace("logspline", quietly = TRUE)) {
    density_method <- "logspline"
  } else {
    density_method <- "kernel"
  }

  estimate_samples_density <- function(samples) {
    nm <- deparse(substitute(samples))
    samples <- utils::stack(samples)
    samples <- split(samples, samples$ind)

    samples <- lapply(samples, function(data) {
      # 1. estimate density
      d_points <- estimate_density(
        data$values,
        method = density_method,
        extend = TRUE,
        extend_scale = 0.05,
        precision = 2^8
      )

      # 2. estimate points
      d_null <- stats::approx(d_points$x, d_points$y, xout = hypothesis)
      d_null$y[is.na(d_null$y)] <- 0

      # 3. direction?
      if (direction > 0) {
        d_points <- d_points[d_points$x > hypothesis, , drop = FALSE]
        d_points$y <- d_points$y / mean(data$values > hypothesis)
        d_null$y <- d_null$y / mean(data$values > hypothesis)
      } else if (direction < 0) {
        d_points <- d_points[d_points$x < hypothesis, , drop = FALSE]
        d_points$y <- d_points$y / mean(data$values < hypothesis)
        d_null$y <- d_null$y / mean(data$values < hypothesis)
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

#' @keywords internal
.update_to_priors <- function(model, verbose = TRUE){
  UseMethod(".update_to_priors")
}

#' @keywords internal
#' @importFrom stats update
#' @importFrom utils capture.output
.update_to_priors.stanreg <- function(model, verbose = TRUE){
  if (!requireNamespace("rstanarm")) {
    stop("Package \"rstanarm\" needed for this function to work. Please install it.")
  }

  if (verbose) {
    message("Computation of Bayes factors: sampling priors, please wait...")
  }

  capture.output(
    model_prior <- suppressWarnings(
      stats::update(model, prior_PD = TRUE)
    )
  )

  model_prior
}

#' @keywords internal
#' @importFrom stats update
#' @importFrom utils capture.output
#' @importFrom methods is
.update_to_priors.brmsfit <- function(model, verbose = TRUE){
  if (!requireNamespace("brms")) {
    stop("Package \"brms\" needed for this function to work. Please install it.")
  }

  if (verbose) {
    message("Computation of Bayes factors: sampling priors, please wait...")
  }

  capture.output(
    model_prior <- try(suppressMessages(suppressWarnings(
      stats::update(model, sample_prior = "only")
    )), silent = TRUE)
  )

  if (is(model_prior, "try-error")) {
    if (grepl("proper priors", model_prior)) {
      stop("Cannot compute BF for 'brmsfit' models fit with default priors.\n",
           "See '?bayesfactor_savagedickey'",
           call. = FALSE)
    } else {
      stop(model_prior)
    }
  }

  model_prior
}