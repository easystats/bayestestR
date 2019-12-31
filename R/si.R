#' Compute Support Intervals
#'
#' A support interval contains only the values of the parameter that predict the observed data better
#' than average, by some degree \emph{k}; these are values of the parameter that are associated with an
#' updating factor ≥ \emph{k}. From the presepctive of the Savage-Dickey Bayes factor, testing
#' against a point null hypothesis for any value within the support interval will yeild a Bayes factor smaller
#' than \emph{1/k}.
#' \cr \cr
#' \strong{For more info, in particular on specifying correct priors for factors with more than 2 levels,
#' see \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.}
#'
#' @param posterior A numerical vector, \code{stanreg} / \code{brmsfit} object, \code{emmGrid}
#' or a data frame - representing a posterior distribution(s) from (see 'Details').
#' @param prior An object representing a prior distribution (see 'Details').
#' @param BF The amount of support required to be included in the interval.
#' @param ... Arguments passed to and from other methods.
#' @inheritParams hdi
#'
#' @details This method is used to compute support intervals based on prior and posterior distributions.
#' \cr\cr
#' For the computation of support intervals, the model priors must be proper priors (at the very least
#' they should be \emph{not flat}, and it is preferable that they be \emph{informative}).
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
#'
#' \subsection{Choosing a value of \code{BF}}{
#' The choice of \code{BF} (the level of support) depends on what we want our interval to represent:
#' \itemize{
#'   \item A \code{BF} = 1 contains values whose credibility is not decreased by observing the data.
#'   \item A \code{BF} > 1 contains values who recived more impressive support from the data.
#'   \item A \code{BF} < 1 contains values whose credibility has not been impressive decreased by observing the data.
#'   These are also values who, if tested agaisnt would produce a Bayes factor larger than 1/\code{BF} in support of
#'   the alternative. E.g., if an SI (BF = 1/3) excludes 0, the Bayes factor against the point-null will be larger than 3.
#' }
#' }
#'
#' @return
#' A data frame containing the lower and upper bounds of the SI.
#' \cr
#' Note that if the level of requested support is higher than observed in the data, the
#' interval will be \code{[NA,NA]}.
#'
#' @examples
#' library(bayestestR)
#'
#' prior <- distribution_normal(1000, mean = 0, sd = 1)
#' posterior <- distribution_normal(1000, mean = .5, sd = .3)
#'
#' si(posterior, prior)
#' \dontrun{
#' # rstanarm models
#' # ---------------
#' library(rstanarm)
#' contrasts(sleep$group) <- contr.bayes # see vingette
#' stan_model <- stan_lmer(extra ~ group + (1 | ID), data = sleep)
#' si(stan_model)
#' si(stan_model, BF = 3)
#'
#' # emmGrid objects
#' # ---------------
#' library(emmeans)
#' group_diff <- pairs(emmeans(stan_model, ~group))
#' si(group_diff, prior = stan_model)
#'
#' # brms models
#' # -----------
#' library(brms)
#' contrasts(sleep$group) <- contr.bayes # see vingette
#' my_custom_priors <-
#'   set_prior("student_t(3, 0, 1)", class = "b") +
#'   set_prior("student_t(3, 0, 1)", class = "sd", group = "ID")
#'
#' brms_model <- brm(extra ~ group + (1 | ID),
#'   data = sleep,
#'   prior = my_custom_priors
#' )
#' si(brms_model)
#' }
#' @references
#' Wagenmakers, E., Gronau, Q. F., Dablander, F., & Etz, A. (2018, November 22). The Support Interval. \doi(10.31234/osf.io/zwnxb)
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
      warning(
        "Prior not specified! ",
        "Please specify a prior (in the form 'prior = distribution_normal(1000, 0, 1)')",
        " to get meaningful results."
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

#' @importFrom insight clean_parameters
#' @rdname si
#' @export
si.stanreg <- function(posterior, prior = NULL,
                       BF = 1, verbose = TRUE,
                       effects = c("fixed", "random", "all"),
                       component = c("conditional", "zi", "zero_inflated", "all"),
                       ...) {
  cleaned_parameters <- insight::clean_parameters(posterior)
  effects <- match.arg(effects)
  component <- match.arg(component)

  samps <- .clean_priors_and_posteriors(posterior, prior,
                                        verbose = verbose,
                                        effects = effects, component = component)

  # Get SIs
  temp <- si.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    BF = BF, verbose = verbose, ...
  )

  out <- .prepare_output(temp, cleaned_parameters)

  class(out) <- class(temp)

  out
}


#' @rdname si
#' @export
si.brmsfit <- si.stanreg


#' @rdname si
#' @export
si.emmGrid <- function(posterior, prior = NULL,
                       BF = 1, verbose = TRUE, ...) {

  samps <- .clean_priors_and_posteriors(posterior, prior,
                                        verbose = verbose)

  # Get SIs
  si.data.frame(
    posterior = samps$posterior, prior = samps$prior,
    BF = BF, verbose = verbose, ...
  )
}

#' @rdname si
#' @export
si.data.frame <- function(posterior, prior = NULL, BF = 1, verbose = TRUE, ...){

  if (is.null(prior)) {
    prior <- posterior
    warning(
      "Prior not specified! ",
      "Please specify priors (with column order matching 'posterior')",
      " to get meaningful results."
    )
  }

  sis <- matrix(NA, nrow = ncol(posterior), ncol = 2)
  for (par in seq_along(posterior)) {
    sis[par, ] <- .si(posterior[[par]],
                      prior[[par]],
                      BF = BF, ...)
  }

  out <- data.frame(
    Parameter = colnames(posterior),
    CI = BF,
    CI_low = sis[,1],
    CI_high = sis[,2],
    stringsAsFactors = FALSE
  )
  class(out) <- unique(c("bayestestR_si", "see_si", "bayestestR_ci", "see_ci", class(out)))

  out
}

.si <- function(posterior, prior, BF = 1, extend_scale = 0.05, precision = 2^8) {
  if (!requireNamespace("logspline")) {
    stop("Package \"logspline\" needed for this function to work. Please install it.")
  }

  x <- c(prior, posterior)
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

  f_prior <- logspline::logspline(prior)
  f_posterior <- logspline::logspline(posterior)
  d_prior <- logspline::dlogspline(x_axis, f_prior)
  d_posterior <- logspline::dlogspline(x_axis, f_posterior)

  relative_d <- d_posterior / d_prior

  x_supported <- x_axis[relative_d >= BF]
  if (length(x_supported)<2) {
    return(c(NA,NA))
  } else {
    range(range(x_supported))
  }

}