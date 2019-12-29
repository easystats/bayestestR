#' Compute Support Intervals
#'
#' A support interval at contains only the values for the parameter predicts the observed data better
#' than average, by some degree \emph{k}. From the presepctive of the Savage-Dickey Bayes factor, testing
#' against a point null hypothesis for any value within the support interval will yeild a Bayes factor larger
#' than \emph{k}.
#' \cr \cr
#' \strong{For more info, in particular on specifying correct priors for factors with more than 2 levels,
#' see \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.}
#'
#' @param posterior A numerical vector, \code{stanreg} / \code{brmsfit} object, \code{emmGrid}
#' or a data frame - representing a posterior distribution(s) from (see 'Details').
#' @param prior An object representing a prior distribution (see 'Details').
#' @param BF The amount of support required to be included in the interval.
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
                      BF = BF)
  }

  out <- as.data.frame(cbind(colnames(posterior),BF,sis))
  colnames(out) <- c("Parameter","CI","CI_low","CI_high")
  class(out) <- unique(c("bayestestR_si", "see_si", "bayestestR_ci", "see_ci", class(out)))

  out
}

.si <- function(posterior, prior, BF = 1) {
  if (!requireNamespace("logspline")) {
    stop("Package \"logspline\" needed for this function to work. Please install it.")
  }
  extend_scale <- 0.05
  precision <- 2^8

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

  range(x_axis[relative_d > BF])
}