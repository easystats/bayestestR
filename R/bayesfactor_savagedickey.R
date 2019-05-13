#' Savage-Dickey density ratio Bayes factor
#'
#' This method computes the ratio between the density of a single value (typically the null)
#' in two distributions, typically the posterior vs. the prior distributions.
#'
#' @param posterior Vector representing a posterior distribution, or a \code{stanreg} object.
#' @param prior Vector representing a prior distribution (If \code{posterior} is a vector, otherwise ignored).
#' @param direction Test type. One of \code{0}, \code{"two-sided"} (defult; two tailed),
#' \code{-1}, \code{"left"} (left tailed), \code{1}, \code{"right"} (right tailed).
#' @param hypothesis Value to be tested against (usually \code{0} in the context of null hypothesis testing).
#' @param effects Should results for fixed effects, random effects or both be returned? Only applies to mixed models. May be abbreviated.
#' @param ... Currently not used.
#'
#' @return A data frame containing the Bayes factor representing by how
#' much \emph{less} the null is likely under the posterior compared to the prior.
#'
#' @details This method is used to examine if the hypothesis value is less or more
#' likely given the observed data. When posterior is a model (\code{stanreg}),
#' posterior and prior samples are extracted for each parameter, and
#' Savage-Dickey Bayes factors are computed for each parameter.
#' \cr \cr
#' A Bayes factor greater than 1 can be interpereted as evidence against the null,
#' at which one convention is that a Bayes factor greater than 3 can be considered
#' as "substantial" evidence against the null (and vice versa, a Bayes factor
#' smaller than 1/3 indicates substantial evidence in favor of the null-hypothesis)
#' (\cite{Wetzels et al. 2011}).
#'
#' @examples
#' library(bayestestR)
#'
#' prior <- distribution_normal(1000, mean = 0, sd = 1)
#' posterior <- distribution_normal(1000, mean = .5, sd = .3)
#'
#' bayesfactor_savagedickey(posterior, prior)
#' \dontrun{
#' library(rstanarm)
#' stan_model <- stan_glm(extra ~ group, data = sleep)
#' bayesfactor_savagedickey(stan_model)
#' }
#'
#' @references
#' \itemize{
#' \item Wagenmakers, E. J., Lodewyckx, T., Kuriyal, H., & Grasman, R. (2010). Bayesian hypothesis testing for psychologists: A tutorial on the Savage-Dickey method. Cognitive psychology, 60(3), 158-189.
#' \item Wetzels, R., Matzke, D., Lee, M. D., Rouder, J. N., Iverson, G. J., & Wagenmakers, E.-J. (2011). Statistical Evidence in Experimental Psychology: An Empirical Comparison Using 855 t Tests. Perspectives on Psychological Science, 6(3), 291–298. \doi{10.1177/1745691611406923}
#' }
#'
#' @author Mattan S. Ben-Shachar
#'
#' @export
bayesfactor_savagedickey <- function(posterior, prior = NULL, direction = "two-sided", hypothesis = 0, ...) {
  UseMethod("bayesfactor_savagedickey")
}


#' @rdname bayesfactor_savagedickey
#' @export
#' @importFrom stats rcauchy sd
bayesfactor_savagedickey.numeric <- function(posterior, prior = NULL, direction = "two-sided", hypothesis = 0, ...) {
  if (is.null(prior)) {
    prior <- posterior
    warning(
      "Prior not specified!\n",
      "Please specify a prior (in the form 'prior = distribution_normal(1000, 0, 1)')",
      " to get meaningful results!"
    )
  }

  bf_val <- data.frame(BFsd = .bayesfactor_savagedickey(posterior, prior, direction = direction, hypothesis = hypothesis))
  class(bf_val) <- c("bayesfactor_savagedickey", class(bf_val))
  attr(bf_val, "hypothesis") <- hypothesis

  bf_val
}

#' @rdname bayesfactor_savagedickey
#' @export
#' @importFrom insight find_algorithm
#' @importFrom stats update
#' @importFrom utils capture.output
bayesfactor_savagedickey.stanreg <- function(posterior, prior = NULL,
                                             direction = "two-sided", hypothesis = 0,
                                             effects = c("fixed", "random", "all"),
                                             ...) {
  if (!requireNamespace("rstanarm")) {
    stop("Package \"rstanarm\" needed for this function to work. Please install it.")
  }

  effects <- match.arg(effects)

  # Get Priors
  if (is.null(prior)) {
    alg <- insight::find_algorithm(posterior)

    capture.output(prior <- suppressWarnings(
      stats::update(
        posterior,
        prior_PD = TRUE,
        iter = alg$iterations,
        chains = alg$chains,
        warmup = alg$warmup
      )
    ))
    prior <- insight::get_parameters(prior, effects = effects)
  }

  posterior <- insight::get_parameters(posterior, effects = effects)

  # Get savage-dickey BFs
  sdbf <- numeric(ncol(prior))

  for (par in seq_len(ncol(posterior))) {
    sdbf[par] <- .bayesfactor_savagedickey(posterior[[par]],
      prior[[par]],
      direction = direction,
      hypothesis = hypothesis
    )
  }

  bf_val <- data.frame(BFsd = sdbf, row.names = colnames(posterior))
  class(bf_val) <- c("bayesfactor_savagedickey", class(bf_val))
  attr(bf_val, "hypothesis") <- hypothesis

  bf_val
}

#' @importFrom insight print_color
.bayesfactor_savagedickey <- function(posterior, prior, direction = "two-sided", hypothesis = 0) {
  # find direction
  direction.opts <- data.frame(
    String = c("left", "right", "two-sided", "<", ">", "=", "-1", "0", "1", "+1"),
    Value = c(-1, 1, 0, -1, 1, 0, -1, 0, 1, 1)
  )
  direction <- direction.opts$Value[pmatch(direction, direction.opts$String, 2)[1]]

  if (requireNamespace("logspline")) {
    f_post <- suppressWarnings(logspline::logspline(posterior))
    f_prior <- suppressWarnings(logspline::logspline(prior))

    d_post <- logspline::dlogspline(hypothesis, f_post)
    d_prior <- logspline::dlogspline(hypothesis, f_prior)

    norm_post <- norm_prior <- 1
    if (direction < 0) {
      norm_post <- logspline::plogspline(hypothesis, f_post)
      norm_prior <- logspline::plogspline(hypothesis, f_prior)
    } else if (direction > 0) {
      norm_post <- 1 - logspline::plogspline(hypothesis, f_post)
      norm_prior <- 1 - logspline::plogspline(hypothesis, f_prior)
    }
  } else {
    insight::print_color("Consider installing the \"logspline\" package for a more robust estimate.\n", "red")
    d_post <- density_at(posterior, hypothesis)
    d_prior <- density_at(prior, hypothesis)

    norm_post <- norm_prior <- 1
    if (direction < 0) {
      norm_post <- mean(posterior < hypothesis)
      norm_prior <- mean(prior < hypothesis)
    } else if (direction > 0) {
      norm_post <- 1 - mean(posterior < 0)
      norm_prior <- 1 - mean(prior < hypothesis)
    }
  }

  (d_prior / norm_prior) / (d_post / norm_post)
}
