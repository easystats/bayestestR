#' Bayes Factors (BF) for Order Restricted Models
#'
#' This method computes Bayes factors for comparing a model with an order restrictions on its parameters
#' with the fully unrestricted model. \emph{Note that this method should only be used for confirmatory analyses}.
#' \cr \cr
#' The \code{bf_*} function is an alias of the main function.
#' \cr \cr
#' \strong{For more info, in particular on specifying correct priors for factors with more than 2 levels, see \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.}
#'
#' @param posterior A \code{stanreg} / \code{brmsfit} object, \code{emmGrid} or a data frame - representing a posterior distribution(s) from (see Details).
#' @param hypothesis A character vector specifying the restrictions as logical conditions (see examples below).
#' @param prior An object representing a prior distribution (see Details).
#' @inheritParams hdi
#'
#' @details This method is used to compute Bayes factors for order-restricted models vs un-restricted
#' models by setting an order restriction on the prior and posterior distributions
#' (\cite{Morey & Wagenmakers, 2013}).
#' \cr\cr
#' (Though it is possible to use \code{bayesfactor_restricted()} to test interval restrictions,
#' it is more suitable for testing order restrictions; see examples).
#'
#' @inheritSection bayesfactor_parameters Setting the correct \code{prior}
#'
#' @inheritSection bayesfactor_parameters Interpreting Bayes Factors
#'
#' @return A data frame containing the (log) Bayes factor representing evidence \emph{against} the un-restricted model.
#'
#' @examples
#' library(bayestestR)
#' prior <- data.frame(
#'   X = rnorm(100),
#'   X1 = rnorm(100),
#'   X3 = rnorm(100)
#' )
#'
#' posterior <- data.frame(
#'   X = rnorm(100, .4),
#'   X1 = rnorm(100, -.2),
#'   X3 = rnorm(100)
#' )
#'
#' hyps <- c(
#'   "X > X1 & X1 > X3",
#'   "X > X1"
#' )
#'
#' bayesfactor_restricted(posterior, hypothesis = hyps, prior = prior)
#' \dontrun{
#' # rstanarm models
#' # ---------------
#' if (require("rstanarm") && require("emmeans")) {
#'   fit_stan <- stan_glm(mpg ~ wt + cyl + am,
#'     data = mtcars, refresh = 0
#'   )
#'   hyps <- c(
#'     "am > 0 & cyl < 0",
#'     "cyl < 0",
#'     "wt - cyl > 0"
#'   )
#'   bayesfactor_restricted(fit_stan, hypothesis = hyps)
#'
#'   # emmGrid objects
#'   # ---------------
#'   # replicating http://bayesfactor.blogspot.com/2015/01/multiple-comparisons-with-bayesfactor-2.html
#'   disgust_data <- read.table(url("http://www.learnbayes.org/disgust_example.txt"), header = TRUE)
#'
#'   contrasts(disgust_data$condition) <- contr.orthonorm # see vignette
#'   fit_model <- stan_glm(score ~ condition, data = disgust_data, family = gaussian())
#'
#'   em_condition <- emmeans(fit_model, ~condition)
#'   hyps <- c("lemon < control & control < sulfur")
#'
#'   bayesfactor_restricted(em_condition, prior = fit_model, hypothesis = hyps)
#'   # > # Bayes Factor (Order-Restriction)
#'   # >
#'   # >                          Hypothesis P(Prior) P(Posterior)   BF
#'   # >  lemon < control & control < sulfur     0.17         0.75 4.49
#'   # > ---
#'   # > Bayes factors for the restricted model vs. the un-restricted model.
#' }
#' }
#' @references
#' \itemize{
#' \item Morey, R. D., & Wagenmakers, E. J. (2014). Simple relation between Bayesian order-restricted and point-null hypothesis tests. Statistics & Probability Letters, 92, 121-124.
#' \item Morey, R. D., & Rouder, J. N. (2011). Bayes factor approaches for testing interval null hypotheses. Psychological methods, 16(4), 406.
#' \item Morey, R. D. (Jan, 2015). Multiple Comparisons with BayesFactor, Part 2 – order restrictions. Retrived from https://richarddmorey.org/category/order-restrictions/.
#' }
#'
#' @export
bayesfactor_restricted <- function(posterior, hypothesis, prior = NULL, verbose = TRUE, ...) {
  UseMethod("bayesfactor_restricted")
}

#' @rdname bayesfactor_restricted
#' @export
bf_restricted <- bayesfactor_restricted

#' @rdname bayesfactor_restricted
#' @export
bayesfactor_restricted.stanreg <- function(posterior, hypothesis, prior = NULL,
                                           verbose = TRUE,
                                           effects = c("fixed", "random", "all"),
                                           component = c("conditional", "zi", "zero_inflated", "all"),
                                           ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  samps <- .clean_priors_and_posteriors(posterior, prior,
    effects, component,
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

#' @rdname bayesfactor_restricted
#' @export
bayesfactor_restricted.blavaan <- function(posterior, hypothesis, prior = NULL,
                                           verbose = TRUE, ...) {
  samps <- .clean_priors_and_posteriors(posterior, prior,
                                        verbose = verbose)

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
bayesfactor_restricted.data.frame <- function(posterior, hypothesis, prior = NULL, ...) {
  p_hypothesis <- parse(text = hypothesis)

  if (is.null(prior)) {
    prior <- posterior
    warning(
      "Prior not specified! ",
      "Please specify priors (with column names matching 'posterior')",
      " to get meaningful results."
    )
  }

  .get_prob <- function(x, data) {
    x_logical <- try(eval(x, envir = data), silent = TRUE)
    if (inherits(x_logical, "try-error")) {
      cnames <- colnames(data)
      is_name <- make.names(cnames) == cnames
      cnames[!is_name] <- paste0("`", cnames[!is_name], "`")
      cnames <- paste0(cnames, collapse = ", ")
      stop(x_logical, "Available parameters are: ", cnames)
    } else if (!all(is.logical(x_logical))) {
      stop("Hypotheses must be logical")
    }
    mean(x_logical)
  }

  posterior_p <- sapply(p_hypothesis, .get_prob, data = posterior)
  prior_p <- sapply(p_hypothesis, .get_prob, data = prior)


  BF <- posterior_p / prior_p
  res <- data.frame(
    Hypothesis = hypothesis,
    p_prior = prior_p,
    p_posterior = posterior_p,
    log_BF = log(BF)
  )

  class(res) <- unique(c(
    "bayesfactor_restricted",
    class(res)
  ))

  res
}
