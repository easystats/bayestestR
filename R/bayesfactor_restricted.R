#' Bayes Factors (BF) for Order Restricted Models
#'
#' This method computes Bayes factors for comparing a model with an order restrictions on its parameters
#' with the fully unrestricted model. \emph{Note that this method should only be used for confirmatory analyses}.
#' \cr \cr
#' \strong{For info on specifying correct priors for factors with more than 2 levels, see \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.}
#' \cr \cr
#' For more info, see \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.
#'
#' @param posterior A \code{stanreg} / \code{brmsfit} object, \code{emmGrid} or a data frame - representing a posterior distribution(s) from (see Details).
#' @param hypothesis A character vector specifying the restrictions as logical conditions (see examples below).
#' @param prior An object representing a prior distribution (see Details).
#' @inheritParams hdi
#'
#' @details This method is used to compute Bayes factors for order-restricted models vs un-restricted
#' models by setting an order restriction on the prior and posterior distributions
#' (\cite{Morey & Wagenmakers, 2013}).
#'
#' (Though it is possible to use \code{bayesfactor_restricted} to test interval restrictions,
#' it is more suitable for testing order restrictions (see examples)).
#'
#' When \code{posterior} is a model (\code{stanreg}, \code{brmsfit}), posterior and prior samples are
#' extracted for each parameter, and Savage-Dickey Bayes factors are computed for each parameter.
#'
#' \strong{NOTE:} For \code{brmsfit} models, the model must have been fitted with \emph{custom (non-default)} priors. See example below.
#'
#' \subsection{Setting the correct \code{prior}}{
#' It is important to provide the correct \code{prior} for meaningful results.
#' \itemize{
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
#' \subsection{Interpreting Bayes Factors}{
#' A Bayes factor greater than 1 can be interpereted as evidence against the null,
#' at which one convention is that a Bayes factor greater than 3 can be considered
#' as "substantial" evidence against the null (and vice versa, a Bayes factor
#' smaller than 1/3 indicates substantial evidence in favor of the null-hypothesis)
#' (\cite{Wetzels et al. 2011}).
#' }
#'
#' @return A data frame containing the Bayes factor representing evidence \emph{against} the un-restricted model.
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
#' library(rstanarm)
#' fit_stan <- stan_glm(mpg ~ wt + cyl + am,
#'   data = mtcars
#' )
#' hyps <- c(
#'   "am > 0 & cyl < 0",
#'   "cyl < 0",
#'   "wt - cyl > 0"
#' )
#' bayesfactor_restricted(fit_stan, hypothesis = hyps)
#'
#' # emmGrid objects
#' # ---------------
#' library(emmeans)
#'
#' # replicating http://bayesfactor.blogspot.com/2015/01/multiple-comparisons-with-bayesfactor-2.html
#' disgust_data <- read.table(url("http://www.learnbayes.org/disgust_example.txt"), header = TRUE)
#'
#' contrasts(disgust_data$condition) <- contr.bayesian # see vignette
#' fit_model <- stan_glm(score ~ condition, data = disgust_data, family = gaussian())
#'
#' em_condition <- emmeans(fit_model, ~condition)
#' hyps <- c("lemon < control & control < sulfur")
#'
#' bayesfactor_restricted(em_condition, prior = fit_model, hypothesis = hyps)
#' # > # Bayes Factor (Order-Restriction)
#' # >
#' # >                          Hypothesis P(Prior) P(Posterior) Bayes Factor
#' # >  lemon < control & control < sulfur     0.17         0.75         4.49
#' # > ---
#' # > Bayes factors for the restricted movel vs. the un-restricted model.
#' }
#'
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

#' @importFrom insight get_parameters
#' @rdname bayesfactor_restricted
#' @export
bayesfactor_restricted.stanreg <- function(posterior, hypothesis, prior = NULL,
                                           verbose = TRUE,
                                           effects = c("fixed", "random", "all"),
                                           ...) {
  effects <- match.arg(effects)

  # Get Priors
  if (is.null(prior)) {
    prior <- posterior
  }

  prior <- .update_to_priors(prior, verbose = verbose)
  prior <- insight::get_parameters(prior, effects = effects)
  posterior <- insight::get_parameters(posterior, effects = effects)

  # Get savage-dickey BFs
  bayesfactor_restricted.data.frame(
    posterior = posterior, prior = prior,
    hypothesis = hypothesis
  )
}

#' @rdname bayesfactor_restricted
#' @export
bayesfactor_restricted.brmsfit <- bayesfactor_restricted.stanreg

#' @importFrom stats update
#' @importFrom insight get_parameters
#' @rdname bayesfactor_restricted
#' @export
bayesfactor_restricted.emmGrid <- function(posterior, hypothesis, prior = NULL,
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
  } else if (!inherits(prior, "emmGrid")) { # then is it a model
    prior <- .update_to_priors(prior, verbose = verbose)
    prior <- insight::get_parameters(prior, effects = "fixed")
    prior <- stats::update(posterior, post.beta = as.matrix(prior))
  }

  prior <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(prior, names = FALSE)))
  posterior <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(posterior, names = FALSE)))

  bayesfactor_restricted.data.frame(
    posterior = posterior, prior = prior,
    hypothesis = hypothesis
  )
}

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
    Prior_prob = prior_p,
    Posterior_prob = posterior_p,
    BF = BF
  )

  class(res) <- unique(c(
    "bayesfactor_restricted",
    class(res)
  ))

  res
}
