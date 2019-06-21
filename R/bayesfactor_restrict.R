#' Savage-Dickey density ratio Bayes Factor (BF)
#'
#' This method computes the
#'
#' @param posterior A \code{stanreg} / \code{brmsfit} object, \code{emmGrid} or a data frame - representing a posterior distribution(s) from (see Details).
#' @param prior An object representing a prior distribution (see Details).
#' @param hypothesis String ...
#' @inheritParams hdi
#'
#' @details
#' Order restriction are conducted by setting an order restriction on the prior and
#' posterior distributions (\cite{Morey & Wagenmakers, 2013}).
#'
#' @return A data frame containing the Bayes factor representing evidence \emph{against} the un-restricted model.
#'
#' @examples
#' library(bayestestR)
#' posterior <- data.frame(X = rnorm(100),
#'                         X1 = rnorm(100),
#'                         X3 = rnorm(100))
#'
#' prior <- data.frame(X = rnorm(100,.4),
#'                     X1 = rnorm(100,-.2),
#'                     X3 = rnorm(100))
#'
#' hyps = c("X > X1 & X1 > X3",
#'          "X > X1")
#'
#' bayesfactor_restrict(posterior,prior,hypothesis = hyps)
#' \dontrun{
#' # rstanarm models
#' # ---------------
#' library(rstanarm)
#' fit_stan <- stan_glm(mpg ~ wt + cyl + am,
#'                      data = mtcars)
#' hyps <- c("am > 0 & cyl < 0","cyl < 0",
#'           "wt - cyl > 0")
#' bayesfactor_restrict(fit_stan, hypothesis = hyps)
#'
#' # emmGrid objects
#' # ---------------
#' library(emmeans)
#' fit <- stan_glm(mpg  ~ factor(cyl), mtcars, family = gaussian())
#' em_cyl <- emmeans(fit, ~ cyl)
#' em_cyl
#' hyps <- c("`4` > 0", # note the ticks!
#'           "`4` > `6`",
#'           "`4` > `6` & `6` > `8`")
#' bayesfactor_restrict(em_cyl, prior = fit, hypothesis = hyps)
#' }
#'
#' @references
#' Morey, R. D., & Wagenmakers, E. J. (2014). Simple relation between Bayesian order-restricted and point-null hypothesis tests. Statistics & Probability Letters, 92, 121-124.
#'
#' @export
bayesfactor_restrict <- function(posterior, prior = NULL, hypothesis, verbose = TRUE, ...) {
  UseMethod("bayesfactor_restrict")
}

#' @importFrom insight get_parameters
#' @rdname bayesfactor_restrict
#' @export
bayesfactor_restrict.stanreg <- function(posterior, prior = NULL,
                                         hypothesis,
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
  bayesfactor_restrict.data.frame(
    posterior = posterior, prior = prior,
    hypothesis = hypothesis
  )
}

#' @rdname bayesfactor_restrict
#' @export
bayesfactor_restrict.brmsfit <- bayesfactor_restrict.stanreg

#' @importFrom stats update
#' @importFrom insight get_parameters
#' @rdname bayesfactor_restrict
#' @export
bayesfactor_restrict.emmGrid <- function(posterior, prior = NULL,
                                         hypothesis,
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
    prior <- stats::update(posterior, post.beta = as.matrix(prior))
  }

  prior <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(prior, names = FALSE)))
  posterior <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(posterior, names = FALSE)))

  bayesfactor_restrict.data.frame(
    posterior = posterior, prior = prior,
    hypothesis = hypothesis
  )
}

#' @export
bayesfactor_restrict.data.frame <- function(posterior, prior = NULL, hypothesis) {
  p_hypothesis <- parse(text = hypothesis)

  if (is.null(prior)) {
    prior <- posterior
    warning(
      "Prior not specified! ",
      "Please specify priors (with column names matching 'posterior')",
      " to get meaningful results."
    )
  }

  .get_prob <- function(x,data){
    mean(eval(x, envir = data))
  }

  posterior_p <- sapply(p_hypothesis, .get_prob, data = posterior)
  prior_p <- sapply(p_hypothesis, .get_prob, data = prior)


  BF <- posterior_p / prior_p
  res <- data.frame(Hypothesis = hypothesis,
                    Prior_prob = prior_p,
                    Posterior_prob = posterior_p,
                    BF = BF)

  class(res) <- unique(c(
    "bayesfactor_restrict",
    class(res)
  ))

  res
}