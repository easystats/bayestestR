#' Bayes Factors (BF) for Order Restricted Models
#'
#' This method computes Bayes factors for comparing a model with an order restrictions on its parameters
#' with the fully unrestricted model. *Note that this method should only be used for confirmatory analyses*.
#' \cr \cr
#' The `bf_*` function is an alias of the main function.
#' \cr \cr
#' \strong{For more info, in particular on specifying correct priors for factors with more than 2 levels, see [the Bayes factors vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).}
#'
#' @param posterior A `stanreg` / `brmsfit` object, `emmGrid` or a data frame - representing a posterior distribution(s) from (see Details).
#' @param hypothesis A character vector specifying the restrictions as logical conditions (see examples below).
#' @param prior An object representing a prior distribution (see Details).
#' @inheritParams hdi
#'
#' @details This method is used to compute Bayes factors for order-restricted models vs un-restricted
#' models by setting an order restriction on the prior and posterior distributions
#' (\cite{Morey & Wagenmakers, 2013}).
#' \cr\cr
#' (Though it is possible to use `bayesfactor_restricted()` to test interval restrictions,
#' it is more suitable for testing order restrictions; see examples).
#'
#' @inheritSection bayesfactor_parameters Setting the correct `prior`
#'
#' @inheritSection bayesfactor_parameters Interpreting Bayes Factors
#'
#' @return A data frame containing the (log) Bayes factor representing evidence
#'   *against* the un-restricted model (Use `as.numeric()` to extract the
#'   non-log Bayes factors; see examples). (A `bool_results` attribute contains
#'   the results for each sample, indicating if they are included or not in the
#'   hypothesized restriction.)
#'
#' @examples
#' set.seed(444)
#' library(bayestestR)
#' prior <- data.frame(
#'   A = rnorm(1000),
#'   B = rnorm(1000),
#'   C = rnorm(1000)
#' )
#'
#' posterior <- data.frame(
#'   A = rnorm(1000, .4, 0.7),
#'   B = rnorm(1000, -.2, 0.4),
#'   C = rnorm(1000, 0, 0.5)
#' )
#'
#' hyps <- c(
#'   "A > B & B > C",
#'   "A > B & A > C",
#'   "C > A"
#' )
#'
#' if (getRversion() > "3.5.0") {
#'   (b <- bayesfactor_restricted(posterior, hypothesis = hyps, prior = prior))
#'
#'   as.numeric(b)
#'
#'   if (require("see") && require("patchwork")) {
#'     i <- attr(b, "bool_results")[["posterior"]]
#'
#'     see::plots(
#'       plot(estimate_density(posterior)),
#'       # distribution **conditional** on the restrictions
#'       plot(estimate_density(posterior[i[[hyps[1]]], ])) + ggplot2::ggtitle(hyps[1]),
#'       plot(estimate_density(posterior[i[[hyps[2]]], ])) + ggplot2::ggtitle(hyps[2]),
#'       plot(estimate_density(posterior[i[[hyps[3]]], ])) + ggplot2::ggtitle(hyps[3]),
#'       guides = "collect"
#'     )
#'   }
#' }
#'
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
#' - Morey, R. D., & Wagenmakers, E. J. (2014). Simple relation between Bayesian order-restricted and point-null hypothesis tests. Statistics & Probability Letters, 92, 121-124.
#' - Morey, R. D., & Rouder, J. N. (2011). Bayes factor approaches for testing interval null hypotheses. Psychological methods, 16(4), 406.
#' - Morey, R. D. (Jan, 2015). Multiple Comparisons with BayesFactor, Part 2 – order restrictions. Retrieved from https://richarddmorey.org/category/order-restrictions/.
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

  .test_hypothesis <- function(x, data) {
    x_logical <- try(eval(x, envir = data), silent = TRUE)
    if (inherits(x_logical, "try-error")) {
      cnames <- colnames(data)
      is_name <- make.names(cnames) == cnames
      cnames[!is_name] <- paste0("`", cnames[!is_name], "`")
      cnames <- paste0(cnames, collapse = ", ")
      stop(x_logical, "Available parameters are: ", cnames, call. = FALSE)
    } else if (!all(is.logical(x_logical))) {
      stop("Hypotheses must be logical", call. = FALSE)
    }
    x_logical
  }



  posterior_l <- as.data.frame(lapply(p_hypothesis, .test_hypothesis, data = posterior))
  prior_l <- as.data.frame(lapply(p_hypothesis, .test_hypothesis, data = prior))
  colnames(posterior_l) <- colnames(prior_l) <- if (!is.null(names(hypothesis))) names(hypothesis) else hypothesis

  posterior_p <- sapply(posterior_l, mean)
  prior_p <- sapply(prior_l, mean)
  BF <- posterior_p / prior_p

  res <- data.frame(
    Hypothesis = hypothesis,
    p_prior = prior_p,
    p_posterior = posterior_p,
    log_BF = log(BF)
  )

  attr(res, "bool_results") <- list(posterior = posterior_l, prior = prior_l)
  class(res) <- unique(c(
    "bayesfactor_restricted",
    class(res)
  ))

  res
}


#' @export
bayesfactor_restricted.draws <- function(posterior, hypothesis, prior = NULL, ...) {
  bayesfactor_restricted(.posterior_draws_to_df(posterior), hypothesis = hypothesis, prior = prior, ...)
}

#' @export
bayesfactor_restricted.rvar <- bayesfactor_restricted.draws
