#' Convert p-values to (pseudo) Bayes Factors
#'
#' Convert p-values to (pseudo) Bayes Factors. This transformation has been
#' suggested by Aust, Pawel, and Wagenmakers (2026), but is based on a vast
#' amount of assumptions. It might therefore be not reliable. Use at your own
#' risks.
#'
#' @param x A (frequentist) model object, or a (numeric) vector of p-values.
#'   p-values must come from a _two-tailed_ *z*- or *t*-test or from an *F*- or
#'   a \eqn{\chi^2}-test with a single degree of freedom. p-values must not be
#'   corrected for multiple testing.
#' @param n_obs (Effective) number of observations. For mixed models this must
#'   be supplied and generally corresponds to the number of clusters. Either
#'   length 1, or same length as `p`.
#' @param log Whether to return log Bayes Factors. **Note:** The `print()`
#'   method always shows `BF` - the `"log_BF"` column is only accessible from
#'   the returned data frame.
#' @param ... Arguments passed to [parameters::p_value()] if `x` is a model object.
#'
#' @references
#' - Aust, F., Pawel, S., & Wagenmakers, E.J. (2026). Extracting Bayesian
#'   Evidence from Frequentist p-Values. Preprint available on ArXiv:
#'   https://arxiv.org/abs/2607.12132
#'
#' @seealso [bic_to_bf()] for approximate Bayes factors based on BICs.
#'
#' @examplesIf require("parameters")
#'
#' # Compare to BIC-approximated and pseudo BF
#' # --------------------------------------------
#' m0 <- lm(mpg ~ 1, data = mtcars)
#' m1 <- lm(mpg ~ am, data = mtcars)
#' m2 <- lm(mpg ~ factor(cyl), data = mtcars)
#'
#' # BIC-approximated BF, m1 against null model
#' bayesfactor_models(m1, denominator = m0)
#' # bic_to_bf(BIC(m1), denominator = BIC(m0)) # equivalent
#'
#' # pseudo-BF based on p-values
#' p_to_bf(m1)[-1, ] # dropping intercept
#'
#' # When using a p-value from an F/chisq-test with more than one degree of
#' # freedom, the pseudo-BF is not reliable:
#' bayesfactor_models(m2, denominator = m0)
#' p_to_bf(anova(m2), n_obs = nrow(mtcars))
#'
#' @examplesIf require("parameters") && require("lmerTest")
#'
#' # Mixed models
#' # ------------------
#'
#' data("sleepstudy", package = "lme4")
#' mixed0 <- lmerTest::lmer(Reaction ~ 1 + (Days | Subject), data = sleepstudy)
#' mixed1 <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'
#' bayesfactor_models(mixed1, denominator = mixed0)
#'
#' p_to_bf(
#'   mixed1,
#'   n_obs = nlevels(lme4::sleepstudy$Subject), # *effective* sample size here
#'   method = "S" # make sure to get the correct p-values for the fixed effects
#' )[-1, ] # dropping intercept
#'
#' @return A data frame with the p-values and pseudo-Bayes factors (against the null).
#'
#' @seealso [bic_to_bf()] for approximate Bayes factors based on BICs.
#'
#' @export
p_to_bf <- function(x, ...) {
  UseMethod("p_to_bf")
}

#' @export
#' @rdname p_to_bf
p_to_bf.numeric <- function(x, n_obs = NULL, log = FALSE, ...) {
  p <- x
  # Validate n_obs
  if (is.null(n_obs)) {
    insight::format_error("Argument `n_obs` must be specified.")
  } else if (length(n_obs) == 1L) {
    n_obs <- rep(n_obs, times = length(p))
  } else if (length(n_obs) != length(p)) {
    insight::format_error("`n_obs` must be of length 1 or same length as `p`.")
  }

  # Convert
  log_BF <- vector("numeric", length = length(p))
  for (i in seq_along(p)) {
    if (p[i] <= 0.1) {
      log_BF[i] <- log(3) + log(p[i]) + log(sqrt(n_obs[i]))
    } else if (p[i] <= 0.5) {
      log_BF[i] <- log(4 / 3) + log(p[i]) * (2 / 3) + log(sqrt(n_obs[i]))
    } else {
      log_BF[i] <- log(p[i]) / 4 + log(sqrt(n_obs[i]))
    }
  }

  # Clean up
  out <- data.frame(
    p = p,
    # IMPORTANT! This is BF10!
    log_BF = -log_BF,
    stringsAsFactors = FALSE
  )

  if (!log) {
    out$BF <- exp(out$log_BF)
    out$log_BF <- NULL
  }

  class(out) <- c("p_to_pseudo_bf", "data.frame")
  out
}

#' @export
#' @rdname p_to_bf
p_to_bf.default <- function(x, n_obs = NULL, log = FALSE, ...) {
  if (!insight::is_model(x)) {
    insight::format_error(
      "Argument `x` must be a model object, or a numeric vector of p-values."
    )
  }

  is_mixed <- tryCatch(
    inherits(x, "aovlist") || insight::is_mixed_model(x),
    error = function(e) FALSE
  )
  if (is_mixed) {
    if (is.null(n_obs)) {
      insight::format_error(
        "Argument `n_obs` must be specified for mixed models."
      )
    }
    n_ess <- n_obs
  } else {
    # Do this only for non-mixed models (for mixed models we need to use the
    # effective sample size)
    n_ess <- insight::n_obs(x)

    # validation check
    if (is.null(n_ess)) {
      if (is.null(n_obs)) {
        insight::format_error(
          "Unable to determine the number of observations. Please specify `n_obs` for this model."
        )
      }
      # user may also pass n_obs via dots...
      n_ess <- n_obs
    }
  }

  insight::check_if_installed("parameters")
  params <- parameters::p_value(x, ...)
  p <- params$p

  out <- p_to_bf(p, n_obs = n_ess, log = log)
  out <- cbind(params, out[, -1, drop = FALSE])

  class(out) <- c("p_to_pseudo_bf", "data.frame")
  out
}


# methods ---------------

#' @export
print.p_to_pseudo_bf <- function(x, ...) {
  x_orig <- x
  if ("log_BF" %in% colnames(x)) {
    x$BF <- exp(x$log_BF)
    x$log_BF <- NULL
  }
  cat(insight::export_table(
    insight::format_table(x),
    caption = "Pseudo-BF (against NULL)"
  ))
  invisible(x_orig)
}
