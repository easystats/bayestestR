#' Convert p-values to (pseudo) Bayes Factors
#'
#' Convert p-values to (pseudo) Bayes Factors. This transformation has been
#' suggested by Wagenmakers (2022), but is based on a vast amount of assumptions.
#' It might therefore be not reliable. Use at your own risks. For more accurate
#' approximate Bayes factors, use [bic_to_bf()] instead.
#'
#' @param x A (frequentist) model object, or a (numeric) vector of p-values.
#' @param n_obs Number of observations. Either length 1, or same length as `p`.
#' @param log Wether to return log Bayes Factors. **Note:** The `print()` method
#' always shows `BF` - the `"log_BF"` column is only accessible from the returned
#' data frame.
#' @param ... Other arguments to be passed (not used for now).
#'
#' @references
#' - Wagenmakers, E.J. (2022). Approximate objective Bayes factors from p-values
#' and sample size: The 3p(sqrt(n)) rule. Preprint available on ArXiv:
#' https://psyarxiv.com/egydq
#'
#' @examplesIf require("parameters")
#' data(iris)
#' model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
#' p_to_bf(model)
#'
#' # Examples that demonstrate comparison between
#' # BIC-approximated and pseudo BF
#' # --------------------------------------------
#' m0 <- lm(mpg ~ 1, mtcars)
#' m1 <- lm(mpg ~ am, mtcars)
#' m2 <- lm(mpg ~ factor(cyl), mtcars)
#'
#' # In this first example, BIC-approximated BF and
#' # pseudo-BF based on p-values are close...
#'
#' # BIC-approximated BF, m1 against null model
#' bic_to_bf(BIC(m1), denominator = BIC(m0))
#'
#' # pseudo-BF based on p-values - dropping intercept
#' p_to_bf(m1)[-1, ]
#'
#' # The second example shows that results from pseudo-BF are less accurate
#' # and should be handled wit caution!
#' bic_to_bf(BIC(m2), denominator = BIC(m0))
#' p_to_bf(anova(m2), n_obs = nrow(mtcars))
#'
#' @return A data frame with the p-values and pseudo-Bayes factors (against the null).
#'
#' @seealso [bic_to_bf()] for more accurate approximate Bayes factors.
#'
#' @export
p_to_bf <- function(x, ...) {
  UseMethod("p_to_bf")
}

#' @export
#' @rdname p_to_bf
p_to_bf.numeric <- function(x, log = FALSE, n_obs = NULL, ...) {
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
      log_BF[i] <- log(3 * p[i] * sqrt(n_obs[i]))
    } else if (p[i] <= 0.5) {
      # log_BF[i] <- log((4 / 3) * p[i] ^ (2 / 3) * sqrt(n_obs[i]))
      log_BF[i] <- log(p[i]) * (2 / 3) + log(sqrt(n_obs[i]) * (4 / 3))
    } else {
      # log_BF[i] <- p[i] ^ .25 * sqrt(n_obs[i])
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
p_to_bf.default <- function(x, log = FALSE, ...) {
  if (insight::is_model(x)) {
    insight::check_if_installed("parameters")
    params <- parameters::p_value(x)
    p <- params$p
    n_obs <- insight::n_obs(x)
    # validation check
    if (is.null(n_obs)) {
      # user may also pass n_obs via dots...
      n_obs <- list(...)$n_obs
    }
  } else {
    insight::format_error("Argument `x` must be a model object, or a numeric vector of p-values.")
  }

  out <- p_to_bf(p, n_obs = n_obs, log = log)
  out <- cbind(params, out[, -1, drop = FALSE])

  class(out) <- c("p_to_pseudo_bf", "data.frame")
  out
}


# methods ---------------

#' @export
print.p_to_pseudo_bf <- function(x, ...) {
  cat(insight::export_table(insight::format_table(x), caption = "Pseudo-BF (against NULL)"))
}
