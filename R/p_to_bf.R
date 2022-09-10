#' Convert p-values to Bayes Factors
#'
#' Convert p-values to pseudo-Bayes Factors. For more accurate approximate Bayes factors, use [bic_to_bf()] instead. 
#'
#' @param x A (frequentist) model object, or a (numeric) vector of p-values.
#' @param n_obs Number of observations. Either length 1, or same length as `p`.
#'
#' @references
#' - Wagenmakers, E.J. (2022). Approximate objective Bayes factors from p-values
#' and sample size: The 3p(sqrt(n)) rule. Preprint available on ArXiv:
#' https://psyarxiv.com/egydq
#'
#' @examples
#' if (requireNamespace("parameters", quietly = TRUE)) {
#'   data(iris)
#'   model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
#'   p_to_bf(model)
#' }
#'
#' @return A data frame with the p-values and Bayes factors (against the null).
#'
#' @seealso [bic_to_bf()] for more accurate approximate Bayes factors.
#'
#' @export
p_to_bf <- function(x, log = FALSE, ...) {
  UseMethod("p_to_bf")
}

#' @export
#' @rdname p_to_bf
p_to_bf.numeric <- function(x, log = FALSE, n_obs = NULL, ...) {
  p <- x
  # Validate n_obs
  if (is.null(n_obs)) {
    stop("Argument 'n_obs' must be specified.", call. = FALSE)
  } else if (length(n_obs) == 1L) {
    n_obs <- rep(n_obs, times = length(p))
  } else if (length(n_obs) != length(p)) {
    stop("'n_obs' must be of length 1 or same length as 'p'.")
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
    log_BF = -log_BF
  )

  if (!log) {
    out$BF <- exp(out$log_BF)
    out$log_BF <- NULL
  }

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
  } else {
    stop("Argument must be a model object, or a numeric vector of p-values.", call. = FALSE)
  }

  out <- p_to_bf(params$p, n_obs = n_obs)
  cbind(params, out[,-1, drop = FALSE])
}
