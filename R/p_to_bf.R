#' Convert p-values to Bayes Factors
#'
#' Convert p-values to Bayes Factors
#'
#' @param x A (frequentist) model object, or a (numeric) vector of p-values.
#' @param n_obs Number of observations. When `x` is a model object, this is
#'   automatically extracted from the model via `insight::n_obs()`.
#' @param log If `TRUE`, return the `log(BF)`.
#'
#' @references
#' Wagenmakers, E.-J. (2022). Approximate objective Bayes factors from p-values and sample size: The 3p(sqrt(n)) rule. Preprint available on ArXiv: https://psyarxiv.com/egydq
#'
#' @examples
#' if (requireNamespace("parameters", quietly = TRUE)) {
#'   data(iris)
#'   model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
#'   p_to_bf(model)
#' }
#' @return The Bayes Factors corresponding to the p-values.
#'
#' @export
p_to_bf <- function(x, n_obs = NULL, log = FALSE) {
  if (insight::is_model(x)) {
    insight::check_if_installed("parameters")
    params <- parameters::p_value(x)
    p <- params$p
    n_obs <- insight::n_obs(x)
  } else if (all(is.numeric(x))) {
    if (is.null(n_obs)) {
      stop("Argument 'n_obs' must be specified.", call. = FALSE)
    }
    p <- x
    params <- NULL
  } else {
    stop("Argument must be a model object, or a numeric vector of p-values.", call. = FALSE)
  }

  bf <- vector("numeric")

  for (i in p) {
    if (i <= 0.1) {
      bf <- c(bf, 3 * i * sqrt(n_obs))
    } else if (i <= 0.5) {
      bf <- c(bf, (4 / 3) * i^(2/3) * sqrt(n_obs))
    } else {
      bf <- c(bf, i^.25 * sqrt(n_obs))
    }
  }

  if (log) {
    bf <- log(bf)
  }

  if (!is.null(params)) {
    params$BF <- bf
  } else {
    params <- bf
  }

  params
}
