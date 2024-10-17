#' Convert BIC indices to Bayes Factors via the BIC-approximation method.
#'
#' The difference between two Bayesian information criterion (BIC) indices of
#' two models can be used to approximate Bayes factors via:
#' \cr
#' \deqn{BF_{10} = e^{(BIC_0 - BIC_1)/2}}{BF10 = exp((BIC0-BIC1)/2)}
#'
#' @param bic A vector of BIC values.
#' @param denominator The BIC value to use as a denominator (to test against).
#' @param log If `TRUE`, return the `log(BF)`.
#'
#' @references
#' Wagenmakers, E. J. (2007). A practical solution to the pervasive problems of
#' p values. Psychonomic bulletin & review, 14(5), 779-804
#'
#' @examples
#' bic1 <- BIC(lm(Sepal.Length ~ 1, data = iris))
#' bic2 <- BIC(lm(Sepal.Length ~ Species, data = iris))
#' bic3 <- BIC(lm(Sepal.Length ~ Species + Petal.Length, data = iris))
#' bic4 <- BIC(lm(Sepal.Length ~ Species * Petal.Length, data = iris))
#'
#' bic_to_bf(c(bic1, bic2, bic3, bic4), denominator = bic1)
#' @return The Bayes Factors corresponding to the BIC values against the denominator.
#'
#' @export
bic_to_bf <- function(bic, denominator, log = FALSE) {
  delta <- (denominator - bic) / 2

  if (log) {
    delta
  } else {
    exp(delta)
  }
}
